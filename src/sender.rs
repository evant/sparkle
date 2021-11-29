use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::Write;
use std::mem;

use std::os::raw::c_char;
use std::path::Path;
use std::str::FromStr;

use cranelift::codegen::binemit::{NullStackMapSink, NullTrapSink};
use cranelift::codegen::Context;
use cranelift::prelude::settings::{self, Configurable};
use cranelift::prelude::*;
use cranelift::prelude::{isa, AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use nom::lib::std::collections::HashMap;
use sparkle::SYMBOLS;
use target_lexicon::{OperatingSystem, Triple};

use crate::error::ReportError;
use crate::pst;
use crate::pst::{
    Arg, BinOperator, Call, Declaration, DeclareVar, Expr, Index, LValue, Literal, Paragraph,
    Report, Statement,
};
use crate::types::ArrayType;
use crate::types::Type::{Array, Boolean, Chars, Number};
use crate::vars::{Callable, Callables};

type ReportResult<T> = Result<T, ReportError>;

pub fn send_out<'a>(report: &'a Report, name: &str, target: &str) -> ReportResult<String> {
    let mut sender = object_sender(name, target)?;
    let mut globals = Callables::new();
    send(report, &mut sender, &mut globals, |_, _| Ok(()))?;
    let is_windows = sender.is_windows();
    let product = sender.module.finish();
    let ext = if is_windows { ".obj" } else { ".o" };
    let path_name = name.to_owned() + ext;
    let path = Path::new(&path_name);
    let mut file = File::create(path).expect("error opening file");
    file.write_all(&product.emit().unwrap())
        .expect("error writing to file");

    Ok(path_name)
}

pub fn gallop_mane(report: &Report, target: &str) -> ReportResult<()> {
    let mut sender = simple_jit_sender(target)?;
    let mut globals = Callables::new();
    let id = send(report, &mut sender, &mut globals, |_, _| Ok(()))?;
    sender.module.finalize_definitions();
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> isize>(code) };

    code();

    Ok(())
}

pub unsafe trait IntoSparkleType: Sized {
    type T;

    fn into(self) -> Self::T;
}

pub unsafe trait FromSparkleType: Sized {
    type T;

    fn from<T>(other: Self::T) -> Self;
}

unsafe impl IntoSparkleType for f64 {
    type T = f64;

    fn into(self) -> Self::T {
        self
    }
}

unsafe impl FromSparkleType for f64 {
    type T = f64;

    fn from<T>(other: Self::T) -> Self {
        other
    }
}

unsafe impl IntoSparkleType for bool {
    type T = i32;

    fn into(self) -> Self::T {
        if self {
            1
        } else {
            0
        }
    }
}

unsafe impl FromSparkleType for bool {
    type T = i32;

    fn from<T>(other: Self::T) -> Self {
        other != 0
    }
}

unsafe impl IntoSparkleType for &str {
    type T = *const c_char;

    fn into(self) -> Self::T {
        CString::new(self).unwrap().into_raw()
    }
}

unsafe impl FromSparkleType for String {
    type T = *const c_char;

    fn from<T>(other: Self::T) -> Self {
        unsafe { CStr::from_ptr(other).to_str().unwrap().to_owned() }
    }
}

pub fn proofread<'a>(report: &'a Report<'a>, target: &str) -> ReportResult<()> {
    let mut sender = simple_jit_sender(target)?;
    let mut globals = Callables::new();

    let mut buff = String::new();
    send(&report, &mut sender, &mut globals, |_sender, context| {
        cranelift::codegen::write_function(&mut buff, &context.func)?;
        Ok(())
    })?;

    print!("{}", buff);

    Ok(())
}

fn object_sender(name: &str, target: &str) -> ReportResult<Sender<ObjectModule>> {
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();
    let triple = Triple::from_str(target)?;
    let isa_builder = isa::lookup(triple.clone())?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let builder = ObjectBuilder::new(isa, name.to_owned(), default_libcall_names()).unwrap();

    let module = ObjectModule::new(builder);

    Ok(Sender::new(module, triple)?)
}

fn simple_jit_sender(target: &str) -> ReportResult<Sender<JITModule>> {
    let triple = Triple::from_str(target)?;
    let mut builder = JITBuilder::new(default_libcall_names());
    builder.symbols(SYMBOLS.iter().map(|e| *e));
    let module = JITModule::new(builder);

    Ok(Sender::new(module, triple)?)
}

fn send<'a, M: Module>(
    report: &'a Report,
    sender: &'a mut Sender<M>,
    globals: &'a mut Callables<'a>,
    mut f: impl FnMut(&Sender<M>, &Context) -> ReportResult<()>,
) -> ReportResult<FuncId> {
    let mut mane_paragraphs: Vec<FuncId> = vec![];
    let mut declared_paragraphs: Vec<(FuncId, &Paragraph)> = vec![];

    let sig = sender.module.make_signature();
    let init_globals = sender
        .module
        .declare_function("init_globals", Linkage::Local, &sig)?;

    let mut context = sender.module.make_context();
    let mut function_builder_context = FunctionBuilderContext::new();

    let mut function_sender = FunctionSender::new(&mut context, &mut function_builder_context);
    let entry_block = function_sender.builder.create_block();
    function_sender.builder.switch_to_block(entry_block);
    function_sender.seal_block(entry_block);

    for declaration in &report.declarations {
        match declaration {
            Declaration::Paragraph(paragraph) => {
                let id = declare_paragraph(paragraph, sender)?;
                let arg_types = paragraph
                    .decl
                    .args
                    .iter()
                    .map(|Arg(type_, _)| *type_)
                    .collect();
                globals.insert(
                    paragraph.decl.name,
                    Callable::Func(paragraph.decl.return_type, arg_types, id),
                );
                declared_paragraphs.push((id, paragraph));
                if paragraph.mane {
                    mane_paragraphs.push(id);
                }
            }
            Declaration::Var(declare_var) => {
                send_declare_global(declare_var, sender, globals, &mut function_sender)?;
            }
        }
    }

    function_sender.builder.ins().return_(&[]);
    function_sender.builder.finalize();

    sender.module.define_function(
        init_globals,
        &mut context,
        &mut NullTrapSink::default(),
        &mut NullStackMapSink {},
    )?;
    f(sender, &mut context)?;
    sender.module.clear_context(&mut context);

    for (id, paragraph) in declared_paragraphs {
        send_paragraph(
            paragraph,
            id,
            sender,
            globals,
            &mut context,
            &mut function_builder_context,
            &mut f,
        )?;
    }

    let id = send_mane(
        &mane_paragraphs,
        init_globals,
        sender,
        &mut context,
        &mut function_builder_context,
        &mut f,
    )?;

    sender.finalize(&mut context);

    Ok(id)
}

fn send_declare_global<'a, M: Module>(
    declare_var: &DeclareVar<'a>,
    sender: &mut Sender<M>,
    globals: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let DeclareVar(pst::Variable(name), type_, expr, is_const) = declare_var;
    let (type_, _value) =
        send_init_variable(*type_, expr, *is_const, sender, globals, function_sender)?;

    sender
        .data_context
        .define_zeroinit(ir_type(sender.pointer_type, type_).bytes() as usize);
    let id = sender
        .module
        .declare_data(name, Linkage::Local, !*is_const, false)?;
    sender.module.define_data(id, &sender.data_context)?;
    sender.data_context.clear();

    globals.insert(name, Callable::Global(type_, id, *is_const));

    Ok(())
}

fn send_mane<M: Module>(
    mane_paragraphs: &[FuncId],
    init_globals: FuncId,
    sender: &mut Sender<M>,
    context: &mut Context,
    function_builder_context: &mut FunctionBuilderContext,
    f: &mut impl FnMut(&Sender<M>, &Context) -> ReportResult<()>,
) -> ReportResult<FuncId> {
    let int = sender.pointer_type;
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(sender.pointer_type));

    let mut function_sender = FunctionSender::new(context, function_builder_context);

    let entry_block = function_sender.builder.create_block();
    function_sender
        .builder
        .append_block_params_for_function_params(entry_block);
    function_sender.builder.switch_to_block(entry_block);
    function_sender.seal_block(entry_block);

    let local_callee = sender
        .module
        .declare_func_in_func(init_globals, function_sender.builder.func);
    function_sender.builder.ins().call(local_callee, &[]);

    for func_id in mane_paragraphs {
        let local_callee = sender
            .module
            .declare_func_in_func(*func_id, function_sender.builder.func);
        function_sender.builder.ins().call(local_callee, &[]);
    }

    let zero = function_sender.builder.ins().iconst(int, 0);
    let var = cranelift::prelude::Variable::new(0);
    function_sender.builder.declare_var(var, int);
    function_sender.builder.def_var(var, zero);
    let return_value = function_sender.builder.use_var(var);
    function_sender.builder.ins().return_(&[return_value]);

    let id = sender.module.declare_function(
        "main",
        Linkage::Export,
        &function_sender.builder.func.signature,
    )?;
    function_sender.builder.finalize();

    sender.module.define_function(
        id,
        context,
        &mut NullTrapSink::default(),
        &mut NullStackMapSink {},
    )?;
    f(sender, context)?;
    sender.module.clear_context(context);

    Ok(id)
}

fn declare_paragraph<'a, M: Module>(
    paragraph: &'a Paragraph,
    sender: &mut Sender<M>,
) -> ReportResult<FuncId> {
    let mut sig = sender.module.make_signature();
    for Arg(type_, _) in &paragraph.decl.args {
        sig.params
            .push(AbiParam::new(ir_type(sender.pointer_type, *type_)));
    }
    if let Some(type_) = paragraph.decl.return_type {
        sig.returns
            .push(AbiParam::new(ir_type(sender.pointer_type, type_)));
    }
    let id = sender
        .module
        .declare_function(paragraph.decl.name, Linkage::Local, &sig)?;

    Ok(id)
}

fn send_paragraph<'a, M: Module>(
    paragraph: &'a Paragraph,
    func_id: FuncId,
    sender: &mut Sender<M>,
    globals: &'a Callables<'a>,
    context: &mut Context,
    function_builder_context: &mut FunctionBuilderContext,
    f: &mut impl FnMut(&Sender<M>, &Context) -> ReportResult<()>,
) -> ReportResult<()> {
    let mut vars = globals.create_child();
    let mut function_sender = FunctionSender::new(context, function_builder_context);

    let entry_block = function_sender.builder.create_block();

    for Arg(arg_type, name) in &paragraph.decl.args {
        let value_type = ir_type(sender.pointer_type, *arg_type);
        function_sender
            .builder
            .func
            .signature
            .params
            .push(AbiParam::new(value_type));
        let arg_value = function_sender
            .builder
            .append_block_param(entry_block, value_type);
        vars.insert(*name, Callable::Arg(*arg_type, arg_value));
    }
    if let Some(return_type) = paragraph.decl.return_type {
        function_sender
            .builder
            .func
            .signature
            .returns
            .push(AbiParam::new(ir_type(sender.pointer_type, return_type)));
    }

    function_sender.builder.switch_to_block(entry_block);
    function_sender.seal_block(entry_block);

    for statement in paragraph.statements.iter() {
        send_statement(
            statement,
            paragraph.decl.return_type,
            sender,
            &mut vars,
            &mut function_sender,
        )?;
    }

    if paragraph.decl.return_type.is_none() {
        function_sender.builder.ins().return_(&[]);
    }
    function_sender.builder.finalize();
    sender.module.define_function(
        func_id,
        context,
        &mut NullTrapSink::default(),
        &mut NullStackMapSink {},
    )?;
    f(sender, context)?;
    sender.module.clear_context(context);

    Ok(())
}

fn send_statement<'a, M: Module>(
    statement: &'a Statement<'a>,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    match statement {
        Statement::Print(expr) => send_print_statement(expr, true, sender, vars, function_sender),
        Statement::Read(var, type_, prompt) => {
            send_read_statement(var, *type_, prompt, sender, vars, function_sender)
        }
        Statement::Declare(DeclareVar(var, type_, expr, is_const)) => {
            send_declare_statement(var, *type_, expr, *is_const, sender, vars, function_sender)
        }
        Statement::Assign(var, expr) => {
            send_assign_statement(var, expr, sender, vars, function_sender)
        }
        Statement::Increment(var) => send_increment_statement(var, vars, sender, function_sender),
        Statement::Decrement(var) => send_decrement_statement(var, vars, sender, function_sender),
        Statement::If(cond, if_, else_) => send_if_else(
            cond,
            if_,
            else_,
            return_type,
            sender,
            &mut vars.create_child(),
            function_sender,
        ),
        Statement::While(cond, body) => send_while_statement(
            cond,
            body,
            return_type,
            sender,
            &mut vars.create_child(),
            function_sender,
        ),
        Statement::DoWhile(cond, body) => send_do_while_statement(
            cond,
            body,
            return_type,
            sender,
            &mut vars.create_child(),
            function_sender,
        ),
        Statement::For(type_, var, from, to, body) => send_for_statement(
            *type_,
            var,
            from,
            to,
            body,
            return_type,
            sender,
            &mut vars.create_child(),
            function_sender,
        ),
        Statement::Call(call) => {
            send_call(call, sender, vars, function_sender)?;
            Ok(())
        }
        Statement::Return(expr) => send_return(expr, return_type, sender, vars, function_sender),
    }
}

fn send_print_statement<'a, M: Module>(
    expr: &Expr<'a>,
    newline: bool,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let (type_, value) = send_expression(expr, sender, vars, function_sender)?;

    let (fun, arg_type) = match type_ {
        Chars => ("chars", sender.pointer_type),
        Number => ("num", types::F64),
        Boolean => ("bool", types::I32),
        Array(array_type) => (
            match array_type {
                ArrayType::Chars => "array_chars",
                ArrayType::Number => "array_num",
                ArrayType::Boolean => "array_bool",
            },
            sender.pointer_type,
        ),
    };

    let prefix = if newline { "println_" } else { "print_" }.to_string();

    sender.call_external_fn(
        &mut function_sender.builder,
        &(prefix + fun),
        &[arg_type],
        &[value],
    )?;

    Ok(())
}

// helper for debugging issues
fn printf<M: Module>(
    format: &str,
    value: Value,
    sender: &mut Sender<M>,
    builder: &mut FunctionBuilder,
) -> ReportResult<()> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(sender.pointer_type));
    let callee = sender
        .module
        .declare_function("printf", Linkage::Import, &sig)?;

    let str = create_constant_string(
        &format,
        &mut sender.module,
        &mut sender.data_context,
        &mut sender.string_constants.user_defined,
    )?;
    let format_value = reference_constant_string_data(str, sender, builder)?;

    let local_callee = sender.module.declare_func_in_func(callee, builder.func);
    let _call = builder.ins().call(local_callee, &[format_value, value]);

    Ok(())
}

fn send_read_statement<'a, M: Module>(
    var: &LValue<'a>,
    declared_type: Option<crate::types::Type>,
    prompt: &Option<Expr<'a>>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_write_lvalue_statement(
        var,
        sender,
        vars,
        function_sender,
        |type_, sender, vars, function_sender| {
            if let Some(t) = declared_type {
                type_.check(t)?;
            }

            if let Some(expr) = prompt {
                send_print_statement(expr, false, sender, vars, function_sender)?;
            }

            let (fun, ret_type) = match type_ {
                Chars => ("read_chars", sender.pointer_type),
                Number => ("read_num", types::F64),
                Boolean => ("read_bool", types::I32),
                Array(_) => unimplemented!(),
            };

            let parsed_value = sender.call_external_fn_for_result(
                &mut function_sender.builder,
                fun,
                &[],
                ret_type,
                &[],
            )?;

            Ok((type_, parsed_value))
        },
    )
}

fn send_declare_statement<'a, M: Module>(
    var: &'a pst::Variable<'a>,
    type_: Option<crate::types::Type>,
    expr: &Option<Vec<Expr<'a>>>,
    is_const: bool,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let pst::Variable(name) = var;
    let (type_, value) = send_init_variable(type_, expr, is_const, sender, vars, function_sender)?;

    let var = vars.new_variable();
    vars.insert(name, Callable::Var(type_, var, is_const));

    function_sender
        .builder
        .declare_var(var, ir_type(sender.pointer_type, type_));
    function_sender.builder.def_var(var, value);

    Ok(())
}

fn send_init_variable<'a, M: Module>(
    type_: Option<crate::types::Type>,
    exprs: &Option<Vec<Expr<'a>>>,
    is_const: bool,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let (type_, value) = match (type_, exprs) {
        (Some(type_), Some(exprs)) => {
            let (expr_type, value) = if exprs.len() > 1 {
                // type must be an array
                if let Array(element_type) = type_ {
                    send_array_expression(
                        element_type,
                        exprs,
                        is_const,
                        sender,
                        vars,
                        function_sender,
                    )?
                } else {
                    return Err(ReportError::TypeError("expected array".to_string()));
                }
            } else {
                send_expression(&exprs[0], sender, vars, function_sender)?
            };
            // ensure type matches what's declared.
            (type_.check(expr_type)?, value)
        }
        (Some(type_), None) => match type_ {
            Number => send_number_literal(0f64, function_sender),
            Boolean => send_boolean_literal(false, function_sender),
            type_ => {
                // default to null pointer
                let value = function_sender.builder.ins().iconst(sender.pointer_type, 0);
                (type_, value)
            }
        },
        (None, Some(exprs)) => send_expression(&exprs[0], sender, vars, function_sender)?,
        _ => {
            return Err(ReportError::TypeError(
                "must declare type or value".to_owned(),
            ));
        }
    };
    Ok((type_, value))
}

fn send_assign_statement<'a, M: Module>(
    var: &LValue<'a>,
    expr: &Expr<'a>,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_write_lvalue_statement(
        var,
        sender,
        vars,
        function_sender,
        |_, sender, vars, function_sender| send_expression(expr, sender, vars, function_sender),
    )?;

    Ok(())
}

fn send_write_lvalue_statement<'a, M: Module>(
    var: &LValue<'a>,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
    f: impl FnOnce(
        crate::types::Type,
        &mut Sender<M>,
        &Callables,
        &mut FunctionSender,
    ) -> ReportResult<(crate::types::Type, Value)>,
) -> ReportResult<()> {
    match var {
        LValue::Variable(name) => match vars.get(name)? {
            Callable::Arg(_, _) => Err(ReportError::TypeError(format!(
                "Sorry, you can't assign to an argument '{}",
                name
            ))),
            Callable::Func(_, _, _) => Err(ReportError::TypeError(format!(
                "Sorry, you can't assign to a paragraph '{}'",
                name
            ))),
            Callable::Var(type_, var, is_const) => {
                let (value_type, value) = f(*type_, sender, vars, function_sender)?;
                type_.check(value_type)?;
                if *is_const {
                    return Err(ReportError::TypeError(format!(
                        "Nopony can change what is always true: '{}'",
                        name
                    )));
                }
                function_sender.builder.def_var(*var, value);
                Ok(())
            }
            Callable::Global(type_, id, is_const) => {
                let (value_type, value) = f(*type_, sender, vars, function_sender)?;
                type_.check(value_type)?;
                if *is_const {
                    return Err(ReportError::TypeError(format!(
                        "Nopony can change what is always true: '{}'",
                        name
                    )));
                }
                let local_id = sender
                    .module
                    .declare_data_in_func(*id, function_sender.builder.func);
                let addr = function_sender
                    .builder
                    .ins()
                    .global_value(sender.pointer_type, local_id);
                function_sender
                    .builder
                    .ins()
                    .store(MemFlags::new(), value, addr, 0);
                Ok(())
            }
        },
        LValue::Index(Index(name, expr)) => {
            let (expr_type, expr_value) = send_expression(expr, sender, vars, function_sender)?;
            Number.check(expr_type)?;

            let callable = vars.get(name)?;
            let (element_type, array_value) = if let Some((type_, value)) =
                send_var(callable, sender, &mut function_sender.builder)
            {
                if let Array(element_type) = type_ {
                    (element_type, value)
                } else {
                    return Err(ReportError::TypeError(format!(
                        "Sorry, you can only index into an array, not a {}",
                        type_
                    )));
                }
            } else {
                return Err(ReportError::TypeError(format!(
                    "Sorry use must call a paragraph, not index it"
                )));
            };

            let type_ = element_type.into();
            let ir_type = ir_type(sender.pointer_type, type_);

            let (value_type, value) = f(type_, sender, vars, function_sender)?;
            type_.check(value_type)?;

            let index = function_sender
                .builder
                .ins()
                .fcvt_to_uint(types::I64, expr_value);

            let fun = match type_ {
                Chars => "array_set_chars",
                Number => "array_set_num",
                Boolean => "array_set_bool",
                Array(_) => {
                    return Err(ReportError::TypeError(format!(
                        "Sorry you can put an array in an array"
                    )));
                }
            };

            sender.call_external_fn(
                &mut function_sender.builder,
                fun,
                &[sender.pointer_type, sender.pointer_type, ir_type],
                &[array_value, index, value],
            )?;

            Ok(())
        }
    }
}

fn send_if_else<'a, M: Module>(
    cond: &Expr<'a>,
    if_: &'a [Statement<'a>],
    else_: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let (expr_type, expr_value) = send_expression(cond, sender, vars, function_sender)?;
    Boolean.check(expr_type)?;

    let else_block = function_sender.builder.create_block();
    let merge_block = function_sender.builder.create_block();
    function_sender
        .builder
        .ins()
        .brz(expr_value, else_block, &[]);

    let continue_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(continue_block, &[]);
    function_sender.builder.switch_to_block(continue_block);

    for statement in if_ {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(else_block);
    function_sender.seal_block(continue_block);
    function_sender.seal_block(else_block);

    for statement in else_ {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(merge_block);
    function_sender.seal_block(merge_block);

    Ok(())
}

fn send_while_statement<'a, M: Module>(
    cond: &Expr<'a>,
    body: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let header_block = function_sender.builder.create_block();
    let exit_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(header_block, &[]);
    function_sender.builder.switch_to_block(header_block);

    let (expr_type, expr_value) = send_expression(cond, sender, vars, function_sender)?;
    Boolean.check(expr_type)?;

    function_sender
        .builder
        .ins()
        .brz(expr_value, exit_block, &[]);

    let continue_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(continue_block, &[]);
    function_sender.builder.switch_to_block(continue_block);

    for statement in body {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }
    function_sender.builder.ins().jump(header_block, &[]);

    function_sender.builder.switch_to_block(exit_block);

    // We've reached the bottom of the loop, so there will be no
    // more backedges to the header to exits to the bottom.
    function_sender.seal_block(header_block);
    function_sender.seal_block(continue_block);
    function_sender.seal_block(exit_block);

    Ok(())
}

fn send_do_while_statement<'a, M: Module>(
    cond: &Expr<'a>,
    body: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let header_block = function_sender.builder.create_block();
    let exit_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(header_block, &[]);
    function_sender.builder.switch_to_block(header_block);

    for statement in body {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    let (expr_type, expr_value) = send_expression(cond, sender, vars, function_sender)?;
    Boolean.check(expr_type)?;

    function_sender
        .builder
        .ins()
        .brz(expr_value, exit_block, &[]);

    function_sender.builder.ins().jump(header_block, &[]);

    function_sender.builder.switch_to_block(exit_block);

    // We've reached the bottom of the loop, so there will be no
    // more backedges to the header to exits to the bottom.
    function_sender.seal_block(header_block);
    function_sender.seal_block(exit_block);

    Ok(())
}

fn send_for_statement<'a, M: Module>(
    type_: Option<crate::types::Type>,
    var: &pst::Variable<'a>,
    from: &Expr,
    to: &Expr,
    body: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    if let Some(type_) = type_ {
        Number.check(type_)?;
    }
    let (from_type, from_value) = send_expression(from, sender, vars, function_sender)?;
    Number.check(from_type)?;
    let (to_type, to_value) = send_expression(to, sender, vars, function_sender)?;
    Number.check(to_type)?;
    let pst::Variable(counter_name) = var;
    let counter = vars.new_variable();
    vars.insert(counter_name, Callable::Var(Number, counter, true));

    function_sender.builder.declare_var(counter, types::F64);
    function_sender.builder.def_var(counter, from_value);

    let header_block = function_sender.builder.create_block();
    let exit_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(header_block, &[]);
    function_sender.builder.switch_to_block(header_block);

    let counter_value = function_sender.builder.use_var(counter);
    let cmp = function_sender
        .builder
        .ins()
        .fcmp(FloatCC::GreaterThan, counter_value, to_value);
    function_sender.builder.ins().brnz(cmp, exit_block, &[]);

    let continue_block = function_sender.builder.create_block();
    function_sender.builder.ins().jump(continue_block, &[]);
    function_sender.builder.switch_to_block(continue_block);

    for statement in body {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    let one = function_sender.builder.ins().f64const(1f64);
    let inc = function_sender.builder.ins().fadd(counter_value, one);
    function_sender.builder.def_var(counter, inc);
    function_sender.builder.ins().jump(header_block, &[]);

    function_sender.builder.switch_to_block(exit_block);

    // We've reached the bottom of the loop, so there will be no
    // more backedges to the header to exits to the bottom.
    function_sender.seal_block(header_block);
    function_sender.seal_block(continue_block);
    function_sender.seal_block(exit_block);

    Ok(())
}

fn send_call<M: Module>(
    call: &Call,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<Option<(crate::types::Type, Value)>> {
    let Call(name, args) = call;
    let callable = vars.get(name)?;

    if args.is_empty() {
        if let Some((type_, value)) = send_var(callable, sender, &mut function_sender.builder) {
            return Ok(Some((type_, value)));
        }
    }

    if let Callable::Func(type_, arg_types, func_id) = callable {
        let mut arg_values = vec![];
        for (arg, arg_type) in args.iter().zip(arg_types) {
            let (expr_type, expr_value) = send_expression(arg, sender, vars, function_sender)?;
            arg_type.check(expr_type)?;
            arg_values.push(expr_value);
        }

        let local_callee = sender
            .module
            .declare_func_in_func(*func_id, &mut function_sender.builder.func);
        let result = function_sender
            .builder
            .ins()
            .call(local_callee, &arg_values);

        match type_ {
            None => Ok(None),
            Some(t) => {
                let v = function_sender.builder.inst_results(result)[0];
                Ok(Some((*t, v)))
            }
        }
    } else {
        return Err(ReportError::TypeError(format!(
            "Sorry, you can't call a variable: '{}'",
            name
        )));
    }
}

fn send_index<M: Module>(
    index: &Index,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let Index(name, index_expr) = index;

    let (expr_type, expr_value) = send_expression(index_expr, sender, vars, function_sender)?;
    Number.check(expr_type)?;

    let callable = vars.get(name)?;
    let (element_type, array_value) =
        if let Some((type_, value)) = send_var(callable, sender, &mut function_sender.builder) {
            if let Array(element_type) = type_ {
                (element_type, value)
            } else {
                return Err(ReportError::TypeError(format!(
                    "Sorry, you can only index into an array, not a {}",
                    type_
                )));
            }
        } else {
            return Err(ReportError::TypeError(format!(
                "Sorry use must call a paragraph, not index it"
            )));
        };
    let type_ = element_type.into();
    let ir_type = ir_type(sender.pointer_type, type_);

    let index = function_sender
        .builder
        .ins()
        .fcvt_to_uint(types::I64, expr_value);

    let fun = match element_type {
        ArrayType::Chars => "array_get_chars",
        ArrayType::Number => "array_get_num",
        ArrayType::Boolean => "array_get_bool",
    };

    let result = sender.call_external_fn_for_result(
        &mut function_sender.builder,
        fun,
        &[sender.pointer_type, sender.pointer_type],
        ir_type,
        &[array_value, index],
    )?;

    Ok((type_, result))
}

fn send_return<M: Module>(
    expr: &Expr,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let return_type = return_type.ok_or_else(|| {
        ReportError::TypeError("You need to declare the type you are returning".to_string())
    })?;

    let (expr_type, expr_value) = send_expression(expr, sender, vars, function_sender)?;
    return_type.check(expr_type)?;

    function_sender.builder.ins().return_(&[expr_value]);

    Ok(())
}

fn send_increment_statement<M: Module>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_update_var_statement(var, vars, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fadd(value, one)
    })
}

fn send_decrement_statement<M: Module>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_update_var_statement(var, vars, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fsub(value, one)
    })
}

fn send_update_var_statement<'a, 'b, M: Module>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender<'b>,
    f: impl FnOnce(&mut FunctionBuilder<'b>, Value) -> Value,
) -> ReportResult<()> {
    let pst::Variable(name) = var;
    match vars.get(name)? {
        Callable::Arg(_, _) => Err(ReportError::TypeError(format!(
            "Sorry, you can't assign to an argument '{}'",
            name
        ))),
        Callable::Func(_, _, _) => Err(ReportError::TypeError(format!(
            "Sorry, you can't assign to a paragraph '{}'",
            name
        ))),
        Callable::Var(type_, var, is_const) => {
            Number.check(*type_)?;
            if *is_const {
                return Err(ReportError::TypeError(format!(
                    "Nopony can change what is always true: '{}'",
                    name
                )));
            }

            let value = function_sender.builder.use_var(*var);
            let new_value = f(&mut function_sender.builder, value);
            function_sender.builder.def_var(*var, new_value);

            Ok(())
        }
        Callable::Global(type_, id, is_const) => {
            Number.check(*type_)?;
            if *is_const {
                return Err(ReportError::TypeError(format!(
                    "Nopony can change what is always true: '{}'",
                    name
                )));
            }

            let ir_type = ir_type(sender.pointer_type, *type_);
            let local_id = sender
                .module
                .declare_data_in_func(*id, function_sender.builder.func);
            let addr = function_sender
                .builder
                .ins()
                .global_value(sender.pointer_type, local_id);

            let value = function_sender
                .builder
                .ins()
                .load(ir_type, MemFlags::new(), addr, 0);
            let new_value = f(&mut function_sender.builder, value);
            function_sender
                .builder
                .ins()
                .store(MemFlags::new(), new_value, addr, 0);

            Ok(())
        }
    }
}

fn send_array_expression<'a, M: Module>(
    element_type: crate::types::ArrayType,
    exprs: &[Expr<'a>],
    is_const: bool,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let mut iter = exprs.iter();

    let array_header_alloc_size = if is_const {
        // size, pointer to data, data
        sender.pointer_type.bytes() as i64 * 2
            + exprs.len() as i64 * ir_type(sender.pointer_type, element_type.into()).bytes() as i64
    } else {
        // size, pointer to data, capacity
        sender.pointer_type.bytes() as i64 * 3
    };

    let array_header_size = function_sender
        .builder
        .ins()
        .iconst(sender.pointer_type, array_header_alloc_size);
    let array_header = alloc(array_header_size, 1, sender, &mut function_sender.builder)?;
    let element_size = function_sender.builder.ins().iconst(
        sender.pointer_type,
        (ir_type(sender.pointer_type, element_type.into())).bytes() as i64,
    );
    let array = if is_const {
        function_sender
            .builder
            .ins()
            .iadd_imm(array_header, (sender.pointer_type.bytes() * 2) as i64)
    } else {
        alloc(
            element_size,
            exprs.len() as i64,
            sender,
            &mut function_sender.builder,
        )?
    };
    let size = function_sender
        .builder
        .ins()
        .iconst(sender.pointer_type, exprs.len() as i64);
    function_sender
        .builder
        .ins()
        .store(MemFlags::new(), size, array_header, 0);
    function_sender.builder.ins().store(
        MemFlags::new(),
        array,
        array_header,
        sender.pointer_type.bytes() as i32,
    );
    function_sender.builder.ins().store(
        MemFlags::new(),
        size,
        array_header,
        (sender.pointer_type.bytes() * 2) as i32,
    );

    let mut offset = 0i32;
    while let Some(expr) = iter.next() {
        let (expr_type, expr_value) = send_expression(expr, sender, vars, function_sender)?;
        // ensure all elements are the same type.
        let t: crate::types::Type = element_type.into();
        t.check(expr_type)?;

        function_sender
            .builder
            .ins()
            .store(MemFlags::new(), expr_value, array, offset);
        offset += ir_type(sender.pointer_type, expr_type).bytes() as i32;
    }

    Ok((Array(element_type), array_header))
}

fn send_expression<'a, M: Module>(
    expr: &Expr<'a>,
    sender: &mut Sender<M>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let result = match expr {
        Expr::BinOp(op, left, right) => {
            let (left_type, left_value) = send_expression(left, sender, vars, function_sender)?;
            let (right_type, right_value) = send_expression(right, sender, vars, function_sender)?;

            match op {
                BinOperator::AddOrAnd => {
                    if left_type.is_number() {
                        // assume add
                        Number.check_bin(left_type, right_type, || {
                            function_sender.builder.ins().fadd(left_value, right_value)
                        })
                    } else {
                        // assume and
                        Boolean.check_bin(left_type, right_type, || {
                            function_sender.builder.ins().band(left_value, right_value)
                        })
                    }
                }
                BinOperator::Sub => Number.check_bin(left_type, right_type, || {
                    function_sender.builder.ins().fsub(left_value, right_value)
                }),
                BinOperator::Mul => Number.check_bin(left_type, right_type, || {
                    function_sender.builder.ins().fmul(left_value, right_value)
                }),
                BinOperator::Div => Number.check_bin(left_type, right_type, || {
                    function_sender.builder.ins().fdiv(left_value, right_value)
                }),
                BinOperator::Or => Boolean.check_bin(left_type, right_type, || {
                    function_sender.builder.ins().bor(left_value, right_value)
                }),
                BinOperator::EitherOr => Boolean.check_bin(left_type, right_type, || {
                    function_sender.builder.ins().bxor(left_value, right_value)
                }),
                BinOperator::Equal => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::Equal,
                        FloatCC::Equal,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
                BinOperator::NotEqual => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::NotEqual,
                        FloatCC::NotEqual,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
                BinOperator::LessThan => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::SignedLessThan,
                        FloatCC::LessThan,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
                BinOperator::GreaterThan => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::SignedGreaterThan,
                        FloatCC::GreaterThan,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
                BinOperator::LessThanOrEqual => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::SignedLessThanOrEqual,
                        FloatCC::LessThanOrEqual,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
                BinOperator::GreaterThanOrEqual => {
                    let type_ = left_type.check(right_type)?;
                    send_comparison(
                        type_,
                        IntCC::SignedGreaterThanOrEqual,
                        FloatCC::GreaterThanOrEqual,
                        left_value,
                        right_value,
                        sender,
                        function_sender,
                    )
                }
            }?
        }
        Expr::Not(expr) => {
            let (type_, value) = send_expression(expr, sender, vars, function_sender)?;

            Boolean.check(type_)?;
            let b = function_sender
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, value, 0);
            (type_, function_sender.builder.ins().bint(types::I32, b))
        }
        Expr::Concat(exprs) => {
            let mut values = vec![];

            for expr in exprs {
                let (expr_type, expr_value) = send_expression(expr, sender, vars, function_sender)?;
                values.push((
                    expr_type,
                    expr_value,
                    calculate_buff_size(expr_type, expr_value, sender, function_sender)?,
                ));
            }

            let mut len = function_sender.builder.ins().iconst(sender.pointer_type, 0);
            for (_, _, buff_size) in &values {
                let entry_size = match buff_size {
                    SizedValue::Simple(size) => size,
                    SizedValue::Dynamic { size, .. } => size,
                };
                len = function_sender.builder.ins().iadd(len, *entry_size);
            }

            let result = sender.call_external_fn_for_result(
                &mut function_sender.builder,
                "alloc_chars",
                &[sender.pointer_type],
                sender.pointer_type,
                &[len],
            )?;

            let mut offset = chars_data(result, sender, &mut function_sender.builder);

            for (type_, value, buff_size) in &values {
                match buff_size {
                    SizedValue::Simple(size) => {
                        match type_ {
                            Chars => {
                                write_chars(*value, offset, sender, &mut function_sender.builder);
                            }
                            Boolean => {
                                let string = bool_to_string(*value, sender, function_sender)?;
                                write_chars(string, offset, sender, &mut function_sender.builder);
                            }
                            _ => unreachable!(),
                        }
                        offset = function_sender.builder.ins().iadd(offset, *size);
                    }
                    SizedValue::Dynamic { size, ptr } => {
                        sender.call_external_fn(
                            &mut function_sender.builder,
                            "move_chars",
                            &[sender.pointer_type, sender.pointer_type],
                            &[*ptr, offset],
                        )?;
                        offset = function_sender.builder.ins().iadd(offset, *size);
                    }
                }
            }

            (Chars, result)
        }
        Expr::Lit(lit) => send_literal(lit, sender, function_sender)?,
        Expr::Call(call) => send_call(call, sender, vars, function_sender)?.ok_or_else(|| {
            ReportError::TypeError(format!(
                "You need to return something from '{}' if you want to use it",
                call.0
            ))
        })?,
        Expr::Index(index) => send_index(index, sender, vars, function_sender)?,
    };

    Ok(result)
}

fn write_chars<M: Module>(
    value: Value,
    dst: Value,
    sender: &mut Sender<M>,
    builder: &mut FunctionBuilder,
) {
    let len = chars_len(value, sender, builder);
    let src = chars_data(value, sender, builder);
    builder.call_memcpy(sender.module.target_config(), dst, src, len);
}

fn calculate_buff_size<M: Module>(
    type_: crate::types::Type,
    value: Value,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<SizedValue> {
    let result = match type_ {
        Chars => SizedValue::Simple(chars_len(value, sender, &mut function_sender.builder)),
        Number => {
            let chars = sender.call_external_fn_for_result(
                &mut function_sender.builder,
                "num_to_chars",
                &[types::F64],
                sender.pointer_type,
                &[value],
            )?;
            SizedValue::Dynamic {
                size: chars_len(chars, sender, &mut function_sender.builder),
                ptr: chars,
            }
        }
        Boolean => SizedValue::Simple(function_sender.builder.ins().iconst(sender.pointer_type, 3)),
        Array(type_) => {
            let fun = match type_ {
                ArrayType::Chars => "array_chars_to_chars",
                ArrayType::Number => "array_num_to_chars",
                ArrayType::Boolean => "array_bool_to_chars",
            };
            let chars = sender.call_external_fn_for_result(
                &mut function_sender.builder,
                fun,
                &[sender.pointer_type],
                sender.pointer_type,
                &[value],
            )?;
            SizedValue::Dynamic {
                size: chars_len(chars, sender, &mut function_sender.builder),
                ptr: chars,
            }
        }
    };
    Ok(result)
}

fn send_comparison<M: Module>(
    type_: crate::types::Type,
    icc: IntCC,
    fcc: FloatCC,
    left_value: Value,
    right_value: Value,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    Ok((
        Boolean,
        match type_ {
            Chars => {
                let value = sender.call_external_fn_for_result(
                    &mut function_sender.builder,
                    "compare_chars",
                    &[sender.pointer_type, sender.pointer_type],
                    types::I32,
                    &[left_value, right_value],
                )?;
                let cmp = function_sender.builder.ins().icmp_imm(icc, value, 0);
                function_sender.builder.ins().bint(types::I32, cmp)
            }
            Number => {
                let cmp = function_sender
                    .builder
                    .ins()
                    .fcmp(fcc, left_value, right_value);
                function_sender.builder.ins().bint(types::I32, cmp)
            }
            Boolean => {
                let cmp = function_sender
                    .builder
                    .ins()
                    .icmp(icc, left_value, right_value);
                function_sender.builder.ins().bint(types::I32, cmp)
            }
            Array(_) => unimplemented!(),
        },
    ))
}

fn send_literal<'a, M: Module>(
    lit: &Literal<'a>,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let result = match lit {
        Literal::Chars(string) => {
            let id = sender.create_constant_string(string)?;
            (
                crate::types::Type::Chars,
                reference_constant_string(id, sender, &mut function_sender.builder)?,
            )
        }
        Literal::Number(n) => send_number_literal(*n, function_sender),
        Literal::Boolean(b) => send_boolean_literal(*b, function_sender),
    };
    Ok(result)
}

fn send_number_literal(
    n: f64,
    function_sender: &mut FunctionSender,
) -> (crate::types::Type, Value) {
    (
        crate::types::Type::Number,
        function_sender.builder.ins().f64const(n),
    )
}

fn send_boolean_literal(
    b: bool,
    function_sender: &mut FunctionSender,
) -> (crate::types::Type, Value) {
    (
        crate::types::Type::Boolean,
        function_sender
            .builder
            .ins()
            .iconst(types::I32, if b { 1 } else { 0 }),
    )
}

fn create_constant_string<M: Module>(
    string: &str,
    module: &mut M,
    data_context: &mut DataContext,
    constants: &mut HashMap<String, DataId>,
) -> ReportResult<DataId> {
    if let Some(id) = constants.get(string) {
        return Ok(*id);
    }

    let len = string.len();
    let mut s: Vec<u8> =
        Vec::with_capacity(len + module.target_config().pointer_bytes() as usize * 2);
    s.write(&(-1isize).to_le_bytes()); // ref count
    s.write(&len.to_le_bytes()); // size
    s.write(string.as_bytes()); // contents

    // let chars = sparkle::Chars::from_bytes(string.as_bytes());

    data_context.define(s.into_boxed_slice());
    let id = module.declare_data(string, Linkage::Export, false, false)?;

    module.define_data(id, &data_context)?;

    data_context.clear();

    constants.insert(string.to_owned(), id);

    Ok(id)
}

fn bool_to_string<M: Module>(
    bool_value: Value,
    sender: &mut Sender<M>,
    function_sender: &mut FunctionSender,
) -> ReportResult<Value> {
    let else_block = function_sender.builder.create_block();
    let merge_block = function_sender.builder.create_block();
    function_sender
        .builder
        .append_block_param(merge_block, sender.pointer_type);

    let then_return = reference_constant_string(
        sender.string_constants.yes,
        sender,
        &mut function_sender.builder,
    )?;
    function_sender
        .builder
        .ins()
        .brz(bool_value, else_block, &[]);
    function_sender
        .builder
        .ins()
        .jump(merge_block, &[then_return]);

    function_sender.builder.switch_to_block(else_block);
    function_sender.seal_block(else_block);
    let else_return = reference_constant_string(
        sender.string_constants.no,
        sender,
        &mut function_sender.builder,
    )?;
    function_sender
        .builder
        .ins()
        .jump(merge_block, &[else_return]);

    function_sender.builder.switch_to_block(merge_block);
    function_sender.seal_block(merge_block);

    Ok(function_sender.builder.block_params(merge_block)[0])
}

fn reference_constant_string<M: Module>(
    id: DataId,
    sender: &mut Sender<M>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let local_id = sender.module.declare_data_in_func(id, builder.func);

    let string = builder
        .ins()
        .symbol_value(sender.module.target_config().pointer_type(), local_id);

    Ok(string)
}

fn reference_constant_string_data<M: Module>(
    id: DataId,
    sender: &mut Sender<M>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let string = reference_constant_string(id, sender, builder)?;

    Ok(builder
        .ins()
        .iadd_imm(string, sender.pointer_type.bytes() as i64 * 2))
}

fn alloc<M: Module>(
    size: Value,
    count: i64,
    sender: &mut Sender<M>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(sender.pointer_type));

    let nmemb = builder.ins().iconst(sender.pointer_type, count);
    let callee = sender
        .module
        .declare_function("calloc", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let call = builder.ins().call(local_callee, &[nmemb, size]);

    Ok(builder.inst_results(call)[0])
}

fn chars_len<M: Module>(value: Value, sender: &Sender<M>, builder: &mut FunctionBuilder) -> Value {
    builder.ins().load(
        sender.pointer_type,
        MemFlags::new(),
        value,
        sender.pointer_type.bytes() as i32,
    )
}

fn set_chars_len<M: Module>(
    value: Value,
    len: Value,
    sender: &Sender<M>,
    builder: &mut FunctionBuilder,
) {
    builder.ins().store(
        MemFlags::new(),
        len,
        value,
        sender.pointer_type.bytes() as i32,
    );
}

fn chars_data<M: Module>(value: Value, sender: &Sender<M>, builder: &mut FunctionBuilder) -> Value {
    builder
        .ins()
        .iadd_imm(value, sender.pointer_type.bytes() as i64 * 2)
}

fn array_size<M: Module>(value: Value, sender: &Sender<M>, builder: &mut FunctionBuilder) -> Value {
    builder
        .ins()
        .load(sender.pointer_type, MemFlags::new(), value, 0)
}

fn array_data<M: Module>(value: Value, sender: &Sender<M>, builder: &mut FunctionBuilder) -> Value {
    builder.ins().load(
        sender.pointer_type,
        MemFlags::new(),
        value,
        sender.pointer_type.bytes() as i32,
    )
}

struct Sender<M: Module> {
    triple: Triple,
    module: M,
    data_context: DataContext,
    pointer_type: Type,
    string_constants: StringConstants,
}

struct StringConstants {
    yes: DataId,
    no: DataId,
    user_defined: HashMap<String, DataId>,
}

impl<M: Module> Sender<M> {
    fn new(mut module: M, triple: Triple) -> ReportResult<Sender<M>> {
        let pointer_type = module.target_config().pointer_type();
        let mut data_context = DataContext::new();
        let mut user_defined_constants = HashMap::new();
        let string_constants = StringConstants {
            yes: create_constant_string(
                "yes",
                &mut module,
                &mut data_context,
                &mut user_defined_constants,
            )?,
            no: create_constant_string(
                "no",
                &mut module,
                &mut data_context,
                &mut user_defined_constants,
            )?,
            user_defined: user_defined_constants,
        };
        Ok(Sender {
            triple,
            module,
            data_context,
            pointer_type,
            string_constants,
        })
    }

    fn create_constant_string(&mut self, string: &str) -> ReportResult<DataId> {
        create_constant_string(
            string,
            &mut self.module,
            &mut self.data_context,
            &mut self.string_constants.user_defined,
        )
    }

    fn is_windows(&self) -> bool {
        self.triple.operating_system == OperatingSystem::Windows
    }

    fn finalize(&mut self, context: &mut Context) {
        self.module.clear_context(context);
    }

    fn call_external_fn(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        name: &str,
        args: &[types::Type],
        values: &[Value],
    ) -> ReportResult<()> {
        let mut sig = self.module.make_signature();
        for arg in args {
            sig.params.push(AbiParam::new(*arg));
        }
        let callee = self.module.declare_function(name, Linkage::Import, &sig)?;
        let local_callee = self.module.declare_func_in_func(callee, builder.func);
        let _call = builder.ins().call(local_callee, values);

        Ok(())
    }

    fn call_external_fn_for_result(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        name: &str,
        args: &[types::Type],
        ret_type: types::Type,
        values: &[Value],
    ) -> ReportResult<Value> {
        let mut sig = self.module.make_signature();
        for arg in args {
            sig.params.push(AbiParam::new(*arg));
        }
        sig.returns.push(AbiParam::new(ret_type));
        let callee = self.module.declare_function(name, Linkage::Import, &sig)?;
        let local_callee = self.module.declare_func_in_func(callee, builder.func);
        let call = builder.ins().call(local_callee, values);

        Ok(builder.inst_results(call)[0])
    }
}

struct FunctionSender<'b> {
    builder: FunctionBuilder<'b>,
}

impl<'b> FunctionSender<'b> {
    fn new(
        context: &'b mut Context,
        function_builder_context: &'b mut FunctionBuilderContext,
    ) -> FunctionSender<'b> {
        FunctionSender {
            builder: FunctionBuilder::new(&mut context.func, function_builder_context),
        }
    }

    fn seal_block(&mut self, block: Block) {
        self.builder.seal_block(block);
        #[cfg(debug_assertions)]
        {
            if let Err((inst, msg)) = self.builder.func.is_block_basic(block) {
                let inst_str = self.builder.func.dfg.display_inst(inst);
                panic!(
                    "{} failed basic block invariants on {}, {}",
                    block, inst_str, msg
                );
            }
        }
    }
}

fn ir_type(pointer_type: Type, type_: crate::types::Type) -> Type {
    match type_ {
        Chars => pointer_type,
        Number => types::F64,
        // There are numerous bugs with b1, use i32 instead.
        // https://github.com/bytecodealliance/cranelift/issues/1117
        Boolean => types::I32,
        Array(_) => pointer_type,
    }
}

fn send_var<M: Module>(
    callable: &Callable,
    sender: &Sender<M>,
    builder: &mut FunctionBuilder,
) -> Option<(crate::types::Type, Value)> {
    match callable {
        Callable::Arg(type_, value) => Some((*type_, *value)),
        Callable::Var(type_, var, _) => {
            let value = builder.use_var(*var);
            Some((*type_, value))
        }
        Callable::Global(type_, id, _) => {
            let local_id = sender.module.declare_data_in_func(*id, builder.func);
            let ir_type = ir_type(sender.pointer_type, *type_);
            let addr = builder.ins().global_value(sender.pointer_type, local_id);
            let value = builder.ins().load(ir_type, MemFlags::new(), addr, 0);
            Some((*type_, value))
        }
        Callable::Func(_, _, _) => None,
    }
}

enum SizedValue {
    Simple(Value),
    Dynamic { size: Value, ptr: Value },
}

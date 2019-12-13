use std::fs::File;

use std::str::FromStr;
use std::{mem};

use cranelift::codegen::Context;
use cranelift::prelude::settings::{self, Configurable};
use cranelift::prelude::*;
use cranelift::prelude::{isa, AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{default_libcall_names, Backend, DataContext, FuncId, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use target_lexicon::{OperatingSystem, Triple};

use crate::error::ReportError;
use crate::pst;
use crate::pst::{
    Arg, BinOperator, Call, Declaration, DeclareVar, Expr, Literal, Paragraph, Report, Statement,
};
use crate::types::Type::{Boolean, Chars, Number};
use crate::vars::{Callable, Callables};

use cranelift_object::{ObjectBackend, ObjectBuilder, ObjectTrapCollection};
use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::io::Write;
use std::os::raw::c_char;
use std::path::Path;

type ReportResult<T> = Result<T, ReportError>;

pub fn send_out<'a>(report: &'a Report, name: &str, target: &str) -> ReportResult<String> {
    let mut sender = object_sender(name, target)?;
    let mut globals = Callables::new();
    let mut context = sender.module.make_context();
    send(report, &mut sender, &mut globals, &mut context, |_, _| {
        Ok(())
    })?;
    sender.finalize(&mut context);
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
    let mut context = sender.module.make_context();
    let id = send(report, &mut sender, &mut globals, &mut context, |_, _| {
        Ok(())
    })?;
    sender.finalize(&mut context);
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> (isize)>(code) };

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
    let mut context = sender.module.make_context();

    let mut buff = String::new();
    send(
        &report,
        &mut sender,
        &mut globals,
        &mut context,
        |sender, context| {
            cranelift::codegen::write_function(
                &mut buff,
                &context.func,
                &Some(sender.module.isa()).into(),
            )?;
            Ok(())
        },
    )?;

    print!("{}", buff);

    Ok(())
}

fn object_sender(name: &str, target: &str) -> ReportResult<Sender<ObjectBackend>> {
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();
    let triple = Triple::from_str(target)?;
    let isa_builder = isa::lookup(triple.clone())?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let builder = ObjectBuilder::new(
        isa,
        name.to_owned(),
        ObjectTrapCollection::Disabled,
        default_libcall_names(),
    )
    .unwrap();

    let module = Module::<ObjectBackend>::new(builder);

    Ok(Sender::new(module, triple))
}

fn simple_jit_sender(target: &str) -> ReportResult<Sender<SimpleJITBackend>> {
    let triple = Triple::from_str(target)?;
    let builder = SimpleJITBuilder::new(default_libcall_names());
    let module = Module::<SimpleJITBackend>::new(builder);

    Ok(Sender::new(module, triple))
}

fn send<'a, B: Backend>(
    report: &'a Report,
    sender: &'a mut Sender<B>,
    globals: &'a mut Callables<'a>,
    context: &mut Context,
    mut f: impl FnMut(&Sender<B>, &Context) -> ReportResult<()>,
) -> ReportResult<FuncId> {
    send_constants(sender)?;

    let mut mane_paragraphs: Vec<FuncId> = vec![];
    let mut declared_paragraphs: Vec<(FuncId, &Paragraph)> = vec![];

    let sig = sender.module.make_signature();
    let init_globals = sender
        .module
        .declare_function("init_globals", Linkage::Local, &sig)?;

    let mut builder_context = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut function_sender = FunctionSender::new(builder);
    let entry_ebb = function_sender.builder.create_ebb();
    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

    for declaration in &report.declarations {
        match declaration {
            Declaration::Paragraph(paragraph) => {
                let id = declare_paragraph(paragraph, sender)?;
                let arg_types = paragraph.args.iter().map(|Arg(type_, _)| *type_).collect();
                globals.insert(
                    paragraph.name,
                    Callable::Func(paragraph.return_type, arg_types, id),
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
    sender.module.define_function(init_globals, context)?;
    f(sender, context)?;
    sender.module.clear_context(context);

    for (id, paragraph) in declared_paragraphs {
        send_paragraph(paragraph, id, sender, globals, context, &mut f)?;
    }

    let id = send_mane(&mane_paragraphs, init_globals, sender, context, &mut f)?;

    Ok(id)
}

fn send_constants<B: Backend>(sender: &mut Sender<B>) -> ReportResult<()> {
    // for printing booleans
    create_constant_string("yes", sender)?;
    create_constant_string("no", sender)?;
    // for printing 'nothing'
    create_constant_string("nothing", sender)?;
    // for formatting numbers
    create_constant_string("%g", sender)?;
    // for parsing booleans
    create_constant_string("true", sender)?;
    create_constant_string("false", sender)?;
    create_constant_string("right", sender)?;
    create_constant_string("wrong", sender)?;
    create_constant_string("correct", sender)?;
    create_constant_string("incorrect", sender)?;
    Ok(())
}

fn send_declare_global<'a, B: Backend>(
    declare_var: &DeclareVar<'a>,
    sender: &mut Sender<B>,
    globals: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let DeclareVar(pst::Variable(name), type_, expr, is_const) = declare_var;
    let (type_, _value) = send_init_variable(*type_, expr, sender, globals, function_sender)?;

    sender
        .data_context
        .define_zeroinit(ir_type(sender.pointer_type, type_).bytes() as usize);
    let id = sender
        .module
        .declare_data(name, Linkage::Local, !*is_const, Option::None)?;
    sender.module.define_data(id, &sender.data_context)?;
    sender.data_context.clear();

    globals.insert(name, Callable::Global(type_, id, *is_const));

    Ok(())
}

fn send_mane<B: Backend>(
    mane_paragraphs: &[FuncId],
    init_globals: FuncId,
    sender: &mut Sender<B>,
    context: &mut Context,
    f: &mut impl FnMut(&Sender<B>, &Context) -> ReportResult<()>,
) -> ReportResult<FuncId> {
    let mut builder_context = FunctionBuilderContext::new();
    let int = sender.pointer_type;
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(sender.pointer_type));

    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut function_sender = FunctionSender::new(builder);

    let entry_ebb = function_sender.builder.create_ebb();
    function_sender
        .builder
        .append_ebb_params_for_function_params(entry_ebb);
    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

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

    sender.module.define_function(id, context)?;

    f(sender, context)?;

    sender.module.clear_context(context);

    Ok(id)
}

fn declare_paragraph<'a, B: Backend>(
    paragraph: &'a Paragraph,
    sender: &mut Sender<B>,
) -> ReportResult<FuncId> {
    let mut sig = sender.module.make_signature();
    for Arg(type_, _) in &paragraph.args {
        sig.params
            .push(AbiParam::new(ir_type(sender.pointer_type, *type_)));
    }
    if let Some(type_) = paragraph.return_type {
        sig.returns
            .push(AbiParam::new(ir_type(sender.pointer_type, type_)));
    }
    let id = sender
        .module
        .declare_function(paragraph.name, Linkage::Local, &sig)?;

    Ok(id)
}

fn send_paragraph<'a, B: Backend>(
    paragraph: &'a Paragraph,
    func_id: FuncId,
    sender: &mut Sender<B>,
    globals: &'a Callables<'a>,
    context: &mut Context,
    f: &mut impl FnMut(&Sender<B>, &Context) -> ReportResult<()>,
) -> ReportResult<()> {
    let mut builder_context = FunctionBuilderContext::new();

    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut vars = globals.create_child();
    let mut function_sender = FunctionSender::new(builder);

    let entry_ebb = function_sender.builder.create_ebb();

    for Arg(arg_type, name) in &paragraph.args {
        let value_type = ir_type(sender.pointer_type, *arg_type);
        function_sender
            .builder
            .func
            .signature
            .params
            .push(AbiParam::new(value_type));
        let arg_value = function_sender
            .builder
            .append_ebb_param(entry_ebb, value_type);
        vars.insert(*name, Callable::Arg(*arg_type, arg_value));
    }
    if let Some(return_type) = paragraph.return_type {
        function_sender
            .builder
            .func
            .signature
            .returns
            .push(AbiParam::new(ir_type(sender.pointer_type, return_type)));
    }

    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

    for statement in paragraph.statements.iter() {
        send_statement(
            statement,
            paragraph.return_type,
            sender,
            &mut vars,
            &mut function_sender,
        )?;
    }

    if paragraph.return_type.is_none() {
        function_sender.builder.ins().return_(&[]);
    }

    sender.module.define_function(func_id, context)?;

    f(sender, context)?;

    sender.module.clear_context(context);

    Ok(())
}

fn send_statement<'a, B: Backend>(
    statement: &'a Statement<'a>,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    match statement {
        Statement::Print(expr) => send_print_statement(expr, true, sender, vars, function_sender),
        Statement::Read(var, type_, prompt) => {
            send_read_statement(var, type_, prompt, sender, vars, function_sender)
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
        Statement::Call(call) => {
            send_call(call, sender, vars, function_sender)?;
            Ok(())
        }
        Statement::Return(expr) => send_return(expr, return_type, sender, vars, function_sender),
    }
}

fn send_print_statement<'a, B: Backend>(
    expr: &Expr<'a>,
    newline: bool,
    sender: &mut Sender<B>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let (type_, value) = send_expression(expr, sender, vars, function_sender)?;

    let value = match type_ {
        crate::types::Type::Chars => {
            // If this is a null pointer, we need to print 'nothing' instead.
            let merge_block = function_sender.builder.create_ebb();
            function_sender
                .builder
                .append_ebb_param(merge_block, sender.pointer_type);
            let res = function_sender
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, value, 0);
            function_sender
                .builder
                .ins()
                .brz(res, merge_block, &[value]);
            let nothing =
                reference_constant_string("nothing", sender, &mut function_sender.builder)?;
            function_sender.builder.ins().jump(merge_block, &[nothing]);
            function_sender.builder.switch_to_block(merge_block);
            function_sender.builder.seal_block(merge_block);
            function_sender.builder.ebb_params(merge_block)[0]
        }
        crate::types::Type::Number => {
            // convert to string
            let slot = function_sender
                .builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 24));
            let buff = function_sender
                .builder
                .ins()
                .stack_addr(sender.pointer_type, slot, 0);
            let buff_size = function_sender.builder.ins().iconst(types::I64, 24);
            float_to_string(value, buff, buff_size, sender, &mut function_sender.builder)?;
            buff
        }
        crate::types::Type::Boolean => bool_to_string(value, sender, &mut function_sender.builder)?,
    };

    print(value, newline, sender, &mut function_sender.builder)?;

    Ok(())
}

fn print<B: Backend>(
    value: Value,
    newline: bool,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<()> {
    if newline {
        let mut sig = sender.module.make_signature();
        sig.params.push(AbiParam::new(sender.pointer_type));
        let callee = sender
            .module
            .declare_function("puts", Linkage::Import, &sig)?;
        let local_callee = sender.module.declare_func_in_func(callee, builder.func);

        let _call = builder.ins().call(local_callee, &[value]);
    } else {
        let mut sig = sender.module.make_signature();
        sig.params.push(AbiParam::new(sender.pointer_type));
        sig.params.push(AbiParam::new(sender.pointer_type));
        let callee = sender
            .module
            .declare_function("fputs", Linkage::Import, &sig)?;
        let local_callee = sender.module.declare_func_in_func(callee, builder.func);

        let stdout_name = match sender.triple.operating_system {
            OperatingSystem::Darwin { .. }  => "__stdoutp",
            _ => "stdout"
        };

        let sym = sender
            .module
            .declare_data(stdout_name, Linkage::Import, false, None)?;
        let local_id = sender.module.declare_data_in_func(sym, builder.func);
        let stdout_ptr = builder
            .ins()
            .symbol_value(sender.module.target_config().pointer_type(), local_id);
        let stdout = builder.ins().load(sender.pointer_type, MemFlags::new(), stdout_ptr, 0);

        let _call = builder
            .ins()
            .call(local_callee, &[value, stdout]);
    }
    Ok(())
}

fn send_read_statement<'a, B: Backend>(
    var: &'a pst::Variable<'a>,
    declared_type: &Option<crate::types::Type>,
    prompt: &Option<Expr<'a>>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_write_var_statement(
        var,
        sender,
        vars,
        function_sender,
        |type_, sender, vars, function_sender| {
            if let Some(t) = declared_type {
                type_.check(*t)?;
            }

            if let Some(expr) = prompt {
                send_print_statement(expr, false, sender, vars, function_sender)?;
            }

            let builder = &mut function_sender.builder;

            let line_slot = builder.create_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                types::I32.bytes(),
            ));
            let line_ptr = builder.ins().stack_addr(sender.pointer_type, line_slot, 0);
            let zero = builder.ins().iconst(sender.pointer_type, 0);
            builder.ins().stack_store(zero, line_slot, 0);

            let len = get_line(line_ptr, sender, builder)?;
            let line = builder.ins().load(sender.pointer_type, MemFlags::new(), line_ptr, 0);
            // strip off newline.
            let one = builder.ins().iconst(types::I64, 1);
            let newline_index = builder.ins().isub(len, one);
            let newline_ptr = builder.ins().iadd(line, newline_index);
            let zero = builder.ins().iconst(types::I8, 0);
            builder.ins().store(MemFlags::new(), zero, newline_ptr, 0);

            let parsed_value = match type_ {
                Chars => line,
                Number => {
                    string_to_double(line, sender, builder)?
                }
                Boolean => {
                    let merge_block = builder.create_ebb();
                    builder.append_ebb_param(merge_block, types::I32);

                    let yes = reference_constant_string("yes", sender, builder)?;
                    let no = reference_constant_string("no", sender, builder)?;
                    let true_ = reference_constant_string("true", sender, builder)?;
                    let false_ = reference_constant_string("false", sender, builder)?;
                    let right = reference_constant_string("right", sender, builder)?;
                    let wrong = reference_constant_string("wrong", sender, builder)?;
                    let correct = reference_constant_string("correct", sender, builder)?;
                    let incorrect = reference_constant_string("incorrect", sender, builder)?;

                    let zero = builder.ins().iconst(types::I32, 0);
                    let one = builder.ins().iconst(types::I32, 1);

                    let cmp = compare_strings(line, yes, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[one]);
                    let cmp = compare_strings(line, true_, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[one]);
                    let cmp = compare_strings(line, right, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[one]);
                    let cmp = compare_strings(line, correct, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[one]);

                    let cmp = compare_strings(line, no, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[zero]);
                    let cmp = compare_strings(line, false_, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[zero]);
                    let cmp = compare_strings(line, wrong, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[zero]);
                    let cmp = compare_strings(line, incorrect, sender, builder)?;
                    builder.ins().brz(cmp, merge_block, &[zero]);

                    // TODO: print a useful error.
                    builder.ins().trap(TrapCode::User(1));

                    builder.switch_to_block(merge_block);
                    builder.seal_block(merge_block);

                    builder.ebb_params(merge_block)[0]
                },
            };

            Ok((type_, parsed_value))
        }
    )
}

fn send_declare_statement<'a, B: Backend>(
    var: &'a pst::Variable<'a>,
    type_: Option<crate::types::Type>,
    expr: &Option<Expr<'a>>,
    is_const: bool,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let pst::Variable(name) = var;
    let (type_, value) = send_init_variable(type_, expr, sender, vars, function_sender)?;

    let var = vars.new_variable();
    vars.insert(name, Callable::Var(type_, var, is_const));

    function_sender
        .builder
        .declare_var(var, ir_type(sender.pointer_type, type_));
    function_sender.builder.def_var(var, value);

    Ok(())
}

fn send_init_variable<'a, B: Backend>(
    type_: Option<crate::types::Type>,
    expr: &Option<Expr<'a>>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let (type_, value) = match (type_, expr) {
        (Some(type_), Some(expr)) => {
            let (expr_type, value) = send_expression(expr, sender, vars, function_sender)?;
            // ensure type matches what's declared.
            (type_.check(expr_type)?, value)
        }
        (Some(type_), None) => match type_ {
            Chars => {
                let value = function_sender.builder.ins().iconst(sender.pointer_type, 0);
                (Chars, value)
            }
            Number => send_number_literal(0f64, function_sender),
            Boolean => send_boolean_literal(false, function_sender),
        },
        (None, Some(expr)) => send_expression(expr, sender, vars, function_sender)?,
        _ => {
            return Err(ReportError::TypeError(
                "must declare type or value".to_owned(),
            ))
        }
    };
    Ok((type_, value))
}

fn send_assign_statement<'a, B: Backend>(
    var: &pst::Variable<'a>,
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_write_var_statement(
        var,
        sender,
        vars,
        function_sender,
        |_, sender, vars, function_sender| send_expression(expr, sender, vars, function_sender),
    )?;

    Ok(())
}

fn send_write_var_statement<'a, B: Backend>(
    var: &pst::Variable<'a>,
    sender: &mut Sender<B>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
    f: impl FnOnce(
        crate::types::Type,
        &mut Sender<B>,
        &Callables,
        &mut FunctionSender,
    ) -> ReportResult<(crate::types::Type, Value)>,
) -> ReportResult<()> {
    let pst::Variable(name) = var;
    match vars.get(name)? {
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
    }
}

fn send_if_else<'a, B: Backend>(
    cond: &Expr<'a>,
    if_: &'a [Statement<'a>],
    else_: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let (expr_type, expr_value) = send_expression(cond, sender, vars, function_sender)?;
    Boolean.check(expr_type)?;

    let else_block = function_sender.builder.create_ebb();
    let merge_block = function_sender.builder.create_ebb();
    function_sender
        .builder
        .ins()
        .brz(expr_value, else_block, &[]);

    for statement in if_ {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(else_block);
    function_sender.builder.seal_block(else_block);

    for statement in else_ {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(merge_block);
    function_sender.builder.seal_block(merge_block);

    Ok(())
}

fn send_while_statement<'a, B: Backend>(
    cond: &Expr<'a>,
    body: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let header_block = function_sender.builder.create_ebb();
    let exit_block = function_sender.builder.create_ebb();
    function_sender.builder.ins().jump(header_block, &[]);
    function_sender.builder.switch_to_block(header_block);

    let (expr_type, expr_value) = send_expression(cond, sender, vars, function_sender)?;
    Boolean.check(expr_type)?;

    function_sender
        .builder
        .ins()
        .brz(expr_value, exit_block, &[]);

    for statement in body {
        send_statement(statement, return_type, sender, vars, function_sender)?;
    }
    function_sender.builder.ins().jump(header_block, &[]);

    function_sender.builder.switch_to_block(exit_block);

    // We've reached the bottom of the loop, so there will be no
    // more backedges to the header to exits to the bottom.
    function_sender.builder.seal_block(header_block);
    function_sender.builder.seal_block(exit_block);

    Ok(())
}

fn send_do_while_statement<'a, B: Backend>(
    cond: &Expr<'a>,
    body: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    vars: &mut Callables<'a>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    let header_block = function_sender.builder.create_ebb();
    let exit_block = function_sender.builder.create_ebb();
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
    function_sender.builder.seal_block(header_block);
    function_sender.builder.seal_block(exit_block);

    Ok(())
}

fn send_call<B: Backend>(
    call: &Call,
    sender: &mut Sender<B>,
    vars: &Callables,
    function_sender: &mut FunctionSender,
) -> ReportResult<Option<(crate::types::Type, Value)>> {
    let Call(name, args) = call;
    match vars.get(name)? {
        Callable::Arg(type_, value) => {
            if args.is_empty() {
                Ok(Some((*type_, *value)))
            } else {
                Err(ReportError::TypeError(format!(
                    "Sorry, you can't call an argument: '{}'",
                    name
                )))
            }
        }
        Callable::Var(type_, var, _) => {
            if args.is_empty() {
                let value = function_sender.builder.use_var(*var);
                Ok(Some((*type_, value)))
            } else {
                Err(ReportError::TypeError(format!(
                    "Sorry, you can't call a variable: '{}'",
                    name
                )))
            }
        }
        Callable::Func(type_, arg_types, func_id) => {
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
        }
        Callable::Global(type_, id, _) => {
            if args.is_empty() {
                let local_id = sender
                    .module
                    .declare_data_in_func(*id, function_sender.builder.func);
                let ir_type = ir_type(sender.pointer_type, *type_);
                let addr = function_sender
                    .builder
                    .ins()
                    .global_value(sender.pointer_type, local_id);
                let value = function_sender
                    .builder
                    .ins()
                    .load(ir_type, MemFlags::new(), addr, 0);
                Ok(Some((*type_, value)))
            } else {
                Err(ReportError::TypeError(format!(
                    "Sorry, you can't call a variable: '{}'",
                    name
                )))
            }
        }
    }
}

fn send_return<B: Backend>(
    expr: &Expr,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
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

fn send_increment_statement<B: Backend>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_update_var_statement(var, vars, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fadd(value, one)
    })
}

fn send_decrement_statement<B: Backend>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> ReportResult<()> {
    send_update_var_statement(var, vars, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fsub(value, one)
    })
}

fn send_update_var_statement<'a, 'b, B: Backend>(
    var: &pst::Variable,
    vars: &Callables,
    sender: &mut Sender<B>,
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

fn send_expression<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
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
            let mut strings = vec![];
            for expr in exprs {
                let (expr_type, expr_value) = send_expression(expr, sender, vars, function_sender)?;
                let str_value = match expr_type {
                    Chars => expr_value,
                    Number => {
                        //TODO: do this without the extra allocation.
                        let slot = function_sender
                            .builder
                            .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 24));
                        let buff =
                            function_sender
                                .builder
                                .ins()
                                .stack_addr(sender.pointer_type, slot, 0);
                        let buff_size = function_sender
                            .builder
                            .ins()
                            .iconst(sender.pointer_type, 24);
                        float_to_string(
                            expr_value,
                            buff,
                            buff_size,
                            sender,
                            &mut function_sender.builder,
                        )?;
                        buff
                    }
                    Boolean => bool_to_string(expr_value, sender, &mut function_sender.builder)?,
                };

                strings.push(str_value);
            }

            let mut buff_size = function_sender.builder.ins().iconst(sender.pointer_type, 1);

            for string in &strings {
                // if source is null, throw an exception.
                let merge_block = function_sender.builder.create_ebb();
                function_sender
                    .builder
                    .ins()
                    .brnz(*string, merge_block, &[]);
                // TODO: print a useful error.
                function_sender.builder.ins().trap(TrapCode::User(1));
                function_sender.builder.switch_to_block(merge_block);
                function_sender.builder.seal_block(merge_block);

                let len = strlen(*string, sender, &mut function_sender.builder)?;
                buff_size = function_sender.builder.ins().iadd(buff_size, len);
            }

            let buff = alloc(buff_size, sender, &mut function_sender.builder)?;

            for string in &strings {
                concat_strings(
                    buff,
                    buff_size,
                    *string,
                    sender,
                    &mut function_sender.builder,
                )?;
            }

            (Chars, buff)
        }
        Expr::Lit(lit) => send_literal(lit, sender, function_sender)?,
        Expr::Call(call) => send_call(call, sender, vars, function_sender)?.ok_or_else(|| {
            ReportError::TypeError(format!(
                "You need to return something from '{}' if you want to use it",
                call.0
            ))
        })?,
    };

    Ok(result)
}

fn send_comparison<B: Backend>(
    type_: crate::types::Type,
    icc: IntCC,
    fcc: FloatCC,
    left_value: Value,
    right_value: Value,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    Ok((
        Boolean,
        match type_ {
            Chars => {
                // if source is null, throw an exception.
                let else_block = function_sender.builder.create_ebb();
                let merge_block = function_sender.builder.create_ebb();
                function_sender
                    .builder
                    .ins()
                    .brz(left_value, else_block, &[]);
                function_sender
                    .builder
                    .ins()
                    .brz(right_value, else_block, &[]);
                function_sender.builder.ins().jump(merge_block, &[]);

                function_sender.builder.switch_to_block(else_block);
                function_sender.builder.seal_block(else_block);

                // TODO: print a useful error.
                function_sender.builder.ins().trap(TrapCode::User(1));
                function_sender.builder.switch_to_block(merge_block);
                function_sender.builder.seal_block(merge_block);

                let result = compare_strings(
                    left_value,
                    right_value,
                    sender,
                    &mut function_sender.builder,
                )?;
                function_sender.builder.ins().icmp_imm(icc, result, 0)
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
        },
    ))
}

fn send_literal<'a, B: Backend>(
    lit: &Literal<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> ReportResult<(crate::types::Type, Value)> {
    let result = match lit {
        Literal::Chars(string) => {
            create_constant_string(string, sender)?;
            (
                crate::types::Type::Chars,
                reference_constant_string(string, sender, &mut function_sender.builder)?,
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

fn create_constant_string<B: Backend>(string: &str, sender: &mut Sender<B>) -> ReportResult<()> {
    if sender.constants.contains(string) {
        return Ok(());
    }

    // We need to append a null byte at the end for libc.
    let mut s: Vec<_> = string.bytes().collect();
    s.push(b'\0');
    sender.data_context.define(s.into_boxed_slice());
    let id = sender
        .module
        .declare_data(string, Linkage::Export, false, Option::None)?;

    sender.module.define_data(id, &sender.data_context)?;

    sender.data_context.clear();

    sender.constants.insert(string.to_owned());

    Ok(())
}

fn compare_strings<B: Backend>(
    left_value: Value,
    right_value: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(types::I32));

    let callee = sender
        .module
        .declare_function("strcmp", Linkage::Import, &sig)?;
    let local_callee = sender
        .module
        .declare_func_in_func(callee, &mut builder.func);
    let call = builder.ins().call(local_callee, &[left_value, right_value]);
    let result = builder.inst_results(call)[0];

    Ok(result)
}

fn float_to_string<B: Backend>(
    float_value: Value,
    buffer_value: Value,
    buffer_size: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    let result = if sender.is_windows() {
        // use _gcvt on windows as snprintf is inlined so there's no symbol to dynamically link to.
        sig.params.push(AbiParam::new(types::F64));
        sig.params.push(AbiParam::new(types::I32));
        sig.params.push(AbiParam::new(sender.pointer_type));
        sig.returns.push(AbiParam::new(sender.pointer_type));

        let callee = sender
            .module
            .declare_function("_gcvt", Linkage::Import, &sig)?;
        let local_callee = sender.module.declare_func_in_func(callee, builder.func);

        let two = builder.ins().iconst(types::I64, 2);
        let s = builder.ins().isub(buffer_size, two);
        let digits = builder.ins().ireduce(types::I32, s);
        let call = builder
            .ins()
            .call(local_callee, &[float_value, digits, buffer_value]);
        let result = builder.inst_results(call)[0];

        // _gcvt always inserts a decimal at the end, ex: 10 -> "10.", strip it if this happens.
        let len = strlen(result, sender, builder)?;
        let one = builder.ins().iconst(types::I64, 1);
        let last_char_index = builder.ins().isub(len, one);
        let last_char_ptr = builder.ins().iadd(result, last_char_index);
        let last_char = builder
            .ins()
            .load(types::I8, MemFlags::new(), last_char_ptr, 0);
        let cmp = builder.ins().icmp_imm(IntCC::Equal, last_char, b'.' as i64);

        let merge_block = builder.create_ebb();

        builder.ins().brz(cmp, merge_block, &[]);

        let zero = builder.ins().iconst(types::I8, 0);
        builder.ins().store(MemFlags::new(), zero, last_char_ptr, 0);

        builder.ins().jump(merge_block, &[]);

        builder.switch_to_block(merge_block);
        builder.seal_block(merge_block);

        result
    } else {
        sig.params.push(AbiParam::new(sender.pointer_type));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(sender.pointer_type));
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::I32));

        let callee = sender
            .module
            .declare_function("snprintf", Linkage::Import, &sig)?;
        let local_callee = sender.module.declare_func_in_func(callee, builder.func);

        let format = reference_constant_string("%g", sender, builder)?;
        let call = builder.ins().call(
            local_callee,
            &[buffer_value, buffer_size, format, float_value],
        );
        builder.inst_results(call)[0]
    };

    Ok(result)
}

fn bool_to_string<B: Backend>(
    bool_value: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let else_block = builder.create_ebb();
    let merge_block = builder.create_ebb();
    builder.append_ebb_param(merge_block, sender.pointer_type);

    builder.ins().brz(bool_value, else_block, &[]);
    let then_return = reference_constant_string("yes", sender, builder)?;
    builder.ins().jump(merge_block, &[then_return]);

    builder.switch_to_block(else_block);
    builder.seal_block(else_block);
    let else_return = reference_constant_string("no", sender, builder)?;
    builder.ins().jump(merge_block, &[else_return]);

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);

    Ok(builder.ebb_params(merge_block)[0])
}

fn reference_constant_string<B: Backend>(
    string: &str,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let sym = sender
        .module
        .declare_data(string, Linkage::Export, false, Option::None)?;
    let local_id = sender.module.declare_data_in_func(sym, builder.func);

    Ok(builder
        .ins()
        .symbol_value(sender.module.target_config().pointer_type(), local_id))
}

fn concat_strings<B: Backend>(
    dest: Value,
    dest_size: Value,
    source: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(sender.pointer_type));

    let callee = sender
        .module
        .declare_function("strncat", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let call = builder.ins().call(local_callee, &[dest, source, dest_size]);

    Ok(builder.inst_results(call)[0])
}

fn alloc<B: Backend>(
    size: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(sender.pointer_type));

    let nmemb = builder.ins().iconst(sender.pointer_type, 1);
    let callee = sender
        .module
        .declare_function("calloc", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let call = builder.ins().call(local_callee, &[nmemb, size]);

    Ok(builder.inst_results(call)[0])
}

fn strlen<B: Backend>(
    string: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(types::I64));

    let callee = sender
        .module
        .declare_function("strlen", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let call = builder.ins().call(local_callee, &[string]);

    Ok(builder.inst_results(call)[0])
}

fn get_line<B: Backend>(
    line_ptr: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(types::I64));

    let callee = sender
        .module
        .declare_function("getline", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let zero_slot = builder.create_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        types::I32.bytes(),
    ));
    let zero_ptr = builder.ins().stack_addr(sender.pointer_type, zero_slot, 0);
    let zero = builder.ins().iconst(sender.pointer_type, 0);
    builder.ins().stack_store(zero, zero_slot, 0);

    let stdin_name = match sender.triple.operating_system {
        OperatingSystem::Darwin { .. }  => "__stdinp",
        _ => "stdin"
    };

    let sym = sender
        .module
        .declare_data(stdin_name, Linkage::Import, false, None)?;
    let local_id = sender.module.declare_data_in_func(sym, builder.func);
    let stdin_ptr = builder
        .ins()
        .symbol_value(sender.module.target_config().pointer_type(), local_id);
    let stdin = builder.ins().load(sender.pointer_type, MemFlags::new(), stdin_ptr, 0);

    let call = builder
        .ins()
        .call(local_callee, &[line_ptr, zero_ptr, stdin]);

    Ok(builder.inst_results(call)[0])
}

fn string_to_double<B: Backend>(string: Value, sender: &mut Sender<B>, builder: &mut FunctionBuilder) -> ReportResult<Value> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(types::F64));

    let callee = sender
        .module
        .declare_function("strtod", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let zero = builder.ins().iconst(sender.pointer_type, 0);
    let call = builder
        .ins()
        .call(local_callee, &[string, zero]);

    Ok(builder.inst_results(call)[0])
}

struct Sender<B: Backend> {
    triple: Triple,
    module: Module<B>,
    data_context: DataContext,
    constants: HashSet<String>,
    pointer_type: Type,
}

impl<B: Backend> Sender<B> {
    fn new(module: Module<B>, triple: Triple) -> Sender<B> {
        let pointer_type = module.target_config().pointer_type();
        Sender {
            triple,
            module,
            data_context: DataContext::new(),
            constants: HashSet::new(),
            pointer_type,
        }
    }

    fn is_windows(&self) -> bool {
        self.triple.operating_system == OperatingSystem::Windows
    }

    fn finalize(&mut self, context: &mut Context) {
        self.module.clear_context(context);
        self.module.finalize_definitions();
    }
}

struct FunctionSender<'b> {
    builder: FunctionBuilder<'b>,
}

impl<'b> FunctionSender<'b> {
    fn new(builder: FunctionBuilder<'b>) -> FunctionSender<'b> {
        FunctionSender { builder }
    }
}

fn ir_type(pointer_type: Type, type_: crate::types::Type) -> Type {
    match type_ {
        Chars => pointer_type,
        Number => types::F64,
        // There are numerous bugs with b1, use i32 instead.
        // https://github.com/bytecodealliance/cranelift/issues/1117
        Boolean => types::I32,
    }
}

use std::fs::File;

use std::mem;
use std::str::FromStr;

use cranelift::codegen::Context;
use cranelift::prelude::settings::{self, Configurable};
use cranelift::prelude::*;
use cranelift::prelude::{isa, AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use cranelift_module::{default_libcall_names, Backend, DataContext, FuncId, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use target_lexicon::Triple;

use crate::error::ReportError;
use crate::pst;
use crate::pst::{BinOperator, Call, Expr, Literal, Paragraph, Report, Statement};
use crate::types::Type::{Boolean, Chars, Number};
use crate::vars::{Callable, Callables};
use cranelift::codegen::ir::StackSlot;
use std::collections::HashSet;

pub fn send_out<'a>(report: &'a Report, name: &str, target: &str) -> Result<(), ReportError> {
    let mut sender = faerie_sender(name, target)?;
    let mut globals = Callables::new();
    let mut context = sender.module.make_context();
    send(report, &mut sender, &mut globals, &mut context, |_, _| {})?;
    sender.finalize(&mut context);
    let product = sender.module.finish();
    let file = File::create(name.to_owned() + ".o").expect("error opening file");
    product.write(file).expect("error writing to file");

    Ok(())
}

pub fn gallop<'a>(report: &'a Report, _name: &str) -> Result<(), ReportError> {
    let mut sender = simple_jit_sender();
    let mut globals = Callables::new();
    let mut context = sender.module.make_context();
    let id = send(report, &mut sender, &mut globals, &mut context, |_, _| {})?;
    sender.finalize(&mut context);
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> (isize)>(code) };

    code();

    Ok(())
}

pub fn gallop_paragraph<R>(paragraph: &Paragraph) -> Result<R, ReportError> {
    let mut sender = simple_jit_sender();
    let mut globals = Callables::new();
    let mut context = sender.module.make_context();
    let id = declare_paragraph(paragraph, &mut sender)?;
    send_paragraph(
        paragraph,
        id,
        &mut sender,
        &mut globals,
        &mut context,
        &mut |_, _| {},
    )?;
    sender.finalize(&mut context);
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> R>(code) };

    Ok(code())
}

pub fn proofread<'a>(report: &'a Report<'a>) -> Result<(), ReportError> {
    let mut sender = simple_jit_sender();
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
            );
        },
    )?;

    print!("{}", buff);

    Ok(())
}

fn faerie_sender(name: &str, target: &str) -> Result<Sender<FaerieBackend>, ReportError> {
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();
    let isa_builder = isa::lookup(Triple::from_str(target)?)?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let builder = FaerieBuilder::new(
        isa,
        name.to_owned(),
        FaerieTrapCollection::Disabled,
        default_libcall_names(),
    )
    .unwrap();
    let module = Module::<FaerieBackend>::new(builder);

    Ok(Sender::new(module))
}

fn simple_jit_sender() -> Sender<SimpleJITBackend> {
    let builder = SimpleJITBuilder::new(default_libcall_names());
    let module = Module::<SimpleJITBackend>::new(builder);

    Sender::new(module)
}

fn send<'a, B: Backend>(
    report: &'a Report,
    sender: &'a mut Sender<B>,
    globals: &'a mut Callables<'a>,
    context: &mut Context,
    mut f: impl FnMut(&Sender<B>, &Context) -> (),
) -> Result<FuncId, ReportError> {
    // constant strings for printing booleans
    create_constant_string("yes", sender)?;
    create_constant_string("no", sender)?;
    // constant string for formatting numbers
    create_constant_string("%g", sender)?;

    let mut mane_paragraphs: Vec<FuncId> = vec![];
    let mut paragraphs: Vec<(FuncId, &Paragraph)> = vec![];

    for paragraph in &report.paragraphs {
        let id = declare_paragraph(paragraph, sender)?;
        globals.insert(paragraph.name, Callable::Func(paragraph.return_type, id));
        paragraphs.push((id, paragraph));
        if paragraph.mane {
            mane_paragraphs.push(id);
        }
    }

    for (id, paragraph) in paragraphs {
        send_paragraph(paragraph, id, sender, globals, context, &mut f)?;
    }

    let id = send_mane(mane_paragraphs, sender, globals, context, &mut f)?;

    Ok(id)
}

fn send_mane<'a, B: Backend>(
    mane_paragraphs: Vec<FuncId>,
    sender: &'a mut Sender<B>,
    globals: &'a Callables<'a>,
    context: &mut Context,
    f: &mut impl FnMut(&Sender<B>, &Context) -> (),
) -> Result<FuncId, ReportError> {
    let mut builder_context = FunctionBuilderContext::new();
    let int = sender.pointer_type;
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(sender.pointer_type));

    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut function_sender = FunctionSender::new(globals, builder);

    let entry_ebb = function_sender.builder.create_ebb();
    function_sender
        .builder
        .append_ebb_params_for_function_params(entry_ebb);
    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

    for func_id in mane_paragraphs {
        let local_callee = sender
            .module
            .declare_func_in_func(func_id, function_sender.builder.func);
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

    f(sender, context);

    sender.module.clear_context(context);

    Ok(id)
}

fn declare_paragraph<'a, B: Backend>(
    paragraph: &'a Paragraph,
    sender: &mut Sender<B>,
) -> Result<FuncId, ReportError> {
    let mut sig = sender.module.make_signature();
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
    f: &mut impl FnMut(&Sender<B>, &Context) -> (),
) -> Result<(), ReportError> {
    let mut builder_context = FunctionBuilderContext::new();

    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut function_sender = FunctionSender::new(globals, builder);

    if let Some(return_type) = paragraph.return_type {
        function_sender
            .builder
            .func
            .signature
            .returns
            .push(AbiParam::new(ir_type(sender.pointer_type, return_type)));
    }

    let entry_ebb = function_sender.builder.create_ebb();
    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

    for statement in paragraph.statements.iter() {
        send_statement(
            statement,
            paragraph.return_type,
            sender,
            &mut function_sender,
        )?;
    }

    if paragraph.return_type.is_none() {
        function_sender.builder.ins().return_(&[]);
    }

    sender.module.define_function(func_id, context)?;

    f(sender, context);

    sender.module.clear_context(context);

    Ok(())
}

fn send_statement<'a, B: Backend>(
    statement: &'a Statement<'a>,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a, '_>,
) -> Result<(), ReportError> {
    match statement {
        Statement::Print(expr) => send_print_statement(expr, sender, function_sender),
        Statement::Declare(var, type_, expr, is_const) => {
            send_declare_statement(var, *type_, expr, *is_const, sender, function_sender)
        }
        Statement::Assign(var, expr) => send_assign_statement(var, expr, sender, function_sender),
        Statement::Increment(var) => send_increment_statement(var, function_sender),
        Statement::Decrement(var) => send_decrement_statement(var, function_sender),
        Statement::If(cond, if_, else_) => {
            send_if_else(cond, if_, else_, return_type, sender, function_sender)
        }
        Statement::While(cond, body) => {
            send_while_statement(cond, body, return_type, sender, function_sender)
        }
        Statement::Call(call) => {
            send_call(call, sender, function_sender)?;
            Ok(())
        }
        Statement::Return(expr) => send_return(expr, return_type, sender, function_sender),
    }
}

fn send_print_statement<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(), ReportError> {
    let (type_, value) = send_expression(expr, sender, function_sender)?;

    let value = match type_ {
        crate::types::Type::Chars => value,
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

    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    let callee = sender
        .module
        .declare_function("puts", Linkage::Import, &sig)?;
    let local_callee = sender
        .module
        .declare_func_in_func(callee, function_sender.builder.func);

    let _call = function_sender.builder.ins().call(local_callee, &[value]);

    Ok(())
}

fn send_declare_statement<'a, 'b, B: Backend>(
    var: &'a pst::Variable<'a>,
    type_: Option<crate::types::Type>,
    expr: &Option<Expr<'a>>,
    is_const: bool,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a, 'b>,
) -> Result<(), ReportError> {
    let (type_, value) = match (type_, expr) {
        (Some(type_), Some(expr)) => {
            let (expr_type, value) = send_expression(expr, sender, function_sender)?;
            // ensure type matches what's declared.
            (type_.check(expr_type)?, value)
        }
        (Some(type_), None) => match type_ {
            Chars => {
                let value = function_sender.builder.ins().null(sender.pointer_type);
                (Chars, value)
            }
            Number => send_number_literal(0f64, function_sender),
            Boolean => send_boolean_literal(false, function_sender),
        },
        (None, Some(expr)) => send_expression(expr, sender, function_sender)?,
        _ => {
            return Err(ReportError::TypeError(
                "must declare type or value".to_owned(),
            ))
        }
    };

    let slot_size = ir_type(sender.pointer_type, type_);

    let slot = function_sender
        .builder
        .create_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            slot_size.bytes(),
        ));

    function_sender.builder.ins().stack_store(value, slot, 0);

    function_sender
        .vars
        .insert(var.0, Callable::Var(type_, slot, is_const));

    Ok(())
}

fn send_assign_statement<'a, B: Backend>(
    var: &pst::Variable<'a>,
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(), ReportError> {
    let (expr_type, expr_value) = send_expression(expr, sender, function_sender)?;
    let (type_, slot, is_const) = match function_sender.vars.get(var.0)? {
        Callable::Var(type_, slot, is_const) => (type_, slot, is_const),
        Callable::Func(_, _) => {
            return Err(ReportError::TypeError(format!(
                "Sorry, you can't assign to a paragraph '{}'",
                var.0
            )))
        }
    };

    if *is_const {
        return Err(ReportError::TypeError(format!(
            "Nopony can change what is always true: '{}'",
            var.0
        )));
    }

    type_.check(expr_type)?;
    function_sender
        .builder
        .ins()
        .stack_store(expr_value, *slot, 0);

    Ok(())
}

fn send_if_else<'a, B: Backend>(
    cond: &Expr<'a>,
    if_: &'a [Statement<'a>],
    else_: &'a [Statement<'a>],
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a, '_>,
) -> Result<(), ReportError> {
    let (expr_type, expr_value) = send_expression(cond, sender, function_sender)?;
    Boolean.check(expr_type)?;

    let else_block = function_sender.builder.create_ebb();
    let merge_block = function_sender.builder.create_ebb();
    function_sender
        .builder
        .ins()
        .brz(expr_value, else_block, &[]);

    for statement in if_ {
        send_statement(statement, return_type, sender, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(else_block);
    function_sender.builder.seal_block(else_block);

    for statement in else_ {
        send_statement(statement, return_type, sender, function_sender)?;
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
    function_sender: &mut FunctionSender<'a, '_>,
) -> Result<(), ReportError> {
    let header_block = function_sender.builder.create_ebb();
    let exit_block = function_sender.builder.create_ebb();
    function_sender.builder.ins().jump(header_block, &[]);
    function_sender.builder.switch_to_block(header_block);

    let (expr_type, expr_value) = send_expression(cond, sender, function_sender)?;
    Boolean.check(expr_type)?;

    function_sender
        .builder
        .ins()
        .brz(expr_value, exit_block, &[]);

    for statement in body {
        send_statement(statement, return_type, sender, function_sender)?;
    }
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
    function_sender: &mut FunctionSender,
) -> Result<Option<(crate::types::Type, Value)>, ReportError> {
    let Call(name, args) = call;
    let (type_, func_id) = match function_sender.vars.get(name)? {
        Callable::Var(type_, slot, _) => {
            if args.is_empty() {
                return Ok(Some(send_var(*type_, *slot, sender, function_sender)));
            } else {
                return Err(ReportError::TypeError(format!(
                    "Sorry, you can't call a variable: '{}'",
                    name
                )));
            }
        }
        Callable::Func(type_, func_id) => (type_, func_id),
    };

    let local_callee = sender
        .module
        .declare_func_in_func(*func_id, &mut function_sender.builder.func);
    let result = function_sender.builder.ins().call(local_callee, &[]);

    match type_ {
        None => Ok(None),
        Some(t) => {
            let v = function_sender.builder.inst_results(result)[0];
            Ok(Some((*t, v)))
        }
    }
}

fn send_return<B: Backend>(
    expr: &Expr,
    return_type: Option<crate::types::Type>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(), ReportError> {
    let return_type = return_type.ok_or_else(|| {
        ReportError::TypeError("You need to declare the type you are returning".to_string())
    })?;

    let (expr_type, expr_value) = send_expression(expr, sender, function_sender)?;
    return_type.check(expr_type)?;

    function_sender.builder.ins().return_(&[expr_value]);

    Ok(())
}

fn send_increment_statement(
    var: &pst::Variable,
    function_sender: &mut FunctionSender,
) -> Result<(), ReportError> {
    send_update_statement(var, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fadd(value, one)
    })
}

fn send_decrement_statement(
    var: &pst::Variable,
    function_sender: &mut FunctionSender,
) -> Result<(), ReportError> {
    send_update_statement(var, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fsub(value, one)
    })
}

fn send_update_statement<'a, 'b>(
    var: &pst::Variable,
    function_sender: &mut FunctionSender<'a, 'b>,
    f: impl FnOnce(&mut FunctionBuilder<'b>, Value) -> Value,
) -> Result<(), ReportError> {
    let (type_, slot, is_const) = match function_sender.vars.get(var.0)? {
        Callable::Var(type_, slot, is_const) => (type_, slot, is_const),
        Callable::Func(_, _) => {
            return Err(ReportError::TypeError(format!(
                "Sorry, you can't assign to a paragraph '{}'",
                var.0
            )))
        }
    };
    Number.check(*type_)?;

    if *is_const {
        return Err(ReportError::TypeError(format!(
            "Nopony can change what is always true: '{}'",
            var.0
        )));
    }

    let value = function_sender
        .builder
        .ins()
        .stack_load(types::F64, *slot, 0);

    let new_value = f(&mut function_sender.builder, value);
    function_sender
        .builder
        .ins()
        .stack_store(new_value, *slot, 0);

    Ok(())
}

fn send_expression<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(crate::types::Type, Value), ReportError> {
    let result = match expr {
        Expr::BinOp(op, left, right) => {
            let (left_type, left_value) = send_expression(left, sender, function_sender)?;
            let (right_type, right_value) = send_expression(right, sender, function_sender)?;

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
            let (type_, value) = send_expression(expr, sender, function_sender)?;

            Boolean.check(type_)?;
            let b = function_sender
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, value, 0);
            (type_, function_sender.builder.ins().bint(types::I32, b))
        }
        Expr::Concat(exprs) => {
            //TODO: figure out how to allocate the buffer to the correct size.
            let slot = function_sender
                .builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 255));
            let buff = function_sender
                .builder
                .ins()
                .stack_addr(sender.pointer_type, slot, 0);

            // Write 0 so that strcat starts at the beginning of the buffer.
            let zero = function_sender.builder.ins().iconst(types::I32, 0);
            function_sender.builder.ins().stack_store(zero, slot, 0);

            let buff_size = function_sender.builder.ins().iconst(types::I64, 255);

            for expr in exprs {
                let (expr_type, expr_value) = send_expression(expr, sender, function_sender)?;
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
                        let buff_size = function_sender.builder.ins().iconst(types::I64, 24);
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

                concat_strings(
                    buff,
                    buff_size,
                    str_value,
                    sender,
                    &mut function_sender.builder,
                )?;
            }

            (Chars, buff)
        }
        Expr::Lit(lit) => send_literal(lit, sender, function_sender)?,
        Expr::Call(call) => send_call(call, sender, function_sender)?.ok_or_else(|| {
            ReportError::TypeError(format!(
                "You need to return something from '{}' if you want to use it",
                call.0
            ))
        })?,
    };

    Ok(result)
}

fn send_comparison<'a, B: Backend>(
    type_: crate::types::Type,
    icc: IntCC,
    fcc: FloatCC,
    left_value: Value,
    right_value: Value,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(crate::types::Type, Value), ReportError> {
    Ok((
        Boolean,
        match type_ {
            Chars => {
                let result = compare_strings(
                    left_value,
                    right_value,
                    sender,
                    &mut function_sender.builder,
                )?;
                function_sender.builder.ins().icmp_imm(icc, result, 0)
            }
            Number => function_sender
                .builder
                .ins()
                .fcmp(fcc, left_value, right_value),
            Boolean => function_sender
                .builder
                .ins()
                .icmp(icc, left_value, right_value),
        },
    ))
}

fn send_var<B: Backend>(
    type_: crate::types::Type,
    slot: StackSlot,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> (crate::types::Type, Value) {
    let v = function_sender
        .builder
        .ins()
        .stack_load(ir_type(sender.pointer_type, type_), slot, 0);
    (type_, v)
}

fn send_literal<'a, B: Backend>(
    lit: &Literal<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender,
) -> Result<(crate::types::Type, Value), ReportError> {
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

fn create_constant_string<B: Backend>(
    string: &str,
    sender: &mut Sender<B>,
) -> Result<(), ReportError> {
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
) -> Result<Value, ReportError> {
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
) -> Result<Value, ReportError> {
    let mut sig = sender.module.make_signature();
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

    Ok(builder.inst_results(call)[0])
}

fn bool_to_string<B: Backend>(
    bool_value: Value,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<Value, ReportError> {
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
) -> Result<Value, ReportError> {
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
) -> Result<Value, ReportError> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(sender.pointer_type));

    let callee = sender
        .module
        .declare_function("strncat", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let call = builder.ins().call(local_callee, &[dest, source, dest_size]);

    Ok(builder.inst_results(call)[0])
}

struct Sender<B: Backend> {
    module: Module<B>,
    data_context: DataContext,
    constants: HashSet<String>,
    pointer_type: Type,
}

impl<B: Backend> Sender<B> {
    fn new(module: Module<B>) -> Sender<B> {
        let pointer_type = module.target_config().pointer_type();
        Sender {
            module,
            data_context: DataContext::new(),
            constants: HashSet::new(),
            pointer_type,
        }
    }

    fn finalize(&mut self, context: &mut Context) {
        self.module.clear_context(context);
        self.module.finalize_definitions();
    }
}

struct FunctionSender<'a, 'b> {
    vars: Callables<'a>,
    builder: FunctionBuilder<'b>,
}

impl<'a, 'b> FunctionSender<'a, 'b> {
    fn new(globals: &'a Callables<'a>, builder: FunctionBuilder<'b>) -> FunctionSender<'a, 'b> {
        FunctionSender {
            builder,
            vars: globals.create_child(),
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
    }
}

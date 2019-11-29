use std::fs::{read_to_string, File};

use std::mem;
use std::str::FromStr;

use cranelift::codegen::Context;
use cranelift::prelude::settings::{self, Configurable};
use cranelift::prelude::*;
use cranelift::prelude::{isa, AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use cranelift_module::{
    default_libcall_names, Backend, DataContext, FuncId, Linkage, Module, ModuleError,
};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use target_lexicon::Triple;

use crate::error::ReportError;
use crate::pst;
use crate::pst::{BinOperator, Expr, Literal, Paragraph, Report, Statement};
use crate::types::Type::{Boolean, Chars, Number};
use cranelift::codegen::ir::StackSlot;
use cranelift::prelude::settings::detail::Detail::Bool;
use std::collections::{HashMap, HashSet};

pub fn send_out<'a>(report: &'a Report, name: &str, target: &str) -> Result<(), ReportError> {
    let mut sender = faerie_sender(name, target)?;
    let mut context = sender.module.make_context();
    send(report, &mut sender, &mut context)?;
    sender.finalize(&mut context);
    let product = sender.module.finish();
    let file = File::create(name.to_owned() + ".o").expect("error opening file");
    product.write(file).expect("error writing to file");

    Ok(())
}

pub fn gallop<'a>(report: &'a Report, _name: &str) -> Result<(), ReportError> {
    let mut sender = simple_jit_sender();
    let mut context = sender.module.make_context();
    let id = send(report, &mut sender, &mut context)?;
    sender.finalize(&mut context);
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> (isize)>(code) };

    code();

    Ok(())
}

pub fn proofread<'a>(report: &'a Report<'a>) -> Result<(), ReportError> {
    let mut sender = simple_jit_sender();
    let mut context = sender.module.make_context();

    let id = send(&report, &mut sender, &mut context)?;

    let mut buff = String::new();

    cranelift::codegen::write_function(
        &mut buff,
        &context.func,
        &Some(sender.module.isa()).into(),
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
    sender: &mut Sender<B>,
    context: &mut Context,
) -> Result<FuncId, ReportError> {
    let mut builder_context = FunctionBuilderContext::new();
    let int = sender.module.target_config().pointer_type();
    context.func.signature.returns.push(AbiParam::new(int));

    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut function_sender = FunctionSender::new(builder);

    let entry_ebb = function_sender.builder.create_ebb();
    function_sender
        .builder
        .append_ebb_params_for_function_params(entry_ebb);
    function_sender.builder.switch_to_block(entry_ebb);
    function_sender.builder.seal_block(entry_ebb);

    // constant strings for printing booleans
    create_constant_string("yes", sender)?;
    create_constant_string("no", sender)?;

    if !report.paragraphs.is_empty() {
        send_paragraph(&report.paragraphs[0], sender, &mut function_sender)?;
    }

    let zero = function_sender.builder.ins().iconst(int, 0);
    let var = cranelift::prelude::Variable::new(0);
    function_sender.builder.declare_var(var, int);
    function_sender.builder.def_var(var, zero);
    let return_value = function_sender.builder.use_var(var);
    function_sender.builder.ins().return_(&[return_value]);

    let id = sender
        .module
        .declare_function("main", Linkage::Export, &context.func.signature)?;
    sender.module.define_function(id, context)?;

    Ok(id)
}

fn send_paragraph<'a, B: Backend>(
    paragraph: &'a Paragraph,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(), ReportError> {
    for statement in paragraph.statements.iter() {
        send_statement(statement, sender, function_sender)?;
    }

    Ok(())
}

fn send_statement<'a, B: Backend>(
    statement: &'a Statement<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(), ReportError> {
    match statement {
        Statement::Print(expr) => send_print_statement(expr, sender, function_sender),
        Statement::Declare(var, type_, expr, is_const) => {
            send_declare_statement(var, *type_, expr, *is_const, sender, function_sender)
        }
        Statement::Assign(var, expr) => send_assign_statement(var, expr, sender, function_sender),
        Statement::If(cond, if_, else_) => send_if_else(cond, if_, else_, sender, function_sender),
        Statement::Increment(var) => send_increment_statement(var, sender, function_sender),
        Statement::Decrement(var) => send_decrement_statement(var, sender, function_sender),
    }
}

fn send_print_statement<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
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
            float_to_string(value, buff, sender, &mut function_sender.builder)?
        }
        crate::types::Type::Boolean => {
            let else_block = function_sender.builder.create_ebb();
            let merge_block = function_sender.builder.create_ebb();
            function_sender
                .builder
                .append_ebb_param(merge_block, sender.pointer_type);

            function_sender.builder.ins().brz(value, else_block, &[]);
            let then_return =
                reference_constant_string("yes", sender, &mut function_sender.builder)?;
            function_sender
                .builder
                .ins()
                .jump(merge_block, &[then_return]);

            function_sender.builder.switch_to_block(else_block);
            function_sender.builder.seal_block(else_block);
            let else_return =
                reference_constant_string("no", sender, &mut function_sender.builder)?;
            function_sender
                .builder
                .ins()
                .jump(merge_block, &[else_return]);

            function_sender.builder.switch_to_block(merge_block);
            function_sender.builder.seal_block(merge_block);

            function_sender.builder.ebb_params(merge_block)[0]
        }
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

fn send_declare_statement<'a, B: Backend>(
    var: &'a pst::Variable<'a>,
    type_: Option<crate::types::Type>,
    expr: &Option<Expr<'a>>,
    is_const: bool,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
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

    let slot_size = ir_type(sender, type_);

    let slot = function_sender
        .builder
        .create_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            slot_size.bytes(),
        ));

    function_sender.builder.ins().stack_store(value, slot, 0);

    function_sender.vars.insert(
        var,
        VarData {
            type_,
            is_const,
            slot,
        },
    );

    Ok(())
}

fn send_assign_statement<'a, B: Backend>(
    var: &pst::Variable<'a>,
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(), ReportError> {
    let (expr_type, expr_value) = send_expression(expr, sender, function_sender)?;
    let VarData {
        type_,
        is_const,
        slot,
    } = function_sender.vars[var];

    if is_const {
        return Err(ReportError::TypeError(format!(
            "Nopony can change what is always true: {}",
            var.0
        )));
    }

    type_.check(expr_type)?;
    function_sender
        .builder
        .ins()
        .stack_store(expr_value, slot, 0);

    Ok(())
}

fn send_if_else<'a, B: Backend>(
    cond: &Expr<'a>,
    if_: &'a [Statement<'a>],
    else_: &'a [Statement<'a>],
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
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
        send_statement(statement, sender, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(else_block);
    function_sender.builder.seal_block(else_block);

    for statement in else_ {
        send_statement(statement, sender, function_sender)?;
    }

    function_sender.builder.ins().jump(merge_block, &[]);

    function_sender.builder.switch_to_block(merge_block);
    function_sender.builder.seal_block(merge_block);

    Ok(())
}

fn send_increment_statement<'a, B: Backend>(
    var: &pst::Variable,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(), ReportError> {
    send_update_statement(var, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fadd(value, one)
    })
}

fn send_decrement_statement<'a, B: Backend>(
    var: &pst::Variable,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(), ReportError> {
    send_update_statement(var, sender, function_sender, |builder, value| {
        let one = builder.ins().f64const(1f64);
        builder.ins().fsub(value, one)
    })
}

fn send_update_statement<'a, B: Backend>(var: &pst::Variable, sender:&mut Sender<B>, function_sender: &mut FunctionSender<'a>, f: impl FnOnce(&mut FunctionBuilder<'a>, Value) -> Value) -> Result<(), ReportError> {
    let var_data = &function_sender
        .vars
        .get(var)
        .unwrap_or_else(|| panic!("I didn't know '{}'", var.0));
    Number.check(var_data.type_)?;
    let value = function_sender
        .builder
        .ins()
        .stack_load(types::F64, var_data.slot, 0);

    let new_value = f(&mut function_sender.builder, value);
    function_sender
        .builder
        .ins()
        .stack_store(new_value, var_data.slot, 0);

    Ok(())
}

fn send_expression<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
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
        Expr::Not(val) => {
            let (type_, value) = send_value(val, sender, function_sender)?;

            Boolean.check(type_)?;
            let b = function_sender
                .builder
                .ins()
                .icmp_imm(IntCC::Equal, value, 0);
            (type_, function_sender.builder.ins().bint(types::I32, b))
        }
        Expr::Val(val) => send_value(val, sender, function_sender)?,
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
    function_sender: &mut FunctionSender<'a>,
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

fn send_value<'a, B: Backend>(
    value: &pst::Value<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
) -> Result<(crate::types::Type, Value), ReportError> {
    let result = match value {
        pst::Value::Lit(lit) => send_literal(lit, sender, function_sender)?,
        pst::Value::Var(var) => {
            let var_data = &function_sender
                .vars
                .get(var)
                .unwrap_or_else(|| panic!("I didn't know '{}'", var.0));
            let v = function_sender.builder.ins().stack_load(
                ir_type(sender, var_data.type_),
                var_data.slot,
                0,
            );
            (var_data.type_, v)
        }
    };

    Ok(result)
}

fn send_literal<'a, B: Backend>(
    lit: &Literal<'a>,
    sender: &mut Sender<B>,
    function_sender: &mut FunctionSender<'a>,
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
    sender.module.finalize_definitions();

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
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<Value, ReportError> {
    let mut sig = sender.module.make_signature();
    sig.params.push(AbiParam::new(types::F64));
    sig.params.push(AbiParam::new(types::I32));
    sig.params.push(AbiParam::new(sender.pointer_type));
    sig.returns.push(AbiParam::new(sender.pointer_type));

    let callee = sender
        .module
        .declare_function("gcvt", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let n_digits = builder.ins().iconst(types::I32, 9);
    let call = builder
        .ins()
        .call(local_callee, &[float_value, n_digits, buffer_value]);

    Ok(builder.inst_results(call)[0])
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

struct FunctionSender<'a> {
    builder: FunctionBuilder<'a>,
    vars: HashMap<&'a pst::Variable<'a>, VarData>,
}

impl<'a> FunctionSender<'a> {
    fn new(builder: FunctionBuilder<'a>) -> FunctionSender<'a> {
        FunctionSender {
            builder,
            vars: HashMap::new(),
        }
    }
}

struct VarData {
    type_: crate::types::Type,
    is_const: bool,
    slot: StackSlot,
}

fn ir_type<B: Backend>(sender: &Sender<B>, type_: crate::types::Type) -> Type {
    match type_ {
        Chars => sender.pointer_type,
        Number => types::F64,
        // There are numerous bugs with b1, use i32 instead.
        // https://github.com/bytecodealliance/cranelift/issues/1117
        Boolean => types::I32,
    }
}

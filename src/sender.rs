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
use crate::pst::{Expr, Literal, Paragraph, Report, NBinOperator};

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
    let entry_ebb = builder.create_ebb();
    builder.append_ebb_params_for_function_params(entry_ebb);
    builder.switch_to_block(entry_ebb);
    builder.seal_block(entry_ebb);

    // constant strings for printing booleans
    create_constant_string("yes", sender)?;
    create_constant_string("no", sender)?;

    if !report.paragraphs.is_empty() {
        send_paragraph(&report.paragraphs[0], sender, &mut builder)?;
    }

    let zero = builder.ins().iconst(int, 0);
    let var = Variable::new(0);
    builder.declare_var(var, int);
    builder.def_var(var, zero);
    let return_value = builder.use_var(var);
    builder.ins().return_(&[return_value]);

    let id = sender
        .module
        .declare_function("main", Linkage::Export, &context.func.signature)?;
    sender.module.define_function(id, context)?;

    Ok(id)
}

fn send_paragraph<'a, B: Backend>(
    paragraph: &'a Paragraph,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(), ReportError> {
    for statement in paragraph.statements.iter() {
        send_statement(statement, sender, builder)?;
    }

    Ok(())
}

fn send_statement<'a, B: Backend>(
    statement: &Expr<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(), ReportError> {
    let (type_, value) = send_expression(statement, sender, builder)?;

    let value = match type_ {
        crate::types::Type::String => value,
        crate::types::Type::Number => {
            // convert to string
            let slot = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 24));
            let buff = builder.ins().stack_addr(sender.pointer_type, slot, 0);
            float_to_string(value, buff, sender, builder)?
        }
        crate::types::Type::Boolean => {
            let else_block = builder.create_ebb();
            let merge_block = builder.create_ebb();
            builder.append_ebb_param(merge_block, sender.pointer_type);

            builder.ins().brz(value, else_block, &[]);
            let then_return = reference_constant_string("yes", sender, builder)?;
            builder.ins().jump(merge_block, &[then_return]);

            builder.switch_to_block(else_block);
            builder.seal_block(else_block);
            let else_return = reference_constant_string("no", sender, builder)?;
            builder.ins().jump(merge_block, &[else_return]);

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);

            builder.ebb_params(merge_block)[0]
        }
    };

    let mut sig = sender.module.make_signature();
    sig.params
        .push(AbiParam::new(sender.pointer_type));
    let callee = sender
        .module
        .declare_function("puts", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let _call = builder.ins().call(local_callee, &[value]);

    Ok(())
}

fn send_expression<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(crate::types::Type, Value), ReportError> {
    let result = match expr {
        Expr::NBinOp(op, left, right) => {
            let (left_type, left_value) = send_value(left, sender, builder)?;
            let (right_type, right_value) = send_value(right, sender, builder)?;

            if !left_type.is_number() || !right_type.is_number() {
                //TODO: more descriptive error
                return Err(ReportError::TypeError(
                    "expected number but got string".to_owned(),
                ));
            }

            (
                crate::types::Type::Number,
                match op {
                    NBinOperator::Add => builder.ins().fadd(left_value, right_value),
                    NBinOperator::Sub => builder.ins().fsub(left_value, right_value),
                    NBinOperator::Mul => builder.ins().fmul(left_value, right_value),
                    NBinOperator::Div => builder.ins().fdiv(left_value, right_value),
                }
            )
        }
        Expr::Val(val) => send_value(val, sender, builder)?,
    };

    Ok(result)
}

fn send_value<'a, B: Backend>(
    value: &pst::Value<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(crate::types::Type, Value), ReportError> {
    let result = match value {
        pst::Value::Lit(lit) => match lit {
            Literal::String(string) => {
                create_constant_string(string, sender)?;
                (
                    crate::types::Type::String,
                    reference_constant_string(string, sender, builder)?,
                )
            }
            Literal::Number(n) => (crate::types::Type::Number, builder.ins().f64const(*n)),
            Literal::Boolean(b) => (crate::types::Type::Boolean, builder.ins().bconst(types::B1, *b)),
        },
    };

    Ok(result)
}

fn create_constant_string<B: Backend>(
    string: &str,
    sender: &mut Sender<B>,
) -> Result<(), ReportError> {
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

    Ok(())
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
    pointer_type: Type,
}

impl<B: Backend> Sender<B> {
    fn new(module: Module<B>) -> Sender<B> {
        let pointer_type = module.target_config().pointer_type();
        Sender {
            module,
            data_context: DataContext::new(),
            pointer_type,
        }
    }

    fn finalize(&mut self, context: &mut Context) {
        self.module.clear_context(context);
        self.module.finalize_definitions();
    }
}

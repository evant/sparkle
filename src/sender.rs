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
use crate::pst::{Expr, Literal, Paragraph, Report};

pub fn send_out<'a>(report: &'a Report, name: &str) -> Result<(), ReportError<'a>> {
    let mut sender = faerie_sender(name)?;
    let mut context = sender.module.make_context();
    send(report, &mut sender, &mut context)?;
    sender.finalize(&mut context);
    let product = sender.module.finish();
    let file = File::create(name.to_owned() + ".o").expect("error opening file");
    product.write(file).expect("error writing to file");

    Ok(())
}

pub fn gallop<'a>(report: &'a Report, _name: &str) -> Result<(), ReportError<'a>> {
    let mut sender = simple_jit_sender();
    let mut context = sender.module.make_context();
    let id = send(report, &mut sender, &mut context)?;
    sender.finalize(&mut context);
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> (isize)>(code) };

    code();

    Ok(())
}

pub fn proofread<'a>(report: &'a Report<'a>) -> Result<(), ReportError<'a>> {
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

fn faerie_sender(name: &str) -> Result<Sender<FaerieBackend>, ReportError<'static>> {
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();
    let isa_builder = isa::lookup(Triple::from_str("x86_64-unknown-unknown-elf")?)?;
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
) -> Result<FuncId, ReportError<'a>> {
    let mut builder_context = FunctionBuilderContext::new();
    let int = sender.module.target_config().pointer_type();
    context.func.signature.returns.push(AbiParam::new(int));

    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let entry_ebb = builder.create_ebb();
    builder.append_ebb_params_for_function_params(entry_ebb);
    builder.switch_to_block(entry_ebb);
    builder.seal_block(entry_ebb);

    // constant strings for formatting with printf
    create_constant_string("%s\n", sender)?;
    create_constant_string("%f\n", sender)?;

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
) -> Result<(), ReportError<'a>> {
    for statement in paragraph.statements.iter() {
        send_statement(statement, sender, builder)?;
    }

    Ok(())
}

fn send_statement<'a, B: Backend>(
    statement: &Expr<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(), ReportError<'a>> {
    let (type_, value) = send_expression(statement, sender, builder)?;

    let (format, param_type) = match type_ {
        crate::types::Type::String => ("%s\n", sender.module.target_config().pointer_type()),
        crate::types::Type::Number => ("%f\n", types::F64),
    };

    let mut sig = sender.module.make_signature();
    sig.params
        .push(AbiParam::new(sender.module.target_config().pointer_type()));
    let callee = sender
        .module
        .declare_function("printf", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let format_value = reference_constant_string(format, sender, builder)?;
    let _call = builder.ins().call(local_callee, &[format_value, value]);

    // TODO: better way to support varargs?
    // https://github.com/bytecodealliance/cranelift/issues/212
    // https://github.com/bjorn3/rustc_codegen_cranelift/blob/1f8a646592d6e372b614cb94724deaea1753464b/src/abi/mod.rs#L531
    let sig_ref = builder.func.dfg.call_signature(_call).unwrap();
    builder.func.dfg.signatures[sig_ref]
        .params
        .push(AbiParam::new(param_type));

    Ok(())
}

fn send_expression<'a, B: Backend>(
    expr: &Expr<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(crate::types::Type, Value), ReportError<'a>> {
    let result = match expr {
        Expr::Add(left, right) => unreachable!(),
        Expr::Lit(lit) => match lit {
            Literal::String(string) => {
                create_constant_string(string, sender)?;
                (
                    crate::types::Type::String,
                    reference_constant_string(string, sender, builder)?,
                )
            }
            Literal::Number(n) => (crate::types::Type::Number, builder.ins().f64const(*n)),
        },
    };

    Ok(result)
}

fn create_constant_string<B: Backend>(
    string: &str,
    sender: &mut Sender<B>,
) -> Result<(), ReportError<'static>> {
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

fn reference_constant_string<B: Backend>(
    string: &str,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<Value, ReportError<'static>> {
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
}

impl<B: Backend> Sender<B> {
    fn new(module: Module<B>) -> Sender<B> {
        Sender {
            module,
            data_context: DataContext::new(),
        }
    }

    fn finalize(&mut self, context: &mut Context) {
        self.module.clear_context(context);
        self.module.finalize_definitions();
    }
}

use std::borrow::{Borrow, Cow};
use std::env::args;
use std::error::Error;
use std::fmt::Formatter;
use std::fs::{read_to_string, File};
use std::mem;
use std::path::Path;
use std::process::exit;
use std::str::FromStr;

use cranelift::prelude::settings::{self, Configurable};
use cranelift::prelude::*;
use cranelift::prelude::{isa, AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieTrapCollection};
use cranelift_module::{default_libcall_names, Backend, DataContext, FuncId, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use target_lexicon::Triple;

use crate::error::ReportError;
use crate::pst::{Literal, Paragraph, Report};

pub fn send_out<'a>(report: &'a Report, name: &str) -> Result<(), ReportError<'a>> {
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
    let data_context = DataContext::new();
    let mut sender = Sender {
        module,
        data_context,
    };
    send(report, &mut sender)?;
    let product = sender.module.finish();
    let file = File::create(name.to_owned() + ".o").expect("error opening file");
    product.write(file).expect("error writing to file");

    Ok(())
}

pub fn gallop<'a>(report: &'a Report, name: &str) -> Result<(), ReportError<'a>> {
    let builder = SimpleJITBuilder::new(default_libcall_names());
    let module = Module::<SimpleJITBackend>::new(builder);
    let data_context = DataContext::new();
    let mut sender = Sender {
        module,
        data_context,
    };
    let id = send(report, &mut sender)?;
    let code = sender.module.get_finalized_function(id);
    let code = unsafe { mem::transmute::<_, fn() -> (isize)>(code) };

    code();

    Ok(())
}

fn send<'a, B: Backend>(
    report: &'a Report,
    sender: &mut Sender<B>,
) -> Result<FuncId, ReportError<'a>> {
    let mut ctx = sender.module.make_context();
    let mut builder_context = FunctionBuilderContext::new();
    let int = sender.module.target_config().pointer_type();
    ctx.func.signature.returns.push(AbiParam::new(int));

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
    let entry_ebb = builder.create_ebb();
    builder.append_ebb_params_for_function_params(entry_ebb);
    builder.switch_to_block(entry_ebb);
    builder.seal_block(entry_ebb);

    // store newline for printing
    let newline: Vec<_> = "\n\0".bytes().collect();
    sender.data_context.define(newline.into_boxed_slice());
    let id = sender
        .module
        .declare_data("newline", Linkage::Export, false, Option::None)?;
    sender.module.define_data(id, &sender.data_context)?;
    sender.data_context.clear();
    sender.module.finalize_definitions();

    send_paragraph(&report.paragraphs[0], sender, &mut builder)?;

    let zero = builder.ins().iconst(int, 0);
    let var = Variable::new(0);
    builder.declare_var(var, int);
    builder.def_var(var, zero);
    let return_value = builder.use_var(var);
    builder.ins().return_(&[return_value]);

    let id = sender
        .module
        .declare_function("main", Linkage::Export, &ctx.func.signature)?;
    sender.module.define_function(id, &mut ctx)?;

    sender.module.clear_context(&mut ctx);
    sender.module.finalize_definitions();

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
    statement: &Literal<'a>,
    sender: &mut Sender<B>,
    builder: &mut FunctionBuilder,
) -> Result<(), ReportError<'a>> {
    let display = statement.to_cow_string();

    // We need to append a null byte at the end for libc.
    let mut s: Vec<_> = display.bytes().collect();
    s.push(b'\0');

    sender.data_context.define(s.into_boxed_slice());
    let id = sender
        .module
        .declare_data(&display, Linkage::Export, false, Option::None)?;
    sender.module.define_data(id, &sender.data_context)?;
    sender.data_context.clear();
    sender.module.finalize_definitions();

    let mut sig = sender.module.make_signature();
    sig.params
        .push(AbiParam::new(sender.module.target_config().pointer_type()));
    let callee = sender
        .module
        .declare_function("puts", Linkage::Import, &sig)?;
    let local_callee = sender.module.declare_func_in_func(callee, builder.func);

    let sym = sender
        .module
        .declare_data(&display, Linkage::Export, false, Option::None)?;
    let local_id = sender.module.declare_data_in_func(sym, builder.func);
    let var = builder
        .ins()
        .symbol_value(sender.module.target_config().pointer_type(), local_id);

    let call = builder.ins().call(local_callee, &[var]);

    Ok(())
}

struct Sender<B: Backend> {
    module: Module<B>,
    data_context: DataContext,
}

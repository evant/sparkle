use std::env::args;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::fs::{File, read_to_string};
use std::io::Write;
use std::iter::Product;
use std::path::Path;
use std::str::FromStr;

use cranelift::prelude::*;
use cranelift::prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, isa, Type};
use cranelift::prelude::isa::LookupError;
use cranelift::prelude::settings::{self, Configurable};
use cranelift_faerie::{FaerieBackend, FaerieBuilder, FaerieProduct, FaerieTrapCollection};
use cranelift_module::{DataContext, default_libcall_names, Linkage, Module, ModuleError};
use nom::{Compare, InputLength, InputTake, IResult};
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till, take_till1, take_until, take_while, take_while1};
use nom::character::complete::{anychar, space1};
use nom::character::is_space;
use nom::combinator::{map, map_res, not, peek, recognize};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{many0, many1, many_till, separated_list};
use nom::sequence::{delimited, delimitedc, pair, preceded, separated_pair, terminated, terminatedc, tuple};
use target_lexicon::Triple;

use crate::ReportError::{ReadError, SendError};

#[derive(Debug)]
enum ReportError<'a> {
    ReadError(nom::Err<(&'a str, ErrorKind)>),
    SendError(String),
}

impl fmt::Display for ReportError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl Error for ReportError<'_> {}

impl<'a> From<nom::Err<(&'a str, ErrorKind)>> for ReportError<'a> {
    fn from(e: nom::Err<(&'a str, ErrorKind)>) -> Self {
        ReadError(e)
    }
}

impl From<target_lexicon::ParseError> for ReportError<'_> {
    fn from(e: target_lexicon::ParseError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift::prelude::isa::LookupError> for ReportError<'_> {
    fn from(e: cranelift::prelude::isa::LookupError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift_module::ModuleError> for ReportError<'_> {
    fn from(e: cranelift_module::ModuleError) -> Self {
        SendError(e.to_string())
    }
}

fn main() {
    let arg = args().nth(1);
    match arg {
        Some(path) => {
            let p = Path::new(&path);
            if p.is_dir() {
                panic!("expected file but got dir");
            }
            let report = read_to_string(p).expect("error opening report");
            let name = p.file_stem().unwrap().to_str().unwrap().to_owned();
            let product = send(&read(&report).unwrap(), &name).unwrap();
            let file = File::create(name + ".o").expect("error opening file");
            product.write(file).expect("error writing to file");
        }
        None => {
            println!("{}", "usage: fimpp report.fpp")
        }
    }
}

fn read(report_text: &str) -> Result<Report, ReportError> {
    let (_, ast) = report(report_text)?;
    return Ok(ast);
}

fn send<'a>(report: &'a Report, name: &str) -> Result<FaerieProduct, ReportError<'a>> {
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();
    let isa_builder = isa::lookup(Triple::from_str("x86_64-unknown-unknown-elf")?)?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let builder = FaerieBuilder::new(
        isa,
        name.to_owned(),
        FaerieTrapCollection::Disabled,
        default_libcall_names(),
    ).unwrap();
    let mut module = Module::<FaerieBackend>::new(builder);
    let mut builder_context = FunctionBuilderContext::new();
    let mut ctx = module.make_context();
    let mut data_context = DataContext::new();

    let int = module.target_config().pointer_type();
    ctx.func.signature.returns.push(AbiParam::new(int));

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
    let entry_ebb = builder.create_ebb();
    builder.append_ebb_params_for_function_params(entry_ebb);
    builder.switch_to_block(entry_ebb);
    builder.seal_block(entry_ebb);

    send_paragraph(&report.paragraphs[0], &mut module, &mut data_context, &mut builder);

    let zero = builder.ins().iconst(int, 0);
    let var = Variable::new(0);
    builder.declare_var(var, int);
    builder.def_var(var, zero);
    let return_value = builder.use_var(var);
    builder.ins().return_(&[return_value]);

    let id = module.declare_function("main", Linkage::Export, &ctx.func.signature)?;
    module.define_function(id, &mut ctx)?;

    module.clear_context(&mut ctx);
    module.finalize_definitions();

    Ok(module.finish())
}

fn send_paragraph<'a>(paragraph: &'a Paragraph, module: &mut Module<FaerieBackend>, data_context: &mut DataContext, builder: &mut FunctionBuilder) -> Result<(), ReportError<'a>> {
    for statement in paragraph.statements.iter() {
        send_statement(*statement, module, data_context, builder)?;
    }

    Ok(())
}

fn send_statement<'a>(statement: &'a str, module: &mut Module<FaerieBackend>, data_context: &mut DataContext, builder: &mut FunctionBuilder) -> Result<(), ReportError<'a>> {
    // We need to append a null byte at the end for libc.
    let mut s: Vec<_> = statement.bytes().into_iter().collect();
    s.push('\0' as u8);

    data_context.define(s.into_boxed_slice());
    let id = module.declare_data("string", Linkage::Export, false, Option::None)?;
    module.define_data(id, data_context)?;
    data_context.clear();
    module.finalize_definitions();

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(module.target_config().pointer_type()));
    let callee = module.declare_function("puts", Linkage::Import, &sig)?;
    let local_callee = module.declare_func_in_func(callee, builder.func);

    let sym = module.declare_data("string", Linkage::Export, false, Option::None)?;
    let local_id = module.declare_data_in_func(sym, builder.func);
    let var = builder.ins().symbol_value(module.target_config().pointer_type(), local_id);

    let call = builder.ins().call(local_callee, &[var]);

    Ok(())
}

#[derive(Debug, Eq, PartialEq)]
struct Paragraph<'a> {
    name: &'a str,
    closing_name: &'a str,
    statements: Vec<&'a str>,
}

#[derive(Debug, Eq, PartialEq)]
struct Report<'a> {
    name: &'a str,
    paragraphs: Vec<Paragraph<'a>>,
    writer: &'a str,
}

fn is_keyword(s: &str) -> bool {
    match s {
        "Dear" => true,
        _ => false
    }
}

fn is_punctuation(s: char) -> bool {
    match s {
        '!' | ',' | '.' | ':' | '?' | '…' | '‽' => true,
        _ => false
    }
}

fn does(s: &str) -> IResult<&str, &str> {
    alt((tag("does"), tag("do"), tag("did")))(s)
}

fn print(s: &str) -> IResult<&str, &str> {
    alt((tag("I remembered"), tag("I said"), tag("I sang"), tag("I wrote")))(s)
}

fn identifier(s: &str) -> IResult<&str, &str> {
    recognize(separated_list(space1, word))(s)
}

fn word(s: &str) -> IResult<&str, &str> {
    recognize(many1(not_space_or_punctuation))(s)
}

fn whitespace0(s: &str) -> IResult<&str, &str> {
    recognize(many0(whitespace))(s)
}

fn whitespace(s: &str) -> IResult<&str, &str> {
    is_a(" \t\r\n")(s)
}

fn not_space_or_punctuation(s: &str) -> IResult<&str, &str> {
    is_not(" \t\r\n!,.:?…‽")(s)
}

fn punctuation(s: &str) -> IResult<&str, &str> {
    take_while(is_punctuation)(s)
}

fn report(s: &str) -> IResult<&str, Report> {
    map(tuple((
        terminated(report_declaration, whitespace0),
        terminated(many0(paragraph), whitespace0),
        report_closing
    )), |(declaration, paragraphs, closing)| Report {
        name: declaration,
        writer: closing,
        paragraphs,
    })(s)
}

fn report_declaration(s: &str) -> IResult<&str, &str> {
    terminated(preceded(preceded(tag("Dear Princes Celestia:"), whitespace0), identifier), punctuation)(s)
}

fn paragraph(s: &str) -> IResult<&str, Paragraph> {
    map(tuple((
        terminated(paragraph_declaration, whitespace0),
        terminated(many0(statement), whitespace0),
        paragraph_closing
    )), |(declaration, statements, closing)| Paragraph {
        name: declaration,
        closing_name: closing,
        statements,
    })(s)
}

fn paragraph_declaration(s: &str) -> IResult<&str, &str> {
    terminated(preceded(preceded(tag("Today I learned"), whitespace0), identifier), punctuation)(s)
}

fn statement(s: &str) -> IResult<&str, &str> {
    terminated(preceded(preceded(print, whitespace0), literal), punctuation)(s)
}

fn literal(s: &str) -> IResult<&str, &str> {
    delimited(is_a("\""), is_not("\""), is_a("\""))(s)
}

fn paragraph_closing(s: &str) -> IResult<&str, &str> {
    terminated(preceded(preceded(tag("That's all about"), whitespace0), identifier), punctuation)(s)
}

fn report_closing(s: &str) -> IResult<&str, &str> {
    terminated(preceded(preceded(tag("Your faithful student"), punctuation), take_till1(is_punctuation)), punctuation)(s)
}

#[test]
fn parses_does() {
    assert_eq!(does("does"), Ok(("", "does")));
    assert_eq!(does("not"), Err(nom::Err::Error(("not", ErrorKind::Tag))));
}

#[test]
fn parses_word() {
    assert_eq!(word("An"), Ok(("", "An")));
}

#[test]
fn parses_identifier() {
    assert_eq!(identifier("An example letter"), Ok(("", "An example letter")));
    assert_eq!(identifier("An example letter."), Ok((".", "An example letter")));
    assert_eq!(identifier("An example letter ."), Ok((" .", "An example letter")));
}

#[test]
fn parses_report_declaration() {
    assert_eq!(report_declaration("Dear Princes Celestia: An example letter."), Ok(("", "An example letter")));
}

#[test]
fn parses_report_closing() {
    assert_eq!(report_closing("Your faithful student: Twilight Sparkle."), Ok(("", " Twilight Sparkle")));
    assert_eq!(report_closing("Your faithful student, Applejack's hat!"), Ok(("", " Applejack's hat")));
}

#[test]
fn parses_paragraph_declaration() {
    assert_eq!(paragraph_declaration("Today I learned how to fly."), Ok(("", "how to fly")));
    assert_eq!(paragraph_declaration("Today I learned to say hello world:"), Ok(("", "to say hello world")));
}

#[test]
fn parses_paragraph_closing() {
    assert_eq!(paragraph_closing("That's all about how to fly."), Ok(("", "how to fly")));
    assert_eq!(paragraph_closing("That's all about to say hello world!"), Ok(("", "to say hello world")));
}

#[test]
fn parses_paragraph() {
    assert_eq!(paragraph("Today I learned how to fly.\
    I said \"Fly!\".\
    That's all about how to fly."), Ok(("", Paragraph {
        name: "how to fly",
        closing_name: "how to fly",
        statements: vec!["Fly!"],
    })))
}

#[test]
fn parses_report() {
    assert_eq!(report("Dear Princes Celestia: An example letter.\
        Today I learned how to fly:
            I said \"Fly!\"!
        That's all about how to fly!
    Your faithful student: Twilight Sparkle."), Ok(("", Report {
        name: "An example letter",
        paragraphs: vec![Paragraph {
            name: "how to fly",
            closing_name: "how to fly",
            statements: vec!["Fly!"],
        }],
        writer: " Twilight Sparkle",
    })));
}

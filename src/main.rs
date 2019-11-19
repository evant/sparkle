extern crate nom;

use nom::{Compare, InputLength, InputTake, IResult};
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till, take_till1, take_until, take_while, take_while1};
use nom::character::complete::{anychar, space1};
use nom::character::is_space;
use nom::combinator::{map, map_res, not, peek, recognize};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{many0, many1, many_till, separated_list};
use nom::sequence::{delimited, delimitedc, pair, preceded, separated_pair, terminated, terminatedc, tuple};

fn main() {
    println!("Hello, world!");
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

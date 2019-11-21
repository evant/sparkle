use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till1, take_while};
use nom::character::complete::space1;
use nom::combinator::{complete, map, opt, recognize};
use nom::error::{convert_error, VerboseError};
use nom::multi::{many0, many1, separated_list};
use nom::number::complete::double;
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::error::ReportError;
use crate::pst::{Expr, Literal, Paragraph, Report, Value};

type ReadResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

pub fn read(report_text: &str) -> Result<Report, ReportError> {
    let ast = match complete(report)(report_text) {
        Ok((_, ast)) => ast,
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            return Err(ReportError::ReadError(convert_error(report_text, e)));
        }
        _ => unreachable!(),
    };
    Ok(ast)
}

fn is_keyword(s: &str) -> bool {
    match s {
        "Dear" => true,
        _ => false,
    }
}

fn is_punctuation(s: char) -> bool {
    match s {
        '!' | ',' | '.' | ':' | '?' | '…' | '‽' => true,
        _ => false,
    }
}

fn does(s: &str) -> ReadResult<&str> {
    alt((tag("does"), tag("do"), tag("did")))(s)
}

fn print(s: &str) -> ReadResult<&str> {
    alt((
        tag("I remembered"),
        tag("I said"),
        tag("I sang"),
        tag("I wrote"),
    ))(s)
}

fn identifier(s: &str) -> ReadResult<&str> {
    recognize(separated_list(space1, word))(s)
}

fn word(s: &str) -> ReadResult<&str> {
    recognize(many1(not_space_or_punctuation))(s)
}

fn whitespace0(s: &str) -> ReadResult<&str> {
    recognize(many0(alt((whitespace, line_comment, multiline_comment))))(s)
}

fn whitespace(s: &str) -> ReadResult<&str> {
    is_a(" \t\r\n")(s)
}

fn not_space_or_punctuation(s: &str) -> ReadResult<&str> {
    is_not(" \t\r\n!,.:?…‽")(s)
}

fn punctuation(s: &str) -> ReadResult<&str> {
    take_while(is_punctuation)(s)
}

fn report(s: &str) -> ReadResult<Report> {
    map(
        tuple((
            terminated(report_declaration, whitespace0),
            terminated(many0(paragraph), whitespace0),
            report_closing,
        )),
        |(declaration, paragraphs, closing)| Report {
            name: declaration,
            writer: closing,
            paragraphs,
        },
    )(s)
}

fn report_declaration(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(
            preceded(tag("Dear Princes Celestia:"), whitespace0),
            identifier,
        ),
        punctuation,
    )(s)
}

fn paragraph(s: &str) -> ReadResult<Paragraph> {
    map(
        tuple((
            terminated(paragraph_declaration, whitespace0),
            many0(statement),
            paragraph_closing,
        )),
        |(declaration, statements, closing)| Paragraph {
            name: declaration,
            closing_name: closing,
            statements,
        },
    )(s)
}

fn paragraph_declaration(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(preceded(tag("Today I learned"), whitespace0), identifier),
        punctuation,
    )(s)
}

fn statement(s: &str) -> ReadResult<Expr> {
    terminated(
        terminated(preceded(preceded(print, whitespace0), expr), punctuation),
        whitespace0,
    )(s)
}

fn expr(s: &str) -> ReadResult<Expr> {
    alt((add, value_expr))(s)
}

fn value_expr(s: &str) -> ReadResult<Expr> {
    map(value, Expr::Val)(s)
}

fn literal(s: &str) -> ReadResult<Literal> {
    alt((string, number))(s)
}

fn value(s: &str) -> ReadResult<Value> {
    map(literal, Value::Lit)(s)
}

fn add(s: &str) -> ReadResult<Expr> {
    alt((prefix_add, infix_add))(s)
}

fn prefix_add(s: &str) -> ReadResult<Expr> {
    map(
        preceded(
            terminated(tag("add"), whitespace0),
            separated_pair(
                value,
                delimited(whitespace0, tag("and"), whitespace0),
                value,
            ),
        ),
        |(left, right)| Expr::Add(left, right),
    )(s)
}

fn infix_add(s: &str) -> ReadResult<Expr> {
    fn infix_add_delim(s: &str) -> ReadResult<&str> {
        delimited(
            whitespace0,
            alt((tag("added to"), tag("plus"), tag("and"))),
            whitespace0,
        )(s)
    }

    map(
        separated_pair(value, infix_add_delim, value),
        |(left, right)| Expr::Add(left, right),
    )(s)
}

fn string(s: &str) -> ReadResult<Literal> {
    map(delimited(is_a("\""), is_not("\""), is_a("\"")), |s| {
        Literal::String(s)
    })(s)
}

fn number(s: &str) -> ReadResult<Literal> {
    map(
        preceded(opt(terminated(tag("the number"), whitespace0)), double),
        Literal::Number,
    )(s)
}

fn paragraph_closing(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(preceded(tag("That's all about"), whitespace0), identifier),
        punctuation,
    )(s)
}

fn report_closing(s: &str) -> ReadResult<&str> {
    terminated(
        terminated(
            preceded(
                preceded(tag("Your faithful student"), punctuation),
                take_till1(is_punctuation),
            ),
            punctuation,
        ),
        whitespace0,
    )(s)
}

fn line_comment(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(terminated(many1(tag("P.")), tag("S.")), is_not("\n\r")),
        opt(is_a("\n\r")),
    )(s)
}

fn multiline_comment(s: &str) -> ReadResult<&str> {
    delimited(
        tag("("),
        recognize(many0(alt((is_not("()"), multiline_comment)))),
        tag(")"),
    )(s)
}

#[test]
fn parses_word() {
    assert_eq!(word("An"), Ok(("", "An")));
}

#[test]
fn parses_identifier() {
    assert_eq!(
        identifier("An example letter"),
        Ok(("", "An example letter"))
    );
    assert_eq!(
        identifier("An example letter."),
        Ok((".", "An example letter"))
    );
    assert_eq!(
        identifier("An example letter ."),
        Ok((" .", "An example letter"))
    );
}

#[test]
fn parses_report_declaration() {
    assert_eq!(
        report_declaration("Dear Princes Celestia: An example letter."),
        Ok(("", "An example letter"))
    );
}

#[test]
fn parses_report_closing() {
    assert_eq!(
        report_closing("Your faithful student: Twilight Sparkle."),
        Ok(("", " Twilight Sparkle"))
    );
    assert_eq!(
        report_closing("Your faithful student, Applejack's hat!"),
        Ok(("", " Applejack's hat"))
    );
}

#[test]
fn parses_paragraph_declaration() {
    assert_eq!(
        paragraph_declaration("Today I learned how to fly."),
        Ok(("", "how to fly"))
    );
    assert_eq!(
        paragraph_declaration("Today I learned to say hello world:"),
        Ok(("", "to say hello world"))
    );
}

#[test]
fn parses_paragraph_closing() {
    assert_eq!(
        paragraph_closing("That's all about how to fly."),
        Ok(("", "how to fly"))
    );
    assert_eq!(
        paragraph_closing("That's all about to say hello world!"),
        Ok(("", "to say hello world"))
    );
}

#[test]
fn parses_paragraph() {
    assert_eq!(
        paragraph(
            "Today I learned how to fly.
             I said \"Fly!\".
             That's all about how to fly."
        ),
        Ok((
            "",
            Paragraph {
                name: "how to fly",
                closing_name: "how to fly",
                statements: vec![Expr::Val(Value::Lit(Literal::String("Fly!")))],
            }
        ))
    );
    assert_eq!(
        paragraph(
            "Today I learned how to fly.
             I said \"Fly1!\".
             I said the number 5 added to 6.
             That's all about how to fly."
        ),
        Ok((
            "",
            Paragraph {
                name: "how to fly",
                closing_name: "how to fly",
                statements: vec![
                    Expr::Val(Value::Lit(Literal::String("Fly1!"))),
                    Expr::Add(
                        Value::Lit(Literal::Number(5f64)),
                        Value::Lit(Literal::Number(6f64)),
                    )
                ],
            }
        ))
    );
}

#[test]
fn parses_report() {
    assert_eq!(
        report(
            "Dear Princes Celestia: An example letter.

        Today I learned how to fly:
            I said \"Fly!\"!
        That's all about how to fly!
    Your faithful student: Twilight Sparkle.

    P.S. This is ignored"
        ),
        Ok((
            "",
            Report {
                name: "An example letter",
                paragraphs: vec![Paragraph {
                    name: "how to fly",
                    closing_name: "how to fly",
                    statements: vec![Expr::Val(Value::Lit(Literal::String("Fly!")))],
                }],
                writer: " Twilight Sparkle",
            }
        ))
    );
}

#[test]
fn parses_line_comment() {
    assert_eq!(line_comment("P.S. Comment"), Ok(("", " Comment")));
    assert_eq!(line_comment("P.S. Comment\n"), Ok(("", " Comment")));
    assert_eq!(line_comment("P.P.P.S. Comment\n"), Ok(("", " Comment")));
    assert_eq!(line_comment("P.P.P.S. Comment\r\n"), Ok(("", " Comment")));
}

#[test]
fn parses_multiline_comment() {
    assert_eq!(multiline_comment("(Comment)"), Ok(("", "Comment")));
    assert_eq!(
        multiline_comment("(Nested (Comment))"),
        Ok(("", "Nested (Comment)"))
    );
}

#[test]
fn parses_literal() {
    assert_eq!(literal("\"string\""), Ok(("", Literal::String("string"))));
    assert_eq!(literal("12"), Ok(("", Literal::Number(12f64))));
    assert_eq!(
        literal("the number -1.6"),
        Ok(("", Literal::Number(-1.6f64)))
    );
}

#[test]
fn parses_add() {
    assert_eq!(
        add("1 added to 2"),
        Ok((
            "",
            Expr::Add(
                Value::Lit(Literal::Number(1f64)),
                Value::Lit(Literal::Number(2f64)),
            )
        ))
    );
    assert_eq!(
        add("add 1 and 2"),
        Ok((
            "",
            Expr::Add(
                Value::Lit(Literal::Number(1f64)),
                Value::Lit(Literal::Number(2f64)),
            )
        ))
    );
}

#[test]
fn parses_statement() {
    assert_eq!(
        statement("I wrote 1 added to 2."),
        Ok((
            "",
            Expr::Add(
                Value::Lit(Literal::Number(1f64)),
                Value::Lit(Literal::Number(2f64)),
            )
        ))
    );
}

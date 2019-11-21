use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till1, take_while};
use nom::character::complete::space1;
use nom::combinator::{complete, map, opt, recognize};
use nom::multi::{many0, many1, separated_list};
use nom::number::complete::double;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use crate::error::ReportError;
use crate::pst::{Expr, Literal, Paragraph, Report};

pub fn read(report_text: &str) -> Result<Report, ReportError> {
    let (_, ast) = report(report_text)?;
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

fn does(s: &str) -> IResult<&str, &str> {
    alt((tag("does"), tag("do"), tag("did")))(s)
}

fn print(s: &str) -> IResult<&str, &str> {
    alt((
        tag("I remembered"),
        tag("I said"),
        tag("I sang"),
        tag("I wrote"),
    ))(s)
}

fn identifier(s: &str) -> IResult<&str, &str> {
    recognize(separated_list(space1, word))(s)
}

fn word(s: &str) -> IResult<&str, &str> {
    recognize(many1(not_space_or_punctuation))(s)
}

fn whitespace0(s: &str) -> IResult<&str, &str> {
    recognize(many0(alt((whitespace, line_comment, multiline_comment))))(s)
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
    map(
        complete(tuple((
            terminated(report_declaration, whitespace0),
            terminated(many0(paragraph), whitespace0),
            report_closing,
        ))),
        |(declaration, paragraphs, closing)| Report {
            name: declaration,
            writer: closing,
            paragraphs,
        },
    )(s)
}

fn report_declaration(s: &str) -> IResult<&str, &str> {
    terminated(
        preceded(
            preceded(tag("Dear Princes Celestia:"), whitespace0),
            identifier,
        ),
        punctuation,
    )(s)
}

fn paragraph(s: &str) -> IResult<&str, Paragraph> {
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

fn paragraph_declaration(s: &str) -> IResult<&str, &str> {
    terminated(
        preceded(preceded(tag("Today I learned"), whitespace0), identifier),
        punctuation,
    )(s)
}

fn statement(s: &str) -> IResult<&str, Expr> {
    terminated(
        terminated(preceded(preceded(print, whitespace0), expr), punctuation),
        whitespace0,
    )(s)
}

fn expr(s: &str) -> IResult<&str, Expr> {
    map(literal, |l| Expr::Lit(l))(s)
}

fn literal(s: &str) -> IResult<&str, Literal> {
    alt((string, number))(s)
}

fn string(s: &str) -> IResult<&str, Literal> {
    map(delimited(is_a("\""), is_not("\""), is_a("\"")), |s| {
        Literal::String(s)
    })(s)
}

fn number(s: &str) -> IResult<&str, Literal> {
    map(
        preceded(opt(terminated(tag("the number"), whitespace0)), double),
        |n| Literal::Number(n),
    )(s)
}

fn paragraph_closing(s: &str) -> IResult<&str, &str> {
    terminated(
        preceded(preceded(tag("That's all about"), whitespace0), identifier),
        punctuation,
    )(s)
}

fn report_closing(s: &str) -> IResult<&str, &str> {
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

fn line_comment(s: &str) -> IResult<&str, &str> {
    terminated(
        preceded(terminated(many1(tag("P.")), tag("S.")), is_not("\n\r")),
        opt(is_a("\n\r")),
    )(s)
}

fn multiline_comment(s: &str) -> IResult<&str, &str> {
    delimited(
        tag("("),
        recognize(many0(alt((is_not("()"), multiline_comment)))),
        tag(")"),
    )(s)
}

#[test]
fn parses_does() {
    use nom::error::ErrorKind;

    assert_eq!(does("does"), Ok(("", "does")));
    assert_eq!(does("not"), Err(nom::Err::Error(("not", ErrorKind::Tag))));
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
            "Today I learned how to fly.\
             I said \"Fly!\".\
             That's all about how to fly."
        ),
        Ok((
            "",
            Paragraph {
                name: "how to fly",
                closing_name: "how to fly",
                statements: vec![Expr::Lit(Literal::String("Fly!"))],
            }
        ))
    );
    assert_eq!(
        paragraph(
            "Today I learned how to fly.\
             I said \"Fly1!\".\
             I said the number 5.\
             That's all about how to fly."
        ),
        Ok((
            "",
            Paragraph {
                name: "how to fly",
                closing_name: "how to fly",
                statements: vec![
                    Expr::Lit(Literal::String("Fly1!")),
                    Expr::Lit(Literal::Number(5f64))
                ],
            }
        ))
    );
}

#[test]
fn parses_report() {
    assert_eq!(
        report(
            "Dear Princes Celestia: An example letter.\
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
                    statements: vec![Expr::Lit(Literal::String("Fly!"))],
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

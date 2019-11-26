use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till1, take_while};
use nom::character::complete::space1;
use nom::combinator::{complete, map, opt, recognize};
use nom::error::{convert_error, ErrorKind, ParseError, VerboseError};
use nom::multi::{fold_many0, many0, many1, separated_list};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::error::ReportError;
use crate::pst::{BinOperator, Expr, Literal, Paragraph, Report, Statement, Value, Variable};
use crate::types::Type;
use crate::types::Type::{Boolean, Chars, Number};

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

fn keyword(s: &str) -> ReadResult<&str> {
    alt((
        keyword_declare,
        keyword_always,
        keyword_assign,
        keyword_infix_add,
        keyword_infix_sub,
        keyword_infix_mul,
        keyword_infix_div,
    ))(s)
}

fn is_punctuation(s: char) -> bool {
    match s {
        '!' | ',' | '.' | ':' | '?' | '…' | '‽' => true,
        _ => false,
    }
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
    let (rest, word) = recognize(many1(not_space_or_punctuation))(s)?;
    // fail on keywords
    if keyword(word).is_ok() {
        return Err(nom::Err::Error(VerboseError::from_error_kind(
            s,
            ErrorKind::Not,
        )));
    }
    Ok((rest, word))
}

fn whitespace_delim<'a, O, P>(parser: P) -> impl Fn(&'a str) -> ReadResult<O>
where
    P: Fn(&'a str) -> ReadResult<O>,
{
    delimited(whitespace0, parser, whitespace0)
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
            preceded(tag("Dear Princess Celestia:"), whitespace0),
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

fn statement(s: &str) -> ReadResult<Statement> {
    terminated(
        terminated(
            alt((print_statement, declare_statement, assign_statement)),
            punctuation,
        ),
        whitespace0,
    )(s)
}

fn print_statement(s: &str) -> ReadResult<Statement> {
    map(preceded(preceded(print, whitespace0), expr), |s| {
        Statement::Print(s)
    })(s)
}

fn keyword_always(s: &str) -> ReadResult<&str> {
    tag("always")(s)
}

fn declare_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(
            preceded(tag("Did you know that"), whitespace0),
            tuple((
                identifier,
                whitespace_delim(terminated(
                    map(opt(terminated(keyword_always, whitespace0)), |always| {
                        always.is_some()
                    }),
                    keyword_declare,
                )),
                declare_type,
                opt(whitespace_delim(literal)),
            )),
        ),
        |(name, is_const, type_, lit)| Statement::Declare(Variable(name), type_, lit, is_const),
    )(s)
}

fn assign_statement(s: &str) -> ReadResult<Statement> {
    map(
        separated_pair(var, whitespace_delim(keyword_assign), expr),
        |(var, expr)| Statement::Assign(var, expr),
    )(s)
}

fn declare_type(s: &str) -> ReadResult<Type> {
    alt((type_number, type_chars, type_boolean))(s)
}

fn type_number(s: &str) -> ReadResult<Type> {
    map(
        recognize(separated_pair(
            alt((tag("a"), tag("the"))),
            whitespace0,
            tag("number"),
        )),
        |_| Number,
    )(s)
}

fn type_chars(s: &str) -> ReadResult<Type> {
    map(
        recognize(separated_pair(
            alt((tag("a"), tag("the"))),
            whitespace0,
            alt((
                tag("word"),
                tag("phrase"),
                tag("sentence"),
                tag("quote"),
                tag("name"),
            )),
        )),
        |_| Chars,
    )(s)
}

fn type_boolean(s: &str) -> ReadResult<Type> {
    map(
        recognize(separated_pair(
            alt((tag("a"), tag("the"))),
            whitespace0,
            alt((tag("logic"), tag("argument"))),
        )),
        |_| Boolean,
    )(s)
}

fn keyword_declare(s: &str) -> ReadResult<&str> {
    alt((
        tag("is"),
        tag("was"),
        tag("has"),
        tag("had"),
        tag("like"),
        tag("likes"),
        tag("liked"),
    ))(s)
}

fn keyword_assign(s: &str) -> ReadResult<&str> {
    alt((
        recognize(separated_pair(
            alt((tag("is"), tag("are"))),
            whitespace0,
            tag("now"),
        )),
        recognize(separated_pair(
            tag("now"),
            whitespace0,
            alt((tag("like"), tag("likes"))),
        )),
        tag("become"),
        tag("becomes"),
    ))(s)
}

fn expr(s: &str) -> ReadResult<Expr> {
    alt((prefix_term, infix_term, value_expr))(s)
}

fn value_expr(s: &str) -> ReadResult<Expr> {
    alt((prefix_not, map(value, Expr::Val)))(s)
}

fn literal(s: &str) -> ReadResult<Literal> {
    alt((string, number, boolean))(s)
}

fn var(s: &str) -> ReadResult<Variable> {
    map(identifier, Variable)(s)
}

fn value(s: &str) -> ReadResult<Value> {
    alt((value_lit, value_var))(s)
}

fn value_lit(s: &str) -> ReadResult<Value> {
    map(literal, Value::Lit)(s)
}

fn value_var(s: &str) -> ReadResult<Value> {
    map(var, Value::Var)(s)
}

fn prefix<'a, O1, O2, O3, P1, P2, V1, V2>(
    op1: P1,
    op2: P2,
    first: V1,
    second: V2,
) -> impl Fn(&'a str) -> ReadResult<(O2, O3)>
where
    P1: Fn(&'a str) -> ReadResult<O1>,
    P2: Fn(&'a str) -> ReadResult<O1>,
    V1: Fn(&'a str) -> ReadResult<O2>,
    V2: Fn(&'a str) -> ReadResult<O3>,
{
    preceded(
        terminated(op1, whitespace0),
        separated_pair(first, whitespace_delim(op2), second),
    )
}

fn infix<'a, O1, O2, O3, P, V1, V2>(
    operator: P,
    left: V1,
    right: V2,
) -> impl Fn(&'a str) -> ReadResult<(O2, O3)>
where
    P: Fn(&'a str) -> ReadResult<O1>,
    V1: Fn(&'a str) -> ReadResult<O2>,
    V2: Fn(&'a str) -> ReadResult<O3>,
{
    let infix_delim = delimited(whitespace0, operator, whitespace0);
    separated_pair(left, infix_delim, right)
}

fn infix_term(s: &str) -> ReadResult<Expr> {
    let (s, init) = value_expr(s)?;
    fold_many0(pair(infix_op, value_expr), init, |acc, (op, val)| {
        Expr::BinOp(op, Box::new(acc), Box::new(val))
    })(s)
}

fn prefix_term(s: &str) -> ReadResult<Expr> {
    alt((prefix_add, prefix_sub, prefix_mul, prefix_div, prefix_xor))(s)
}

fn infix_op(s: &str) -> ReadResult<BinOperator> {
    alt((
        infix_add_op,
        infix_sub_op,
        infix_mul_op,
        infix_div_op,
        infix_and_op,
        infix_or_op,
    ))(s)
}

fn keyword_infix_add(s: &str) -> ReadResult<&str> {
    alt((tag("added to"), tag("plus"), tag("and")))(s)
}

fn infix_add_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_add), |_| {
        BinOperator::AddOrAnd
    })(s)
}

fn keyword_infix_sub(s: &str) -> ReadResult<&str> {
    alt((tag("minus"), tag("without")))(s)
}

fn infix_sub_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_sub), |_| BinOperator::Sub)(s)
}

fn keyword_infix_mul(s: &str) -> ReadResult<&str> {
    alt((tag("multiplied with"), tag("times")))(s)
}

fn infix_mul_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_mul), |_| BinOperator::Mul)(s)
}

fn keyword_infix_div(s: &str) -> ReadResult<&str> {
    alt((tag("divided by"), tag("over")))(s)
}

fn infix_div_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_div), |_| BinOperator::Div)(s)
}

fn prefix_add(s: &str) -> ReadResult<Expr> {
    map(
        prefix(tag("add"), tag("and"), value_expr, value_expr),
        |(left, right)| Expr::BinOp(BinOperator::AddOrAnd, Box::new(left), Box::new(right)),
    )(s)
}

fn prefix_sub(s: &str) -> ReadResult<Expr> {
    map(
        alt((
            prefix(
                tag("the difference between"),
                tag("and"),
                value_expr,
                value_expr,
            ),
            prefix(tag("subtract"), tag("from"), value_expr, value_expr),
        )),
        |(left, right)| Expr::BinOp(BinOperator::Sub, Box::new(left), Box::new(right)),
    )(s)
}

fn prefix_mul(s: &str) -> ReadResult<Expr> {
    map(
        alt((
            prefix(
                tag("multiply"),
                alt((tag("by"), tag("and"))),
                value_expr,
                value_expr,
            ),
            prefix(tag("the product of"), tag("and"), value_expr, value_expr),
        )),
        |(left, right)| Expr::BinOp(BinOperator::Mul, Box::new(left), Box::new(right)),
    )(s)
}

fn prefix_div(s: &str) -> ReadResult<Expr> {
    map(
        prefix(
            tag("divide"),
            alt((tag("by"), tag("and"))),
            value_expr,
            value_expr,
        ),
        |(left, right)| Expr::BinOp(BinOperator::Div, Box::new(left), Box::new(right)),
    )(s)
}

fn infix_and_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(tag("and")), |_| BinOperator::AddOrAnd)(s)
}

fn infix_or_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(tag("or")), |_| BinOperator::Or)(s)
}

fn prefix_xor(s: &str) -> ReadResult<Expr> {
    map(
        prefix(tag("either"), tag("or"), value_expr, value_expr),
        |(left, right)| Expr::BinOp(BinOperator::EitherOr, Box::new(left), Box::new(right)),
    )(s)
}

fn prefix_not(s: &str) -> ReadResult<Expr> {
    map(
        preceded(terminated(tag("not"), whitespace0), value_lit),
        |b| Expr::Not(b),
    )(s)
}

fn boolean(s: &str) -> ReadResult<Literal> {
    alt((true_, false_))(s)
}

fn true_(s: &str) -> ReadResult<Literal> {
    map(
        alt((tag("correct"), tag("right"), tag("true"), tag("yes"))),
        |_| Literal::Boolean(true),
    )(s)
}

fn false_(s: &str) -> ReadResult<Literal> {
    map(
        alt((tag("false"), tag("incorrect"), tag("no"), tag("wrong"))),
        |_| Literal::Boolean(false),
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
        report_declaration("Dear Princess Celestia: An example letter."),
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
                statements: vec![Statement::Print(Expr::Val(Value::Lit(Literal::String(
                    "Fly!"
                ))))],
            }
        ))
    );
    assert_eq!(
        paragraph(
            "Today I learned how to fly.
             I said \"Fly1!\".
             I said the number 5 added to 6.
             I said yes.
             That's all about how to fly."
        ),
        Ok((
            "",
            Paragraph {
                name: "how to fly",
                closing_name: "how to fly",
                statements: vec![
                    Statement::Print(Expr::Val(Value::Lit(Literal::String("Fly1!")))),
                    Statement::Print(Expr::BinOp(
                        BinOperator::AddOrAnd,
                        Box::new(Expr::Val(Value::Lit(Literal::Number(5f64)))),
                        Box::new(Expr::Val(Value::Lit(Literal::Number(6f64)))),
                    )),
                    Statement::Print(Expr::Val(Value::Lit(Literal::Boolean(true)))),
                ],
            }
        ))
    );
}

#[test]
fn parses_report() {
    assert_eq!(
        report(
            "Dear Princess Celestia: An example letter.

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
                    statements: vec![Statement::Print(Expr::Val(Value::Lit(Literal::String(
                        "Fly!"
                    ))))],
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
fn parses_infix_op() {
    assert_eq!(infix_op(" added to "), Ok(("", BinOperator::AddOrAnd)));
    assert_eq!(infix_op(" minus "), Ok(("", BinOperator::Sub)));
    assert_eq!(infix_op(" multiplied with "), Ok(("", BinOperator::Mul)));
    assert_eq!(infix_op(" divided by "), Ok(("", BinOperator::Div)));
    assert_eq!(infix_op(" or "), Ok(("", BinOperator::Or)));
}

#[test]
fn parses_infix_term() {
    assert_eq!(
        infix_term("1"),
        Ok(("", Expr::Val(Value::Lit(Literal::Number(1f64)))))
    );
    assert_eq!(
        infix_term("1 added to 2"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
            )
        ))
    );
    assert_eq!(
        infix_term("2 plus 1 times 3"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Mul,
                Box::new(Expr::BinOp(
                    BinOperator::AddOrAnd,
                    Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
                    Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
                )),
                Box::new(Expr::Val(Value::Lit(Literal::Number(3f64)))),
            )
        ))
    );
    assert_eq!(
        infix_term("true and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(true)))),
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(false)))),
            )
        ))
    );
    assert_eq!(
        infix_term("true or false and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::BinOp(
                    BinOperator::Or,
                    Box::new(Expr::Val(Value::Lit(Literal::Boolean(true)))),
                    Box::new(Expr::Val(Value::Lit(Literal::Boolean(false)))),
                )),
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(false)))),
            )
        ))
    );
}

#[test]
fn parses_prefix_term() {
    assert_eq!(
        prefix_term("add 1 and 2"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
            )
        ))
    );
    assert_eq!(
        prefix_term("the difference between 2 and 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Sub,
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
            )
        ))
    );
    assert_eq!(
        prefix_term("the product of 2 and 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Mul,
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
            )
        ))
    );
    assert_eq!(
        prefix_term("divide 2 by 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Div,
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
            )
        ))
    );
    assert_eq!(
        expr("either true or false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::EitherOr,
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(true)))),
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(false)))),
            )
        ))
    );
}

#[test]
fn parses_not() {
    assert_eq!(
        prefix_not("not true"),
        Ok(("", Expr::Not(Value::Lit(Literal::Boolean(true)))))
    );
    assert_eq!(
        expr("not true and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Not(Value::Lit(Literal::Boolean(true)))),
                Box::new(Expr::Val(Value::Lit(Literal::Boolean(false)))),
            )
        ))
    );
}

#[test]
fn parses_print_statement() {
    assert_eq!(
        statement("I wrote 1 added to 2."),
        Ok((
            "",
            Statement::Print(Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(2f64)))),
            ))
        ))
    );
    assert_eq!(
        statement("I sang the elements of harmony count."),
        Ok((
            "",
            Statement::Print(Expr::Val(Value::Var(Variable(
                "the elements of harmony count"
            ))))
        ))
    );
}

#[test]
fn parses_declare_statement() {
    assert_eq!(
        statement("Did you know that the elements of harmony count is a number?"),
        Ok((
            "",
            Statement::Declare(
                Variable("the elements of harmony count"),
                Number,
                None,
                false
            )
        ))
    );
    assert_eq!(
        statement("Did you know that Applejack's hat has the name \"Talluah\"?"),
        Ok((
            "",
            Statement::Declare(
                Variable("Applejack's hat"),
                Chars,
                Some(Literal::String("Talluah")),
                false
            )
        ))
    );
    assert_eq!(
        statement("Did you know that Pinkie Pie always has the argument right?"),
        Ok((
            "",
            Statement::Declare(
                Variable("Pinkie Pie"),
                Boolean,
                Some(Literal::Boolean(true)),
                true
            )
        ))
    );
}

#[test]
fn parses_assign_statement() {
    assert_eq!(
        statement("Spike's age is now 11!"),
        Ok((
            "",
            Statement::Assign(
                Variable("Spike's age"),
                Expr::Val(Value::Lit(Literal::Number(11f64)))
            )
        ))
    );
    assert_eq!(
        statement("Spike's age is now 10 plus 1!"),
        Ok((
            "",
            Statement::Assign(
                Variable("Spike's age"),
                Expr::BinOp(
                    BinOperator::AddOrAnd,
                    Box::new(Expr::Val(Value::Lit(Literal::Number(10f64)))),
                    Box::new(Expr::Val(Value::Lit(Literal::Number(1f64))))
                )
            )
        ))
    );
}

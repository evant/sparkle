use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till1, take_while};
use nom::character::complete::space1;
use nom::combinator::{complete, map, opt, recognize};
use nom::error::{convert_error, ErrorKind, ParseError, VerboseError};
use nom::multi::{fold_many0, many0, many1, separated_nonempty_list};
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
        keyword_increment,
        keyword_decrement,
        keyword_declare_paragraph_type,
    ))(s)
}

fn is_punctuation(s: char) -> bool {
    match s {
        '!' | ',' | '.' | ':' | '?' | '…' | '‽' => true,
        _ => false,
    }
}

fn keyword_print(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("I"),
        whitespace1,
        alt((tag("said"), tag("sang"), tag("wrote"))),
    )))(s)
}

fn identifier(s: &str) -> ReadResult<&str> {
    recognize(separated_nonempty_list(space1, word))(s)
}

fn word(s: &str) -> ReadResult<&str> {
    let (rest, word) = recognize(many1(not_space_or_punctuation))(s)?;
    // fail on keywords
    if keyword(s).is_ok() {
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

fn whitespace_delim1<'a, O, P>(parser: P) -> impl Fn(&'a str) -> ReadResult<O>
where
    P: Fn(&'a str) -> ReadResult<O>,
{
    delimited(whitespace1, parser, whitespace1)
}

fn whitespace0(s: &str) -> ReadResult<&str> {
    recognize(many0(alt((whitespace, line_comment, multiline_comment))))(s)
}

fn whitespace1(s: &str) -> ReadResult<&str> {
    recognize(many1(alt((whitespace, line_comment, multiline_comment))))(s)
}

fn whitespace(s: &str) -> ReadResult<&str> {
    is_a(" \t\r\n")(s)
}

fn not_space_or_punctuation(s: &str) -> ReadResult<&str> {
    is_not(" \t\r\n!,.:?…‽\"")(s)
}

fn punctuation(s: &str) -> ReadResult<&str> {
    take_while(is_punctuation)(s)
}

fn report(s: &str) -> ReadResult<Report> {
    map(
        tuple((
            terminated(report_declaration, whitespace0),
            many0(terminated(paragraph, whitespace0)),
            report_closing,
        )),
        |(declaration, paragraphs, closing)| Report {
            name: declaration,
            writer: closing,
            paragraphs,
        },
    )(s)
}

fn keyword_declare_report(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("Dear"),
        whitespace1,
        tag("Princess"),
        whitespace1,
        tag("Celestia:"),
    )))(s)
}

fn report_declaration(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(preceded(keyword_declare_report, whitespace0), identifier),
        punctuation,
    )(s)
}

fn paragraph(s: &str) -> ReadResult<Paragraph> {
    map(
        tuple((
            opt(terminated(tag("Today"), whitespace1)),
            terminated(paragraph_declaration, whitespace0),
            many0(statement),
            paragraph_closing,
        )),
        |(today, (declaration, type_), statements, closing)| Paragraph {
            name: declaration,
            closing_name: closing,
            mane: today.is_some(),
            args: vec![],
            return_type: type_,
            statements,
        },
    )(s)
}

fn keyword_declare_paragraph(s: &str) -> ReadResult<&str> {
    recognize(tuple((tag("I"), whitespace1, tag("learned"))))(s)
}

fn keyword_declare_paragraph_type(s: &str) -> ReadResult<&str> {
    alt((
        tag("with"),
        recognize(tuple((tag("to"), whitespace1, tag("get")))),
    ))(s)
}

fn paragraph_declaration(s: &str) -> ReadResult<(&str, Option<Type>)> {
    terminated(
        pair(
            preceded(preceded(keyword_declare_paragraph, whitespace0), identifier),
            opt(preceded(
                whitespace_delim1(keyword_declare_paragraph_type),
                declare_type,
            )),
        ),
        punctuation,
    )(s)
}

fn statement(s: &str) -> ReadResult<Statement> {
    terminated(
        alt((
            print_statement,
            declare_statement,
            assign_statement,
            if_statement,
            while_statement,
            increment_statement,
            decrement_statement,
            call_statement,
            return_statement,
        )),
        tuple((whitespace0, punctuation, whitespace0)),
    )(s)
}

fn print_statement(s: &str) -> ReadResult<Statement> {
    map(preceded(preceded(keyword_print, whitespace1), expr), |s| {
        Statement::Print(s)
    })(s)
}

fn keyword_always(s: &str) -> ReadResult<&str> {
    tag("always")(s)
}

fn keyword_declare_statement(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("Did"),
        whitespace1,
        tag("you"),
        whitespace1,
        tag("know"),
        whitespace1,
        tag("that"),
    )))(s)
}

fn declare_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(
            preceded(keyword_declare_statement, whitespace1),
            tuple((
                identifier,
                whitespace_delim(terminated(
                    map(opt(terminated(keyword_always, whitespace1)), |always| {
                        always.is_some()
                    }),
                    keyword_declare,
                )),
                opt(declare_type),
                opt(whitespace_delim(expr)),
            )),
        ),
        |(name, is_const, type_, expr)| Statement::Declare(Variable(name), type_, expr, is_const),
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

fn keyword_type_number(s: &str) -> ReadResult<&str> {
    recognize(separated_pair(
        alt((tag("a"), tag("the"))),
        whitespace1,
        tag("number"),
    ))(s)
}

fn type_number(s: &str) -> ReadResult<Type> {
    map(keyword_type_number, |_| Number)(s)
}

fn keyword_type_chars(s: &str) -> ReadResult<&str> {
    recognize(separated_pair(
        alt((tag("a"), tag("the"))),
        whitespace1,
        alt((
            tag("word"),
            tag("phrase"),
            tag("sentence"),
            tag("quote"),
            tag("name"),
        )),
    ))(s)
}

fn type_chars(s: &str) -> ReadResult<Type> {
    map(keyword_type_chars, |_| Chars)(s)
}

fn keyword_type_boolean(s: &str) -> ReadResult<&str> {
    recognize(separated_pair(
        alt((tag("a"), tag("the"))),
        whitespace1,
        alt((tag("logic"), tag("argument"))),
    ))(s)
}

fn type_boolean(s: &str) -> ReadResult<Type> {
    map(keyword_type_boolean, |_| Boolean)(s)
}

fn keyword_declare(s: &str) -> ReadResult<&str> {
    alt((
        tag("is"),
        tag("was"),
        tag("has"),
        tag("had"),
        tag("likes"),
        tag("like"),
        tag("liked"),
    ))(s)
}

fn keyword_assign(s: &str) -> ReadResult<&str> {
    alt((
        recognize(separated_pair(
            alt((tag("is"), tag("are"))),
            whitespace1,
            tag("now"),
        )),
        recognize(separated_pair(
            tag("now"),
            whitespace1,
            alt((tag("like"), tag("likes"))),
        )),
        tag("become"),
        tag("becomes"),
    ))(s)
}

fn if_statement(s: &str) -> ReadResult<Statement> {
    map(
        tuple((
            terminated(if_declaration, whitespace0),
            many0(statement),
            opt(preceded(else_declaration, many0(statement))),
            if_closing,
        )),
        |(cond, if_, else_, _)| Statement::If(cond, if_, else_.unwrap_or_else(|| vec![])),
    )(s)
}

fn if_declaration(s: &str) -> ReadResult<Expr> {
    map(
        preceded(
            terminated(alt((tag("If"), tag("When"))), whitespace1),
            terminated(
                terminated(expr, opt(preceded(whitespace1, tag("then")))),
                punctuation,
            ),
        ),
        |cond| cond,
    )(s)
}

fn else_declaration(s: &str) -> ReadResult<&str> {
    terminated(
        terminated(
            alt((
                recognize(tuple((tag("Or"), whitespace1, tag("else")))),
                tag("Otherwise"),
            )),
            punctuation,
        ),
        whitespace0,
    )(s)
}

fn keyword_if_closing(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("That's"),
        whitespace1,
        tag("what"),
        whitespace1,
        tag("I"),
        whitespace1,
        tag("would"),
        whitespace1,
        tag("do"),
    )))(s)
}

fn if_closing(s: &str) -> ReadResult<&str> {
    terminated(keyword_if_closing, punctuation)(s)
}

fn while_statement(s: &str) -> ReadResult<Statement> {
    map(
        tuple((
            terminated(while_declaration, whitespace0),
            many0(statement),
            while_closing,
        )),
        |(cond, body, _)| Statement::While(cond, body),
    )(s)
}

fn keyword_declare_while(s: &str) -> ReadResult<&str> {
    alt((
        recognize(tuple((
            tag("Here's"),
            whitespace1,
            tag("what"),
            whitespace1,
            tag("I"),
            whitespace1,
            tag("did"),
            whitespace1,
            tag("while"),
        ))),
        recognize(tuple((
            tag("As"),
            whitespace1,
            tag("long"),
            whitespace1,
            tag("as"),
        ))),
    ))(s)
}

fn while_declaration(s: &str) -> ReadResult<Expr> {
    map(
        preceded(
            terminated(keyword_declare_while, whitespace1),
            terminated(expr, punctuation),
        ),
        |cond| cond,
    )(s)
}

fn keyword_while_closing(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("That's"),
        whitespace1,
        tag("what"),
        whitespace1,
        tag("I"),
        whitespace1,
        tag("did"),
    )))(s)
}

fn while_closing(s: &str) -> ReadResult<&str> {
    terminated(keyword_while_closing, punctuation)(s)
}

fn keyword_increment(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("got"),
        whitespace1,
        tag("one"),
        whitespace1,
        tag("more"),
    )))(s)
}

fn increment_statement(s: &str) -> ReadResult<Statement> {
    map(
        terminated(var, pair(whitespace1, keyword_increment)),
        Statement::Increment,
    )(s)
}

fn keyword_decrement(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("got"),
        whitespace1,
        tag("one"),
        whitespace1,
        tag("less"),
    )))(s)
}

fn decrement_statement(s: &str) -> ReadResult<Statement> {
    map(
        terminated(var, pair(whitespace1, keyword_decrement)),
        Statement::Decrement,
    )(s)
}

fn keyword_call(s: &str) -> ReadResult<&str> {
    recognize(tuple((tag("I"), whitespace1, tag("remembered"))))(s)
}

fn call_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(terminated(keyword_call, whitespace1), var),
        Statement::Call,
    )(s)
}

fn keyword_return(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("Then"),
        whitespace1,
        tag("you"),
        whitespace1,
        tag("get"),
    )))(s)
}

fn return_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(terminated(keyword_return, whitespace1), expr),
        Statement::Return,
    )(s)
}

fn expr_term(s: &str) -> ReadResult<Expr> {
    alt((prefix_term, infix_term, value_expr))(s)
}

fn expr(s: &str) -> ReadResult<Expr> {
    let (mut rest, mut e) = expr_term(s)?;
    let mut acc: Vec<Expr> = vec![];
    loop {
        rest = whitespace0(rest)?.0;
        // Alternate between expressions and char literals
        let next_parser = if is_chars_literal(&e) {
            expr_term
        } else {
            expr_string
        };
        acc.push(e);
        let (next_rest, next_e) = match next_parser(rest) {
            Err(nom::Err::Error(_)) => {
                // If we only have one expression return that, otherwise we need to concatenate them.
                return Ok((
                    rest,
                    if acc.len() == 1 {
                        acc.remove(0)
                    } else {
                        Expr::Concat(acc)
                    },
                ));
            }
            Err(e) => return Err(e),
            Ok(v) => v,
        };
        rest = next_rest;
        e = next_e;
    }
}

fn expr_string(s: &str) -> ReadResult<Expr> {
    map(string, |s| Expr::Val(Value::Lit(s)))(s)
}

fn is_chars_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Val(Value::Lit(Literal::Chars(_))) => true,
        _ => false,
    }
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
        infix_lt_op,
        infix_gt_op,
        infix_lte_op,
        infix_gte_op,
        infix_neq_op,
        infix_eq_op,
    ))(s)
}

fn keyword_infix_add(s: &str) -> ReadResult<&str> {
    alt((
        recognize(tuple((tag("added"), whitespace1, tag("to")))),
        tag("plus"),
        tag("and"),
    ))(s)
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
    alt((
        recognize(tuple((tag("multiplied"), whitespace1, tag("with")))),
        tag("times"),
    ))(s)
}

fn infix_mul_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_mul), |_| BinOperator::Mul)(s)
}

fn keyword_infix_div(s: &str) -> ReadResult<&str> {
    alt((
        recognize(tuple((tag("divided"), whitespace1, tag("by")))),
        tag("over"),
    ))(s)
}

fn infix_div_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_div), |_| BinOperator::Div)(s)
}

fn keyword_infix_eq(s: &str) -> ReadResult<&str> {
    alt((tag("is"), tag("was"), tag("has"), tag("had")))(s)
}

fn infix_eq_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_eq), |_| BinOperator::Equal)(s)
}

fn infix_neq_op(s: &str) -> ReadResult<BinOperator> {
    map(
        whitespace_delim(pair(
            keyword_infix_eq,
            alt((preceded(whitespace1, tag("not")), tag("n't"))),
        )),
        |_| BinOperator::NotEqual,
    )(s)
}

fn infix_lt_op(s: &str) -> ReadResult<BinOperator> {
    map(
        whitespace_delim(pair(
            keyword_infix_eq,
            tuple((whitespace1, tag("less"), whitespace1, tag("than"))),
        )),
        |_| BinOperator::LessThan,
    )(s)
}

fn infix_gt_op(s: &str) -> ReadResult<BinOperator> {
    map(
        whitespace_delim(pair(
            keyword_infix_eq,
            tuple((
                whitespace1,
                alt((tag("more"), tag("greater"))),
                whitespace1,
                tag("than"),
            )),
        )),
        |_| BinOperator::GreaterThan,
    )(s)
}

fn infix_lte_op(s: &str) -> ReadResult<BinOperator> {
    map(
        whitespace_delim(pair(
            keyword_infix_eq,
            tuple((
                alt((
                    preceded(whitespace1, tag("not")),
                    preceded(whitespace1, tag("no")),
                    tag("n't"),
                )),
                whitespace1,
                alt((tag("more"), tag("greater"))),
                whitespace1,
                tag("than"),
            )),
        )),
        |_| BinOperator::LessThanOrEqual,
    )(s)
}

fn infix_gte_op(s: &str) -> ReadResult<BinOperator> {
    map(
        whitespace_delim(pair(
            keyword_infix_eq,
            tuple((
                alt((
                    preceded(whitespace1, tag("not")),
                    preceded(whitespace1, tag("no")),
                    tag("n't"),
                )),
                whitespace1,
                tag("less"),
                whitespace1,
                tag("than"),
            )),
        )),
        |_| BinOperator::GreaterThanOrEqual,
    )(s)
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
                recognize(tuple((
                    tag("the"),
                    whitespace1,
                    tag("difference"),
                    whitespace1,
                    tag("between"),
                ))),
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
            prefix(
                recognize(tuple((
                    tag("the"),
                    whitespace1,
                    tag("product"),
                    whitespace1,
                    tag("of"),
                ))),
                tag("and"),
                value_expr,
                value_expr,
            ),
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
        preceded(terminated(tag("not"), whitespace0), value),
        Expr::Not,
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
        Literal::Chars(s)
    })(s)
}

fn number(s: &str) -> ReadResult<Literal> {
    map(
        preceded(opt(terminated(keyword_type_number, whitespace1)), double),
        Literal::Number,
    )(s)
}

fn keyword_close_paragraph(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("That's"),
        whitespace1,
        tag("all"),
        whitespace1,
        tag("about"),
    )))(s)
}

fn paragraph_closing(s: &str) -> ReadResult<&str> {
    terminated(
        preceded(preceded(keyword_close_paragraph, whitespace1), identifier),
        punctuation,
    )(s)
}

fn keyword_close_report(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("Your"),
        whitespace1,
        tag("faithful"),
        whitespace1,
        tag("student"),
    )))(s)
}

fn report_closing(s: &str) -> ReadResult<&str> {
    terminated(
        terminated(
            preceded(
                preceded(keyword_close_report, punctuation),
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
        paragraph_declaration("I learned how to fly."),
        Ok(("", ("how to fly", None)))
    );
    assert_eq!(
        paragraph_declaration("I learned to say hello world with a number:"),
        Ok(("", ("to say hello world", Some(Number))))
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
                mane: true,
                args: vec![],
                return_type: None,
                statements: vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
                    "Fly!"
                ))))],
            }
        ))
    );
    assert_eq!(
        paragraph(
            "I learned how to fly.
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
                mane: false,
                args: vec![],
                return_type: None,
                statements: vec![
                    Statement::Print(Expr::Val(Value::Lit(Literal::Chars("Fly1!")))),
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
                    mane: true,
                    args: vec![],
                    return_type: None,
                    statements: vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
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
    assert_eq!(literal("\"string\""), Ok(("", Literal::Chars("string"))));
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
        prefix_not("not a tree"),
        Ok(("", Expr::Not(Value::Var(Variable("a tree")))))
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
fn parses_comparison() {
    assert_eq!(
        expr("Rainbow Dash is cool"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Equal,
                Box::new(Expr::Val(Value::Var(Variable("Rainbow Dash")))),
                Box::new(Expr::Val(Value::Var(Variable("cool")))),
            )
        ))
    );
    assert_eq!(
        expr("Fluttershy isn't loud"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::NotEqual,
                Box::new(Expr::Val(Value::Var(Variable("Fluttershy")))),
                Box::new(Expr::Val(Value::Var(Variable("loud")))),
            )
        ))
    );
    assert_eq!(
        expr("the number of cupcakes is less than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::LessThan,
                Box::new(Expr::Val(Value::Var(Variable("the number of cupcakes")))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(10f64)))),
            )
        ))
    );
    assert_eq!(
        expr("the number of pies is not less than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThanOrEqual,
                Box::new(Expr::Val(Value::Var(Variable("the number of pies")))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(10f64)))),
            )
        ))
    );
    assert_eq!(
        expr("the number of cakes is more than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThan,
                Box::new(Expr::Val(Value::Var(Variable("the number of cakes")))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(10f64)))),
            )
        ))
    );
    assert_eq!(
        expr("the number of cute animals isn't greater than 100"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::LessThanOrEqual,
                Box::new(Expr::Val(Value::Var(Variable(
                    "the number of cute animals"
                )))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(100f64)))),
            )
        ))
    );
    assert_eq!(
        expr("Applejack has more than 50"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThan,
                Box::new(Expr::Val(Value::Var(Variable("Applejack")))),
                Box::new(Expr::Val(Value::Lit(Literal::Number(50f64)))),
            )
        ))
    )
}

#[test]
fn parses_concat() {
    assert_eq!(
        expr("Applejack\" jugs of cider on the wall\""),
        Ok((
            "",
            Expr::Concat(vec![
                Expr::Val(Value::Var(Variable("Applejack"))),
                Expr::Val(Value::Lit(Literal::Chars(" jugs of cider on the wall")))
            ])
        ))
    );
    assert_eq!(
        expr("\"It needs to be about \" 20 \"% cooler\""),
        Ok((
            "",
            Expr::Concat(vec![
                Expr::Val(Value::Lit(Literal::Chars("It needs to be about "))),
                Expr::Val(Value::Lit(Literal::Number(20f64))),
                Expr::Val(Value::Lit(Literal::Chars("% cooler")))
            ])
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
    assert_eq!(
        statement("I said \"It needs to be about \" 20 \"% cooler\"."),
        Ok((
            "",
            Statement::Print(Expr::Concat(vec![
                Expr::Val(Value::Lit(Literal::Chars("It needs to be about "))),
                Expr::Val(Value::Lit(Literal::Number(20f64))),
                Expr::Val(Value::Lit(Literal::Chars("% cooler")))
            ]))
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
                Some(Number),
                None,
                false,
            )
        ))
    );
    assert_eq!(
        statement("Did you know that Applejack's hat has the name \"Talluah\"?"),
        Ok((
            "",
            Statement::Declare(
                Variable("Applejack's hat"),
                Some(Chars),
                Some(Expr::Val(Value::Lit(Literal::Chars("Talluah")))),
                false,
            )
        ))
    );
    assert_eq!(
        statement("Did you know that Pinkie Pie always is right?"),
        Ok((
            "",
            Statement::Declare(
                Variable("Pinkie Pie"),
                None,
                Some(Expr::Val(Value::Lit(Literal::Boolean(true)))),
                true,
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
                Expr::Val(Value::Lit(Literal::Number(11f64))),
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
                    Box::new(Expr::Val(Value::Lit(Literal::Number(1f64)))),
                ),
            )
        ))
    );
}

#[test]
fn parses_if_else_statement() {
    assert_eq!(
        statement("When true, I said \"Always be honest\". That's what I would do."),
        Ok((
            "",
            Statement::If(
                Expr::Val(Value::Lit(Literal::Boolean(true))),
                vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
                    "Always be honest"
                ))))],
                vec![],
            )
        ))
    );
    assert_eq!(
        statement("When true, I said \"Always be honest\". Otherwise, I said \"Never be honest\". That's what I would do."),
        Ok((
            "",
            Statement::If(
                Expr::Val(Value::Lit(Literal::Boolean(true))),
                vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
                    "Always be honest"
                ))))],
                vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
                    "Never be honest"
                ))))],
            )
        ))
    );
}

#[test]
fn parses_while_statement() {
    assert_eq!(
        statement("As long as true, I said \"I'm cool!\". That's what I did."),
        Ok((
            "",
            Statement::While(
                Expr::Val(Value::Lit(Literal::Boolean(true))),
                vec![Statement::Print(Expr::Val(Value::Lit(Literal::Chars(
                    "I'm cool!"
                ))))],
            )
        ))
    );
}

#[test]
fn parses_increment_statement() {
    assert_eq!(
        statement("Spike got one more."),
        Ok(("", Statement::Increment(Variable("Spike"))))
    );
}

#[test]
fn parses_decrement_statement() {
    assert_eq!(
        statement("Spike got one less."),
        Ok(("", Statement::Decrement(Variable("Spike"))))
    );
}

#[test]
fn parses_call_statement() {
    assert_eq!(
        statement("I remembered how to fly."),
        Ok(("", Statement::Call(Variable("how to fly"))))
    );
}

#[test]
fn parses_return_statement() {
    assert_eq!(
        statement("Then you get a pie!"),
        Ok((
            "",
            Statement::Return(Expr::Val(Value::Var(Variable("a pie"))))
        ))
    );
}

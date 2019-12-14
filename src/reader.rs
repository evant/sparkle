use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_till1, take_while};
use nom::character::complete::space1;
use nom::combinator::{complete, cond, map, opt, recognize};
use nom::error::{convert_error, ErrorKind, ParseError, VerboseError};
use nom::multi::{fold_many0, many0, many1, separated_nonempty_list};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;

use crate::error::ReportError;
use crate::pst::{
    Arg, BinOperator, Call, Declaration, DeclareVar, Expr, Literal, Paragraph, Report, Statement,
    Variable,
};
use crate::types::Type::{Array, Boolean, Chars, Number};
use crate::types::{ArrayType, Type};

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
        keyword_infix_add(true),
        keyword_infix_sub,
        keyword_infix_mul,
        keyword_infix_div,
        keyword_increment,
        keyword_decrement,
        keyword_declare_paragraph_type,
        keyword_using,
        keyword_the_next,
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
            many0(terminated(
                alt((var_declaration, paragraph_declaration)),
                whitespace0,
            )),
            report_closing,
        )),
        |(name, declarations, writer)| Report {
            name,
            writer,
            declarations,
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

fn paragraph_declaration(s: &str) -> ReadResult<Declaration> {
    map(paragraph, Declaration::Paragraph)(s)
}

fn paragraph(s: &str) -> ReadResult<Paragraph> {
    map(
        tuple((
            opt(terminated(tag("Today"), whitespace1)),
            terminated(declare_paragraph, whitespace0),
            many0(statement),
            paragraph_closing,
        )),
        |(today, (name, return_type, args), statements, closing_name)| Paragraph {
            name,
            closing_name,
            mane: today.is_some(),
            args,
            return_type,
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

fn keyword_using(s: &str) -> ReadResult<&str> {
    tag("using")(s)
}

fn declare_paragraph(s: &str) -> ReadResult<(&str, Option<Type>, Vec<Arg>)> {
    terminated(
        tuple((
            preceded(preceded(keyword_declare_paragraph, whitespace0), identifier),
            opt(preceded(
                whitespace_delim1(keyword_declare_paragraph_type),
                declare_type,
            )),
            map(
                opt(preceded(
                    whitespace_delim1(keyword_using),
                    separated_nonempty_list(whitespace_delim1(tag("and")), paragraph_arg),
                )),
                |args| args.unwrap_or_else(|| vec![]),
            ),
        )),
        punctuation,
    )(s)
}

fn paragraph_arg(s: &str) -> ReadResult<Arg> {
    map(
        separated_pair(declare_type, whitespace1, identifier),
        |(type_, name)| Arg(type_, name),
    )(s)
}

fn statement(s: &str) -> ReadResult<Statement> {
    terminated(
        alt((
            print_statement,
            read_statement,
            declare_statement,
            assign_statement,
            if_statement,
            while_statement,
            do_while_statement,
            increment_statement,
            decrement_statement,
            call_statement,
            return_statement,
        )),
        tuple((whitespace0, punctuation, whitespace0)),
    )(s)
}

fn print_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(preceded(keyword_print, whitespace1), expr(true)),
        Statement::Print,
    )(s)
}

fn keyword_read(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("I"),
        whitespace1,
        alt((tag("heard"), tag("read"), tag("asked"))),
    )))(s)
}

fn keyword_the_next(s: &str) -> ReadResult<&str> {
    recognize(tuple((tag("the"), whitespace1, tag("next"))))(s)
}

fn read_statement(s: &str) -> ReadResult<Statement> {
    map(
        preceded(
            preceded(keyword_read, whitespace1),
            tuple((
                var,
                opt(preceded(whitespace_delim1(keyword_the_next), type_)),
                opt(preceded(whitespace1, expr(true))),
            )),
        ),
        |(var, type_, prompt)| Statement::Read(var, type_, prompt),
    )(s)
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
    map(declare_var, Statement::Declare)(s)
}

fn var_declaration(s: &str) -> ReadResult<Declaration> {
    map(
        terminated(declare_var, tuple((whitespace0, punctuation, whitespace0))),
        Declaration::Var,
    )(s)
}

fn declare_var(s: &str) -> ReadResult<DeclareVar> {
    map(
        preceded(
            preceded(keyword_declare_statement, whitespace1),
            tuple((
                identifier,
                preceded(whitespace1, terminated(
                    map(opt(terminated(keyword_always, whitespace1)), |always| {
                        always.is_some()
                    }),
                    keyword_declare,
                )),
                alt((
                    pair(
                        map(preceded(whitespace1, declare_array_type), Some),
                        opt(preceded(whitespace1, separated_nonempty_list(
                            whitespace_delim1(tag("and")),
                            expr(false),
                        ))),
                    ),
                    pair(
                        opt(preceded(whitespace1, declare_type)),
                        opt(map(preceded(whitespace1, expr(true)), |e| vec![e])),
                    ),
                )),
            )),
        ),
        |(name, is_const, (type_, expr))| DeclareVar(Variable(name), type_, expr, is_const),
    )(s)
}

fn assign_statement(s: &str) -> ReadResult<Statement> {
    map(
        separated_pair(var, whitespace_delim(keyword_assign), expr(true)),
        |(var, expr)| Statement::Assign(var, expr),
    )(s)
}

fn declare_type(s: &str) -> ReadResult<Type> {
    preceded(terminated(keyword_declare_type, whitespace1), type_)(s)
}

fn declare_array_type(s: &str) -> ReadResult<Type> {
    preceded(opt(terminated(keyword_declare_array_type, whitespace1)), type_array)(s)
}

fn type_(s: &str) -> ReadResult<Type> {
    alt((type_number, type_chars, type_boolean))(s)
}

fn keyword_declare_type(s: &str) -> ReadResult<&str> {
    alt((tag("a"), tag("the")))(s)
}

fn keyword_declare_array_type(s: &str) -> ReadResult<&str> {
    alt((tag("many"), tag("the")))(s)
}

fn keyword_type_number(s: &str) -> ReadResult<&str> {
    tag("number")(s)
}

fn type_number(s: &str) -> ReadResult<Type> {
    map(keyword_type_number, |_| Number)(s)
}

fn keyword_type_chars(s: &str) -> ReadResult<&str> {
    alt((
        tag("word"),
        tag("phrase"),
        tag("sentence"),
        tag("quote"),
        tag("name"),
    ))(s)
}

fn type_chars(s: &str) -> ReadResult<Type> {
    map(keyword_type_chars, |_| Chars)(s)
}

fn keyword_type_boolean(s: &str) -> ReadResult<&str> {
    alt((tag("logic"), tag("argument")))(s)
}

fn type_boolean(s: &str) -> ReadResult<Type> {
    map(keyword_type_boolean, |_| Boolean)(s)
}

fn type_array(s: &str) -> ReadResult<Type> {
    map(
        alt((type_number_array, type_chars_array, type_boolean_array)),
        Array,
    )(s)
}

fn type_number_array(s: &str) -> ReadResult<ArrayType> {
    map(keyword_type_number_array, |_| ArrayType::Number)(s)
}

fn keyword_type_number_array(s: &str) -> ReadResult<&str> {
    tag("numbers")(s)
}

fn type_chars_array(s: &str) -> ReadResult<ArrayType> {
    map(keyword_type_chars_array, |_| ArrayType::Chars)(s)
}

fn keyword_type_chars_array(s: &str) -> ReadResult<&str> {
    alt((
        tag("words"),
        tag("phrases"),
        tag("sentences"),
        tag("quotes"),
        tag("names"),
    ))(s)
}

fn type_boolean_array(s: &str) -> ReadResult<ArrayType> {
    map(keyword_type_boolean_array, |_| ArrayType::Boolean)(s)
}

fn keyword_type_boolean_array(s: &str) -> ReadResult<&str> {
    tag("arguments")(s)
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
                terminated(expr(true), opt(preceded(whitespace1, tag("then")))),
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
    preceded(
        terminated(keyword_declare_while, whitespace1),
        terminated(expr(true), punctuation),
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

fn do_while_statement(s: &str) -> ReadResult<Statement> {
    map(
        tuple((
            terminated(do_while_declaration, whitespace0),
            many0(statement),
            do_while_closing,
        )),
        |(_, body, cond)| Statement::DoWhile(cond, body),
    )(s)
}

fn keyword_do_while_declaration(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("Here's"),
        whitespace1,
        tag("what"),
        whitespace1,
        tag("I"),
        whitespace1,
        tag("did"),
    )))(s)
}

fn do_while_declaration(s: &str) -> ReadResult<&str> {
    terminated(
        keyword_do_while_declaration,
        terminated(whitespace0, punctuation),
    )(s)
}

fn keyword_do_while_closing(s: &str) -> ReadResult<&str> {
    recognize(tuple((
        tag("I"),
        whitespace1,
        tag("did"),
        whitespace1,
        tag("this"),
        whitespace1,
        alt((
            tag("while"),
            recognize(tuple((
                tag("as"),
                whitespace1,
                tag("long"),
                whitespace1,
                tag("as"),
            ))),
        )),
    )))(s)
}

fn do_while_closing(s: &str) -> ReadResult<Expr> {
    preceded(
        terminated(keyword_do_while_closing, whitespace1),
        expr(true),
    )(s)
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
        preceded(terminated(keyword_call, whitespace1), call),
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
        preceded(terminated(keyword_return, whitespace1), expr(true)),
        Statement::Return,
    )(s)
}

fn expr_term<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    alt((
        prefix_term(allow_infix_and),
        infix_term(allow_infix_and),
        value_expr(allow_infix_and),
    ))
}

fn expr<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    move |s| {
        let expr_term = expr_term(allow_infix_and);
        let (mut rest, mut e) = expr_term(s)?;
        let mut acc: Vec<Expr> = vec![];
        loop {
            let maybe_rest = whitespace0(rest)?.0;
            // Alternate between expressions and char literals
            let next_parser: Box<dyn Fn(&'a str) -> ReadResult<Expr>> = if is_chars_literal(&e) {
                Box::new(|s| expr_term(s))
            } else {
                Box::new(expr_string)
            };
            acc.push(e);
            let (next_rest, next_e) = match next_parser(maybe_rest) {
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
}

fn expr_string(s: &str) -> ReadResult<Expr> {
    map(string, Expr::Lit)(s)
}

fn is_chars_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Lit(Literal::Chars(_)) => true,
        _ => false,
    }
}

fn call_expr(s: &str) -> ReadResult<Expr> {
    map(call, Expr::Call)(s)
}

fn value_expr<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    alt((prefix_not(allow_infix_and), lit_expr, call_expr))
}

fn literal(s: &str) -> ReadResult<Literal> {
    alt((string, number, boolean))(s)
}

fn var(s: &str) -> ReadResult<Variable> {
    map(identifier, Variable)(s)
}

fn call(s: &str) -> ReadResult<Call> {
    map(
        pair(
            identifier,
            opt(preceded(
                whitespace_delim1(tag("using")),
                separated_nonempty_list(whitespace_delim1(tag("and")), expr(false)),
            )),
        ),
        |(name, args)| Call(name, args.unwrap_or_else(|| vec![])),
    )(s)
}

fn lit_expr(s: &str) -> ReadResult<Expr> {
    map(literal, Expr::Lit)(s)
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

fn infix_term<'a>(allow_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    move |s| {
        let value_expr = value_expr(allow_and);
        let (s, init) = value_expr(s)?;
        fold_many0(
            pair(infix_op(allow_and), value_expr),
            init,
            |acc, (op, val)| Expr::BinOp(op, Box::new(acc), Box::new(val)),
        )(s)
    }
}

fn prefix_term<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    alt((
        prefix_add(allow_infix_and),
        prefix_sub(allow_infix_and),
        prefix_mul(allow_infix_and),
        prefix_div(allow_infix_and),
        prefix_xor(allow_infix_and),
    ))
}

fn infix_op<'a>(allow_and: bool) -> impl Fn(&'a str) -> ReadResult<BinOperator> {
    alt((
        infix_add_op(allow_and),
        infix_sub_op,
        infix_mul_op,
        infix_div_op,
        map(cond(allow_and, infix_and_op), |op| op.unwrap()),
        infix_or_op,
        infix_lt_op,
        infix_gt_op,
        infix_lte_op,
        infix_gte_op,
        infix_neq_op,
        infix_eq_op,
    ))
}

fn keyword_infix_add<'a>(allow_and: bool) -> impl Fn(&'a str) -> ReadResult<&'a str> {
    alt((
        recognize(tuple((tag("added"), whitespace1, tag("to")))),
        tag("plus"),
        recognize(cond(allow_and, tag("and"))),
    ))
}

fn infix_add_op<'a>(allow_and: bool) -> impl Fn(&'a str) -> ReadResult<BinOperator> {
    map(whitespace_delim(keyword_infix_add(allow_and)), |_| {
        BinOperator::AddOrAnd
    })
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

fn prefix_add<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    map(
        prefix(
            tag("add"),
            tag("and"),
            value_expr(allow_infix_and),
            value_expr(allow_infix_and),
        ),
        |(left, right)| Expr::BinOp(BinOperator::AddOrAnd, Box::new(left), Box::new(right)),
    )
}

fn prefix_sub<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
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
                value_expr(allow_infix_and),
                value_expr(allow_infix_and),
            ),
            prefix(
                tag("subtract"),
                tag("from"),
                value_expr(allow_infix_and),
                value_expr(allow_infix_and),
            ),
        )),
        |(left, right)| Expr::BinOp(BinOperator::Sub, Box::new(left), Box::new(right)),
    )
}

fn prefix_mul<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    map(
        alt((
            prefix(
                tag("multiply"),
                alt((tag("by"), tag("and"))),
                value_expr(allow_infix_and),
                value_expr(allow_infix_and),
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
                value_expr(allow_infix_and),
                value_expr(allow_infix_and),
            ),
        )),
        |(left, right)| Expr::BinOp(BinOperator::Mul, Box::new(left), Box::new(right)),
    )
}

fn prefix_div<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    map(
        prefix(
            tag("divide"),
            alt((tag("by"), tag("and"))),
            value_expr(allow_infix_and),
            value_expr(allow_infix_and),
        ),
        |(left, right)| Expr::BinOp(BinOperator::Div, Box::new(left), Box::new(right)),
    )
}

fn infix_and_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(tag("and")), |_| BinOperator::AddOrAnd)(s)
}

fn infix_or_op(s: &str) -> ReadResult<BinOperator> {
    map(whitespace_delim(tag("or")), |_| BinOperator::Or)(s)
}

fn prefix_xor<'a>(allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    map(
        prefix(
            tag("either"),
            tag("or"),
            value_expr(allow_infix_and),
            value_expr(allow_infix_and),
        ),
        |(left, right)| Expr::BinOp(BinOperator::EitherOr, Box::new(left), Box::new(right)),
    )
}

fn prefix_not<'a>(_allow_infix_and: bool) -> impl Fn(&'a str) -> ReadResult<Expr> {
    map(
        preceded(
            terminated(tag("not"), whitespace1),
            /*TODO: figure out how to allow not not ..., right now it creates an recursive type.*/
            alt((lit_expr, call_expr)),
        ),
        |e| Expr::Not(Box::new(e)),
    )
}

fn boolean(s: &str) -> ReadResult<Literal> {
    preceded(
        opt(tuple((
            keyword_declare_type,
            whitespace1,
            keyword_type_boolean,
            whitespace1,
        ))),
        alt((true_, false_)),
    )(s)
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
    preceded(
        opt(tuple((
            keyword_declare_type,
            whitespace1,
            keyword_type_chars,
            whitespace1,
        ))),
        alt((
            // is_not fails on empty string, so special-case that.
            map(tag("\"\""), |_| Literal::Chars("")),
            map(delimited(is_a("\""), is_not("\""), is_a("\"")), |s| {
                Literal::Chars(s)
            }),
        )),
    )(s)
}

fn number(s: &str) -> ReadResult<Literal> {
    map(
        preceded(
            opt(tuple((
                keyword_declare_type,
                whitespace1,
                keyword_type_number,
                whitespace1,
            ))),
            double,
        ),
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
fn parses_declare_paragraph() {
    assert_eq!(
        declare_paragraph("I learned how to fly."),
        Ok(("", ("how to fly", None, vec![])))
    );
    assert_eq!(
        declare_paragraph("I learned to say hello with a number:"),
        Ok(("", ("to say hello", Some(Number), vec![])))
    );
    assert_eq!(
        declare_paragraph("I learned to make friends with a phrase using the number of elements of harmony and the word hello:"),
        Ok(("", ("to make friends", Some(Chars), vec![Arg(Number, "of elements of harmony"), Arg(Chars, "hello")])))
    );
}

#[test]
fn parses_arg() {
    assert_eq!(
        paragraph_arg("the word hello"),
        Ok(("", Arg(Chars, "hello")))
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
                statements: vec![Statement::Print(Expr::Lit(Literal::Chars("Fly!")))],
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
                    Statement::Print(Expr::Lit(Literal::Chars("Fly1!"))),
                    Statement::Print(Expr::BinOp(
                        BinOperator::AddOrAnd,
                        Box::new(Expr::Lit(Literal::Number(5f64))),
                        Box::new(Expr::Lit(Literal::Number(6f64))),
                    )),
                    Statement::Print(Expr::Lit(Literal::Boolean(true))),
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

        Did you know that I had 100 (apples)?
    Your faithful student: Twilight Sparkle.

    P.S. This is ignored"
        ),
        Ok((
            "",
            Report {
                name: "An example letter",
                declarations: vec![
                    Declaration::Paragraph(Paragraph {
                        name: "how to fly",
                        closing_name: "how to fly",
                        mane: true,
                        args: vec![],
                        return_type: None,
                        statements: vec![Statement::Print(Expr::Lit(Literal::Chars("Fly!")))],
                    }),
                    Declaration::Var(DeclareVar(
                        Variable("I"),
                        None,
                        Some(vec![Expr::Lit(Literal::Number(100f64))]),
                        false,
                    ))
                ],
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
    assert_eq!(literal("-1.6"), Ok(("", Literal::Number(-1.6f64))));
}

#[test]
fn parses_infix_op() {
    assert_eq!(
        infix_op(true)(" added to "),
        Ok(("", BinOperator::AddOrAnd))
    );
    assert_eq!(infix_op(true)(" minus "), Ok(("", BinOperator::Sub)));
    assert_eq!(
        infix_op(true)(" multiplied with "),
        Ok(("", BinOperator::Mul))
    );
    assert_eq!(infix_op(true)(" divided by "), Ok(("", BinOperator::Div)));
    assert_eq!(infix_op(true)(" or "), Ok(("", BinOperator::Or)));
}

#[test]
fn parses_infix_term() {
    assert_eq!(
        infix_term(true)("1"),
        Ok(("", Expr::Lit(Literal::Number(1f64))))
    );
    assert_eq!(
        infix_term(true)("1 added to 2"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Lit(Literal::Number(1f64))),
                Box::new(Expr::Lit(Literal::Number(2f64))),
            )
        ))
    );
    assert_eq!(
        infix_term(true)("2 plus 1 times 3"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Mul,
                Box::new(Expr::BinOp(
                    BinOperator::AddOrAnd,
                    Box::new(Expr::Lit(Literal::Number(2f64))),
                    Box::new(Expr::Lit(Literal::Number(1f64))),
                )),
                Box::new(Expr::Lit(Literal::Number(3f64))),
            )
        ))
    );
    assert_eq!(
        infix_term(true)("true and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Lit(Literal::Boolean(true))),
                Box::new(Expr::Lit(Literal::Boolean(false))),
            )
        ))
    );
    assert_eq!(
        infix_term(true)("true or false and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::BinOp(
                    BinOperator::Or,
                    Box::new(Expr::Lit(Literal::Boolean(true))),
                    Box::new(Expr::Lit(Literal::Boolean(false))),
                )),
                Box::new(Expr::Lit(Literal::Boolean(false))),
            )
        ))
    );
}

#[test]
fn parses_prefix_term() {
    assert_eq!(
        prefix_term(true)("add 1 and 2"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Lit(Literal::Number(1f64))),
                Box::new(Expr::Lit(Literal::Number(2f64))),
            )
        ))
    );
    assert_eq!(
        prefix_term(true)("the difference between 2 and 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Sub,
                Box::new(Expr::Lit(Literal::Number(2f64))),
                Box::new(Expr::Lit(Literal::Number(1f64))),
            )
        ))
    );
    assert_eq!(
        prefix_term(true)("the product of 2 and 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Mul,
                Box::new(Expr::Lit(Literal::Number(2f64))),
                Box::new(Expr::Lit(Literal::Number(1f64))),
            )
        ))
    );
    assert_eq!(
        prefix_term(true)("divide 2 by 1"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Div,
                Box::new(Expr::Lit(Literal::Number(2f64))),
                Box::new(Expr::Lit(Literal::Number(1f64))),
            )
        ))
    );
    assert_eq!(
        expr(true)("either true or false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::EitherOr,
                Box::new(Expr::Lit(Literal::Boolean(true))),
                Box::new(Expr::Lit(Literal::Boolean(false))),
            )
        ))
    );
}

#[test]
fn parses_not() {
    assert_eq!(
        prefix_not(true)("not true"),
        Ok(("", Expr::Not(Box::new(Expr::Lit(Literal::Boolean(true))))))
    );
    assert_eq!(
        prefix_not(true)("not a tree"),
        Ok(("", Expr::Not(Box::new(Expr::Call(Call("a tree", vec![]))))))
    );
    assert_eq!(
        expr(true)("not true and false"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::AddOrAnd,
                Box::new(Expr::Not(Box::new(Expr::Lit(Literal::Boolean(true))))),
                Box::new(Expr::Lit(Literal::Boolean(false))),
            )
        ))
    );
}

#[test]
fn parses_comparison() {
    assert_eq!(
        expr(true)("Rainbow Dash is cool"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::Equal,
                Box::new(Expr::Call(Call("Rainbow Dash", vec![]))),
                Box::new(Expr::Call(Call("cool", vec![]))),
            )
        ))
    );
    assert_eq!(
        expr(true)("Fluttershy isn't loud"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::NotEqual,
                Box::new(Expr::Call(Call("Fluttershy", vec![]))),
                Box::new(Expr::Call(Call("loud", vec![]))),
            )
        ))
    );
    assert_eq!(
        expr(true)("the number of cupcakes is less than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::LessThan,
                Box::new(Expr::Call(Call("the number of cupcakes", vec![]))),
                Box::new(Expr::Lit(Literal::Number(10f64))),
            )
        ))
    );
    assert_eq!(
        expr(true)("the number of pies is not less than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThanOrEqual,
                Box::new(Expr::Call(Call("the number of pies", vec![]))),
                Box::new(Expr::Lit(Literal::Number(10f64))),
            )
        ))
    );
    assert_eq!(
        expr(true)("the number of cakes is more than 10"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThan,
                Box::new(Expr::Call(Call("the number of cakes", vec![]))),
                Box::new(Expr::Lit(Literal::Number(10f64))),
            )
        ))
    );
    assert_eq!(
        expr(true)("the number of cute animals isn't greater than 100"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::LessThanOrEqual,
                Box::new(Expr::Call(Call("the number of cute animals", vec![]))),
                Box::new(Expr::Lit(Literal::Number(100f64))),
            )
        ))
    );
    assert_eq!(
        expr(true)("Applejack has more than 50"),
        Ok((
            "",
            Expr::BinOp(
                BinOperator::GreaterThan,
                Box::new(Expr::Call(Call("Applejack", vec![]))),
                Box::new(Expr::Lit(Literal::Number(50f64))),
            )
        ))
    )
}

#[test]
fn parses_concat() {
    assert_eq!(
        expr(true)("Applejack\" jugs of cider on the wall\""),
        Ok((
            "",
            Expr::Concat(vec![
                Expr::Call(Call("Applejack", vec![])),
                Expr::Lit(Literal::Chars(" jugs of cider on the wall"))
            ])
        ))
    );
    assert_eq!(
        expr(true)("\"It needs to be about \" 20 \"% cooler\""),
        Ok((
            "",
            Expr::Concat(vec![
                Expr::Lit(Literal::Chars("It needs to be about ")),
                Expr::Lit(Literal::Number(20f64)),
                Expr::Lit(Literal::Chars("% cooler"))
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
                Box::new(Expr::Lit(Literal::Number(1f64))),
                Box::new(Expr::Lit(Literal::Number(2f64))),
            ))
        ))
    );
    assert_eq!(
        statement("I sang the elements of harmony count."),
        Ok((
            "",
            Statement::Print(Expr::Call(Call("the elements of harmony count", vec![])))
        ))
    );
    assert_eq!(
        statement("I said \"It needs to be about \" 20 \"% cooler\"."),
        Ok((
            "",
            Statement::Print(Expr::Concat(vec![
                Expr::Lit(Literal::Chars("It needs to be about ")),
                Expr::Lit(Literal::Number(20f64)),
                Expr::Lit(Literal::Chars("% cooler"))
            ]))
        ))
    );
    assert_eq!(
        statement("I said Tantabus\"\"!"),
        Ok((
            "",
            Statement::Print(Expr::Concat(vec![
                Expr::Call(Call("Tantabus", vec![])),
                Expr::Lit(Literal::Chars(""))
            ]))
        ))
    )
}

#[test]
fn parses_declare_statement() {
    assert_eq!(
        statement("Did you know that the elements of harmony count is a number?"),
        Ok((
            "",
            Statement::Declare(DeclareVar(
                Variable("the elements of harmony count"),
                Some(Number),
                None,
                false,
            ))
        ))
    );
    assert_eq!(
        statement("Did you know that Applejack's hat has the name \"Talluah\"?"),
        Ok((
            "",
            Statement::Declare(DeclareVar(
                Variable("Applejack's hat"),
                Some(Chars),
                Some(vec![Expr::Lit(Literal::Chars("Talluah"))]),
                false,
            ))
        ))
    );
    assert_eq!(
        statement("Did you know that Pinkie Pie always is right?"),
        Ok((
            "",
            Statement::Declare(DeclareVar(
                Variable("Pinkie Pie"),
                None,
                Some(vec![Expr::Lit(Literal::Boolean(true))]),
                true,
            ))
        ))
    );
    assert_eq!(
        statement("Did you know that cake has the names \"chocolate\" and \"apple cinnamon\" and \"fruit\"?"),
        Ok((
            "",
            Statement::Declare(DeclareVar(Variable("cake"), Some(Array(ArrayType::Chars)), Some(vec![
                Expr::Lit(Literal::Chars("chocolate")),
                Expr::Lit(Literal::Chars("apple cinnamon")),
                Expr::Lit(Literal::Chars("fruit")),
            ]), false))
        ))
    );
}

#[test]
fn parses_assign_statement() {
    assert_eq!(
        statement("Spike's age is now 11!"),
        Ok((
            "",
            Statement::Assign(Variable("Spike's age"), Expr::Lit(Literal::Number(11f64)))
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
                    Box::new(Expr::Lit(Literal::Number(10f64))),
                    Box::new(Expr::Lit(Literal::Number(1f64))),
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
                Expr::Lit(Literal::Boolean(true)),
                vec![Statement::Print(Expr::Lit(Literal::Chars(
                    "Always be honest"
                )))],
                vec![],
            )
        ))
    );
    assert_eq!(
        statement("When true, I said \"Always be honest\". Otherwise, I said \"Never be honest\". That's what I would do."),
        Ok((
            "",
            Statement::If(
                Expr::Lit(Literal::Boolean(true)),
                vec![Statement::Print(Expr::Lit(Literal::Chars(
                    "Always be honest"
                )))],
                vec![Statement::Print(Expr::Lit(Literal::Chars(
                    "Never be honest"
                )))],
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
                Expr::Lit(Literal::Boolean(true)),
                vec![Statement::Print(Expr::Lit(Literal::Chars("I'm cool!")))],
            )
        ))
    );
}

#[test]
fn parses_do_while_statement() {
    assert_eq!(
        statement("Here's what I did, I said \"I'm hungry\". I did this as long as true."),
        Ok((
            "",
            Statement::DoWhile(
                Expr::Lit(Literal::Boolean(true)),
                vec![Statement::Print(Expr::Lit(Literal::Chars("I'm hungry")))],
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
        Ok(("", Statement::Call(Call("how to fly", vec![]))))
    );
    assert_eq!(
        statement("I remembered how to fly using \"Twilight Sparkle\"."),
        Ok((
            "",
            Statement::Call(Call(
                "how to fly",
                vec![Expr::Lit(Literal::Chars("Twilight Sparkle"))],
            ))
        ))
    );
    assert_eq!(
        statement("I remembered how to fly using Rainbow Dash and Fluttershy."),
        Ok((
            "",
            Statement::Call(Call(
                "how to fly",
                vec![
                    Expr::Call(Call("Rainbow Dash", vec![])),
                    Expr::Call(Call("Fluttershy", vec![]))
                ],
            ))
        ))
    )
}

#[test]
fn parses_call_expr() {
    assert_eq!(
        expr(true)("how to fly using Rainbow Dash and Fluttershy"),
        Ok((
            "",
            Expr::Call(Call(
                "how to fly",
                vec![
                    Expr::Call(Call("Rainbow Dash", vec![])),
                    Expr::Call(Call("Fluttershy", vec![]))
                ],
            ))
        ))
    )
}

#[test]
fn parses_return_statement() {
    assert_eq!(
        statement("Then you get a pie!"),
        Ok(("", Statement::Return(Expr::Call(Call("a pie", vec![])))))
    );
}

#[test]
fn parses_read_statement() {
    assert_eq!(
        statement("I heard Applejack's speech."),
        Ok((
            "",
            Statement::Read(Variable("Applejack's speech"), None, None)
        ))
    );
    assert_eq!(
        statement("I read Twilight the next number."),
        Ok((
            "",
            Statement::Read(Variable("Twilight"), Some(Number), None)
        ))
    );
    assert_eq!(
        statement("I asked Spike \"How many gems are left?\""),
        Ok((
            "",
            Statement::Read(
                Variable("Spike"),
                None,
                Some(Expr::Lit(Literal::Chars("How many gems are left?"))),
            )
        ))
    );
    assert_eq!(
        statement("I asked Applejack the next number \"How many apples do you have?\""),
        Ok((
            "",
            Statement::Read(
                Variable("Applejack"),
                Some(Number),
                Some(Expr::Lit(Literal::Chars("How many apples do you have?"))),
            )
        ))
    );
}

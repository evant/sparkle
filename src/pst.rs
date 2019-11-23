use std::borrow::Cow;

#[derive(Debug, PartialEq)]
pub struct Paragraph<'a> {
    pub name: &'a str,
    pub closing_name: &'a str,
    pub statements: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Report<'a> {
    pub name: &'a str,
    pub paragraphs: Vec<Paragraph<'a>>,
    pub writer: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    NBinOp(NBinOperator, Value<'a>, Value<'a>),
    BBinOp(BBinOperator, Value<'a>, Value<'a>),
    Not(Value<'a>),
    Val(Value<'a>),
}

#[derive(Debug, PartialEq)]
pub enum NBinOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum BBinOperator {
    And,
    Or,
    EitherOr,
}

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Lit(Literal<'a>),
}

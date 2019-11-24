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

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    String(&'a str),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    BinOp(BinOperator, Box<Expr<'a>>, Box<Expr<'a>>),
    Not(Value<'a>),
    Val(Value<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOperator {
    AddOrAnd,
    Sub,
    Mul,
    Div,
    Or,
    EitherOr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Lit(Literal<'a>),
}

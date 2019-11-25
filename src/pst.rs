use crate::pst::Value::Lit;
use crate::types::Type;

#[derive(Debug, PartialEq)]
pub struct Paragraph<'a> {
    pub name: &'a str,
    pub closing_name: &'a str,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Print(Expr<'a>),
    Declare(Variable<'a>, Type, Option<Literal<'a>>),
    Assign(Variable<'a>, Expr<'a>),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable<'a>(pub &'a str);

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
    Var(Variable<'a>),
}

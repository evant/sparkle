use crate::types::Type;

#[derive(Debug, PartialEq)]
pub struct Paragraph<'a> {
    pub name: &'a str,
    pub closing_name: &'a str,
    pub mane: bool,
    pub args: Vec<Arg<'a>>,
    pub return_type: Option<Type>,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Print(Expr<'a>),
    Declare(Variable<'a>, Option<Type>, Option<Expr<'a>>, bool),
    Assign(Variable<'a>, Expr<'a>),
    Increment(Variable<'a>),
    Decrement(Variable<'a>),
    If(Expr<'a>, Vec<Statement<'a>>, Vec<Statement<'a>>),
    While(Expr<'a>, Vec<Statement<'a>>),
    Call(Variable<'a>),
    Return(Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Report<'a> {
    pub name: &'a str,
    pub paragraphs: Vec<Paragraph<'a>>,
    pub writer: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Chars(&'a str),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable<'a>(pub &'a str);

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    BinOp(BinOperator, Box<Expr<'a>>, Box<Expr<'a>>),
    Not(Value<'a>),
    Concat(Vec<Expr<'a>>),
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
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Lit(Literal<'a>),
    Var(Variable<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arg<'a>(&'a str, Type);

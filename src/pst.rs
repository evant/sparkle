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
    Declare(DeclareVar<'a>),
    Assign(LValue<'a>, Expr<'a>),
    Increment(Variable<'a>),
    Decrement(Variable<'a>),
    If(Expr<'a>, Vec<Statement<'a>>, Vec<Statement<'a>>),
    While(Expr<'a>, Vec<Statement<'a>>),
    DoWhile(Expr<'a>, Vec<Statement<'a>>),
    For(
        Option<Type>,
        Variable<'a>,
        Expr<'a>,
        Expr<'a>,
        Vec<Statement<'a>>,
    ),
    Call(Call<'a>),
    Return(Expr<'a>),
    Read(LValue<'a>, Option<Type>, Option<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct DeclareVar<'a>(
    pub Variable<'a>,
    pub Option<Type>,
    pub Option<Vec<Expr<'a>>>,
    pub bool,
);

#[derive(Debug, PartialEq)]
pub enum Declaration<'a> {
    Paragraph(Paragraph<'a>),
    Var(DeclareVar<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Report<'a> {
    pub name: &'a str,
    pub declarations: Vec<Declaration<'a>>,
    pub writer: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Chars(&'a str),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call<'a>(pub &'a str, pub Vec<Expr<'a>>);

#[derive(Debug, PartialEq, Clone)]
pub struct Index<'a>(pub &'a str, pub Box<Expr<'a>>);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable<'a>(pub &'a str);

#[derive(Debug, PartialEq, Clone)]
pub enum LValue<'a> {
    Variable(&'a str),
    Index(Index<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    BinOp(BinOperator, Box<Expr<'a>>, Box<Expr<'a>>),
    Not(Box<Expr<'a>>),
    Concat(Vec<Expr<'a>>),
    Lit(Literal<'a>),
    Call(Call<'a>),
    Index(Index<'a>),
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
pub struct Arg<'a>(pub Type, pub &'a str);

use std::borrow::Cow;

#[derive(Debug, PartialEq)]
pub struct Paragraph<'a> {
    pub name: &'a str,
    pub closing_name: &'a str,
    pub statements: Vec<Literal<'a>>,
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
}

impl<'a> Literal<'a> {
    pub fn to_cow_string(&self) -> Cow<'a, str> {
        match self {
            Literal::String(s) => (*s).into(),
            Literal::Number(n) => n.to_string().into(),
        }
    }
}
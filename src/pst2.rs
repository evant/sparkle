use logos::Source;
use nom::Slice;
use num_traits::FromPrimitive;
use num_traits::ToPrimitive;
use rowan::{Language, SyntaxKind, SyntaxNode};

use crate::error::ReportError;
use crate::lexer::Bit;
use crate::pst::{Declaration, Paragraph, Report};
use crate::reader2::Reader;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Sparkle {}

pub type SparkleNode = SyntaxNode<Sparkle>;

impl Language for Sparkle {
    type Kind = Bit;

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind {
        SyntaxKind(kind.to_u16().unwrap())
    }
}

pub trait ReaderExt<'source> {
    fn read_into_report(self) -> Result<Report<'source>, ReportError>;
}

pub trait SparkleNodeExt {
    fn to_report(&self) -> Report;
}

impl SparkleNodeExt for SparkleNode {
    fn to_report(&self) -> Report {
        Report {
            name: self
                .children()
                .find(|c| c.kind() == Bit::Topic)
                .map(|c| {
                    let token = c.first_token().unwrap();
                    token.text()
                })
                .unwrap(),
            declarations: self
                .children()
                .take_while(|c| c.kind() == Bit::Paragraph)
                .map(|c| read_declaration(c))
                .collect(),
            writer: self
                .children()
                .find(|c| c.kind() == Bit::Author)
                .map(|c| c.first_token().unwrap().text().clone())
                .unwrap(),
        }
    }
}

fn read_declaration<'source>(node: SparkleNode) -> Declaration<'source> {
    match node.kind() {
        Bit::Paragraph => Declaration::Paragraph(Paragraph {
            name: node
                .children()
                .find(|c| c.kind() == Bit::Identifier)
                .map(|c| c.first_token().unwrap().text().clone())
                .unwrap(),
            closing_name: "",
            mane: node.children().any(|c| c.kind() == Bit::Today),
            args: Vec::new(),
            return_type: None,
            statements: Vec::new(),
        }),
        Bit::Var => {
            todo!()
        }
        _ => unreachable!(),
    }
}

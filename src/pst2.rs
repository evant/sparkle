use num_traits::FromPrimitive;
use num_traits::ToPrimitive;
use rowan::{Language, SyntaxKind, SyntaxNode};

use crate::lexer::Bit;

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

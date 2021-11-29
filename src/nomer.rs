use enumset::EnumSet;
use logos::{Lexer, Logos, Source, Span};

use crate::{lexer::SparkleToken, read_error::ReadError};

pub type Result<'a, T> = std::result::Result<T, ReadError<'a>>;

pub struct Nomer<'source, Token: Logos<'source>> {
    origin: String,
    inner: Lexer<'source, Token>,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<Token>>,
}

impl<'source, Token> Nomer<'source, Token>
where
    Token: SparkleToken<'source>,
{
    pub fn new(origin: String, input: &'source str) -> Self {
        Self {
            origin,
            inner: Token::lexer(input),
            peeked: None,
        }
    }

    pub fn origin(&self) -> &str {
        &self.origin
    }

    pub fn source(&self) -> &'source str {
        self.inner.source()
    }

    pub fn slice(&self) -> &'source str {
        self.inner.slice()
    }

    pub fn span(&self) -> Span {
        self.inner.span()
    }

    pub fn line_num(&self) -> usize {
        self.inner.extras.line_num
    }

    pub fn peek(&mut self) -> Option<Token> {
        let inner = &mut self.inner;
        self.peeked
            .get_or_insert_with(|| inner.next())
            .as_ref()
            .map(|v| *v)
    }

    pub fn expect(
        &mut self,
        expected: impl Into<EnumSet<Token>>,
    ) -> Result<'source, (Token, &'source str)> {
        self.expect_with_label(expected, "What is this?".to_string())
    }

    pub fn expect_with_label(
        &mut self,
        expected: impl Into<EnumSet<Token>>,
        label: String,
    ) -> Result<'source, (Token, &'source str)> {
        let expected = expected.into();
        let actual = self.peek();
        if let Some(bit) = actual {
            if expected.contains(bit) {
                let slice = self.slice();
                self.next();
                return Ok((bit, slice));
            }
        }
        Err(ReadError::unexpected_token(&self, expected, actual, label))
    }
}

impl<'a, Token: Logos<'a> + std::fmt::Debug> Iterator for Nomer<'a, Token> {
    type Item = (Token, &'a <<Token as Logos<'a>>::Source as Source>::Slice);

    fn next(&mut self) -> Option<Self::Item> {
        let bit = match self.peeked.take() {
            Some(v) => v,
            None => self.inner.next(),
        }?;
        let text = self.inner.slice();
        Some((bit, text))
    }
}

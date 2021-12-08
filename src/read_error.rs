use std::fmt::{Display, Formatter};
use std::io::Write;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use enumset::EnumSet;
use logos::Span;

use crate::lexer::SparkleToken;
use crate::nomer::Nomer;

#[derive(Debug)]
pub struct ReadError<'source> {
    pub origin: String,
    pub text: String,
    pub line_num: usize,
    pub source: &'source str,
    pub span: (String, Span),
}

impl<'source> ReadError<'source> {
    pub fn unexpected_token<Token>(
        nom: &Nomer<'source, Token>,
        expected: EnumSet<Token>,
        actual: Option<Token>,
        label: String,
    ) -> ReadError<'source>
    where
        Token: SparkleToken<'source>,
    {
        let mut text = Vec::new();
        write!(text, "Oh my Celestia! I was expecting ").unwrap();
        if expected.len() == 1 {
            write!(text, "{}", expected.iter().next().unwrap());
        } else {
            write!(text, "any of ").unwrap();
            for (i, expected) in expected.iter().enumerate() {
                if i != 0 {
                    write!(text, ", ");
                }
                write!(text, "{}", expected);
            }
        }

        match actual {
            Some(_) => {
                write!(text, " but I read '{}' instead!", nom.slice()).unwrap();
            }
            _ => {}
        }

        ReadError {
            origin: nom.origin().to_owned(),
            text: unsafe { String::from_utf8_unchecked(text) },
            line_num: nom.line_num(),
            source: nom.source(),
            span: (label, nom.span()),
        }
    }

    pub fn mismatched_identifier<Token>(
        nom: &Nomer<'source, Token>,
        first: &'source str,
        second: &'source str,
    ) -> ReadError<'source>
    where
        Token: SparkleToken<'source>,
    {
        let mut text = Vec::new();
        write!(
            text,
            "Oh my Celestia! I was expecting '{}' to match '{}'!",
            second, first
        )
        .unwrap();
        let label = "Keep your work consistent".to_string();

        ReadError {
            origin: nom.origin().to_owned(),
            text: unsafe { String::from_utf8_unchecked(text) },
            line_num: nom.line_num(),
            source: nom.source(),
            span: (label, nom.span()),
        }
    }

    pub fn to_snippet(&self) -> Snippet {
        let (label, span) = &self.span;
        Snippet {
            title: Some(Annotation {
                label: Some(&self.text),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer: Vec::new(),
            slices: vec![Slice {
                source: self.source,
                line_start: 0usize,
                fold: true,
                origin: Some(&self.origin),
                annotations: vec![SourceAnnotation {
                    label,
                    annotation_type: AnnotationType::Error,
                    range: (span.start, span.end),
                }],
            }],
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        }
    }

    pub fn eprint(&self) {
        eprintln!("{}", DisplayList::from(self.to_snippet()));
    }
}

impl Display for ReadError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        DisplayList::from(self.to_snippet()).fmt(f)
    }
}

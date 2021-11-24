use std::fmt::{Display, format, Formatter};

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use logos::Span;
use std::io::Write;

use crate::lexer::Bit;
use crate::reader2::Nomer;

pub struct ReadError<'source> {
    pub origin: String,
    pub text: String,
    pub line_num: usize,
    pub source: &'source str,
    pub span: (String, Span),
}

impl<'source> ReadError<'source> {
    pub fn unexpected_bit(
        origin: String,
        nom: &Nomer<'source, Bit>,
        expected: Bit,
        actual: Option<Bit>,
    ) -> ReadError<'source> {
        let mut text = Vec::new();
        write!(text, "Oh my Celestia! I was expecting '{}'", expected).unwrap();
        match actual {
            Some(_) => {
                write!(text, " but I read '{}' instead!", nom.slice()).unwrap();
            }
            _ => {}
        }
        let label = match expected {
            Bit::DearPrincessCelestia => "Who is this?",
            _ => "What is this?",
        }.to_string();

        ReadError {
            origin,
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

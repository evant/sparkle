use enumset::{enum_set, EnumSet};
use logos::{Lexer, Source};
use logos::{Logos, Span};
use nom::Slice;
use rowan::{GreenNodeBuilder, Language};

use crate::lexer::{Bit, Extras, SparkleToken};
use crate::pst::{Arg, Declaration, Literal, Paragraph, ParagraphDeclaration, Report};
use crate::pst2::Sparkle;
use crate::read_error::ReadError;
use crate::types::Type;

pub struct Reader<'source> {
    nom: Nomer<'source, Bit>,
    builder: ReportBuilder<'source>,
    errors: Vec<ReadError<'source>>,
}

#[derive(Default)]
struct ReportBuilder<'source> {
    name: Option<&'source str>,
    author: Option<&'source str>,
    declarations: Vec<Declaration<'source>>,
}

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
    fn new(origin: String, input: &'source str) -> Self {
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

    fn peek(&mut self) -> Option<Token> {
        let inner = &mut self.inner;
        self.peeked
            .get_or_insert_with(|| inner.next())
            .as_ref()
            .map(|v| *v)
    }

    fn morph<Token2>(self) -> (Nomer<'source, Token2>)
    where
        Token2: Logos<'source, Source = str, Extras = Extras>,
    {
        if let Some(peeked) = self.peeked {
            panic!("must call next() if token is peeked, otherwise the peeked token would be dropped\ntoken: {:?}", peeked);
        }
        Nomer {
            origin: self.origin,
            inner: self.inner.morph(),
            peeked: None,
        }
    }

    fn expect(
        &mut self,
        expected: impl Into<EnumSet<Token>>,
    ) -> Result<(Token, &'source str), ReadError<'source>> {
        self.expect_with_label(expected, "What is this?".to_string())
    }

    fn expect_with_label(
        &mut self,
        expected: impl Into<EnumSet<Token>>,
        label: String,
    ) -> Result<(Token, &'source str), ReadError<'source>> {
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

impl<'a, Token: Logos<'a>> Iterator for Nomer<'a, Token> {
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

impl<'source> Reader<'source> {
    pub fn new(origin: String, input: &'source str) -> Self {
        Self {
            nom: Nomer::new(origin, input),
            builder: Default::default(),
            errors: Vec::new(),
        }
    }

    pub fn source(&self) -> &'source str {
        return self.nom.source();
    }

    pub fn read(mut self) -> Result<Report<'source>, ReadError<'source>> {
        let report = self.read_letter()?;
        Ok(report)
    }

    fn read_letter(&mut self) -> Result<Report<'source>, ReadError<'source>> {
        let subject = self.read_heading()?;

        //TODO: read declarations

        let author = self.read_signature()?;

        Ok(Report {
            name: subject,
            writer: author,
            declarations: Vec::new(),
        })
    }

    fn read_heading(&mut self) -> Result<&'source str, ReadError<'source>> {
        self.eat_whitespace();
        self.nom
            .expect_with_label(Bit::DearPrincessCelestia, "Who is this?".to_string())?;
        self.eat_whitespace();
        let subject = self.read_identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(subject)
    }

    fn read_signature(&mut self) -> Result<&'source str, ReadError<'source>> {
        self.eat_whitespace();
        self.nom.expect(Bit::YourFaithfulStudent)?;
        self.eat_whitespace();
        self.nom.expect(Bit::Punctuation)?;
        self.eat_whitespace();
        let author = self.read_identifier_until(Bit::Punctuation)?;

        Ok(author)
    }

    fn read_paragraph(&mut self) -> Result<Paragraph<'source>, ReadError<'source>> {
        let mane = if let Some(Bit::Today) = self.nom.peek() {
            self.nom.next();
            self.eat_whitespace();
            true
        } else {
            false
        };
        let decl = self.paragraph_opening()?;
        self.eat_whitespace();
        //TODO: statements
        let closing_topic = self.paragraph_closing()?;

        if decl.name != closing_topic {
            return Err(ReadError::mismatched_identifier(
                &self.nom,
                decl.name,
                closing_topic,
            ));
        }

        Ok(Paragraph {
            mane,
            closing_name: decl.name,
            statements: Vec::new(),
            decl,
        })
    }

    fn paragraph_opening(&mut self) -> Result<ParagraphDeclaration<'source>, ReadError<'source>> {
        self.nom.expect(Bit::ILearned)?;
        self.eat_whitespace();
        let topic =
            self.read_identifier_until(enum_set!(Bit::Punctuation | Bit::With | Bit::Using))?;
        let return_type = if let Some(Bit::With) = self.nom.peek() {
            self.nom.next();
            self.eat_whitespace();
            self.nom.expect(Bit::A)?;
            self.eat_whitespace();
            let return_type = self.declare_type()?;
            self.eat_whitespace();
            Some(return_type)
        } else {
            None
        };
        let mut args = Vec::new();
        if let Some(Bit::Using) = self.nom.peek() {
            self.nom.next();
            self.eat_whitespace();
            loop {
                let arg = self.paragraph_arg()?;
                args.push(arg);
                self.eat_whitespace();
                if let Some(Bit::And) = self.nom.peek() {
                    self.nom.next();
                    self.eat_whitespace();
                    continue;
                } else {
                    break;
                }
            }
        }

        self.nom.expect(Bit::Punctuation)?;

        Ok(ParagraphDeclaration {
            name: topic,
            args,
            return_type,
        })
    }

    fn paragraph_arg(&mut self) -> Result<Arg<'source>, ReadError<'source>> {
        self.nom.expect(enum_set!(Bit::A | Bit::The))?;
        self.eat_whitespace();
        let _type = self.declare_type()?;
        self.eat_whitespace();
        let name = self.read_identifier_until(enum_set!(Bit::Punctuation | Bit::And))?;

        Ok(Arg(_type, name))
    }

    fn declare_type(&mut self) -> Result<Type, ReadError<'source>> {
        let (_type, _) = self
            .nom
            .expect(enum_set!(Bit::Number | Bit::Chars | Bit::Boolean))?;
        Ok(match _type {
            Bit::Number => Type::Number,
            Bit::Chars => Type::Chars,
            Bit::Boolean => Type::Boolean,
            _ => unreachable!(),
        })
    }

    fn paragraph_closing(&mut self) -> Result<&'source str, ReadError<'source>> {
        self.nom.expect(Bit::ThatsAllAbout)?;
        self.eat_whitespace();
        let topic = self.read_identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(topic)
    }

    fn literal(&mut self) -> Result<Literal<'source>, ReadError<'source>> {
        return match self.nom.peek() {
            Some(Bit::Quoted) => {
                let (_, text) = self.nom.next().unwrap();
                Ok(Literal::Chars(text.slice(1..text.len() - 1).unwrap()))
            }
            other => Err(ReadError::unexpected_token(
                &self.nom,
                enum_set!(Bit::Quoted),
                other,
                "What is this?".to_string(),
            )),
        };
    }

    fn read_identifier(&mut self) -> Result<&'source str, ReadError<'source>> {
        self.read_identifier_until(EnumSet::empty())
    }

    fn read_identifier_until(
        &mut self,
        until: impl Into<EnumSet<Bit>>,
    ) -> Result<&'source str, ReadError<'source>> {
        let until = until.into();
        let start = self.nom.span().start;
        let mut end = start;
        loop {
            match self.nom.peek() {
                Some(Bit::Word) => {
                    self.nom.next();
                    end = self.nom.span().end;
                }
                Some(Bit::Whitespace) => {
                    self.nom.next();
                }
                other => {
                    if let Some(other) = other {
                        if !until.is_empty() && !until.contains(other) {
                            self.nom.next();
                            end = self.nom.span().end;
                            continue;
                        }
                    }

                    if start == end {
                        return Err(ReadError::unexpected_token(
                            &self.nom,
                            enum_set!(Bit::Word),
                            other,
                            "What is this?".to_string(),
                        ));
                    } else {
                        break;
                    }
                }
            }
        }
        let source = self.nom.source();
        Ok(unsafe { source.get_unchecked(start..end) })
    }

    fn eat_whitespace(&mut self) {
        loop {
            match self.nom.peek() {
                Some(Bit::Whitespace) | Some(Bit::LineComment) => {
                    self.nom.next();
                    continue;
                }
                Some(Bit::OpenParen) => {
                    let mut paren_count = 1;
                    loop {
                        match self.nom.peek() {
                            Some(Bit::OpenParen) => paren_count += 1,
                            Some(Bit::CloseParen) => paren_count -= 1,
                            _ => {}
                        }
                        if paren_count <= 1 {
                            break;
                        }
                        self.nom.next();
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }
}

trait GreenNodeBuilderExt {
    fn start(&mut self, bit: Bit);

    fn push(&mut self, bit: Bit, text: &str);
}

impl GreenNodeBuilderExt for GreenNodeBuilder<'_> {
    fn start(&mut self, bit: Bit) {
        self.start_node(Sparkle::kind_to_raw(bit));
    }

    fn push(&mut self, bit: Bit, text: &str) {
        self.token(Sparkle::kind_to_raw(bit), text.into())
    }
}

#[cfg(test)]
mod test {
    use std::fmt::{Debug, Display};

    use expect_test::{expect, Expect};

    use crate::read_error::ReadError;
    use crate::reader2::Reader;

    fn pst_eq(input: &str, expected: Expect) {
        pst_fn_eq(|r| r.read_letter(), input, expected);
    }

    fn pst_fn_eq<'source, F, Output, Error>(f: F, input: &'source str, expected: Expect)
    where
        F: FnOnce(&mut Reader<'source>) -> Result<Output, Error>,
        Output: Debug,
        Error: Display,
    {
        let mut reader = Reader::new("test.rs".to_string(), input);
        let output = f(&mut reader);
        match output {
            Ok(output) => {
                let string = format!("{:#?}", output);
                expected.assert_eq(&string);
            }
            Err(error) => {
                assert!(false, "{}", error);
            }
        }
    }

    fn pst_fn_errors<'source, F, Output, Error>(f: F, input: &'source str, expected: Expect)
    where
        F: FnOnce(&mut Reader<'source>) -> Result<Output, Error>,
        Output: Debug,
        Error: Display,
    {
        let mut reader = Reader::new("test.rs".to_string(), input);
        let output = f(&mut reader);
        match output {
            Ok(output) => {
                assert!(false, "{:#?}", output);
            }
            Err(error) => {
                let string = format!("{}", error);
                expected.assert_eq(&string);
            }
        }
    }

    #[test]
    fn identifier() {
        pst_fn_eq(
            |r| r.read_identifier(),
            "An example letter",
            expect![["\"An example letter\""]],
        );
        pst_fn_eq(
            |r| r.read_identifier(),
            "An example letter.",
            expect![["\"An example letter\""]],
        );
        pst_fn_eq(
            |r| r.read_identifier(),
            "An example letter .",
            expect![["\"An example letter\""]],
        );
    }

    #[test]
    fn heading() {
        pst_fn_eq(
            |r| r.read_heading(),
            "Dear Princess Celestia: An example letter.",
            expect![["\"An example letter\""]],
        );
        pst_fn_errors(
            |r| r.read_heading(),
            "Dear Princess Luna:",
            expect![[r#"
error: Oh my Celestia! I was expecting 'Dear Princess Celestia:' but I read 'Dear Princess ' instead!
--> test.rs:0:1
 |
0 | Dear Princess Luna:
 | ^^^^^^^^^^^^^^ Who is this?
 |"#]],
        );
    }

    #[test]
    fn signature() {
        pst_fn_eq(
            |r| r.read_signature(),
            "Your faithful student: Twilight Sparkle.",
            expect![["\"Twilight Sparkle\""]],
        );
        pst_fn_eq(
            |r| r.read_signature(),
            "Your faithful student, Applejack's hat!",
            expect![["\"Applejack's hat\""]],
        );
    }

    #[test]
    fn paragraph_opening() {
        pst_fn_eq(
            |r| r.paragraph_opening(),
            "I learned how to fly.",
            expect![[r#"
ParagraphDeclaration {
    name: "how to fly",
    args: [],
    return_type: None,
}"#]],
        );
        pst_fn_eq(
            |r| r.paragraph_opening(),
            "I learned to say hello with a number:",
            expect![[r#"
ParagraphDeclaration {
    name: "to say hello",
    args: [],
    return_type: Some(
        Number,
    ),
}"#]],
        );
        pst_fn_eq(
            |r| r.paragraph_opening(),
            "I learned to make friends with a phrase using the number of elements of harmony and the word hello:",
            expect![[r#"
ParagraphDeclaration {
    name: "to make friends",
    args: [
        Arg(
            Number,
            "of elements of harmony",
        ),
        Arg(
            Chars,
            "hello",
        ),
    ],
    return_type: Some(
        Chars,
    ),
}"#]],
        );
    }

    #[test]
    fn paragraph_arg() {
        pst_fn_eq(
            |r| r.paragraph_arg(),
            "the word hello",
            expect![[r#"
Arg(
    Chars,
    "hello",
)"#]],
        );
    }

    #[test]
    fn paragraph_closing() {
        pst_fn_eq(
            |r| r.paragraph_closing(),
            "That's all about how to fly.",
            expect![["\"how to fly\""]],
        );
    }

    #[test]
    fn empty_paragraph() {
        pst_fn_eq(
            |r| r.read_paragraph(),
            "Today I learned how to fly.
That's all about how to fly.",
            expect![[r#"
Paragraph {
    decl: ParagraphDeclaration {
        name: "how to fly",
        args: [],
        return_type: None,
    },
    mane: true,
    closing_name: "how to fly",
    statements: [],
}"#]],
        );
    }

    #[test]
    fn empty_letter() {
        pst_eq(
            r#"
Dear Princess Celestia: I have nothing to say.
Your faithful student: Twilight Sparkle."#,
            expect![[r#"
Report {
    name: "I have nothing to say",
    declarations: [],
    writer: "Twilight Sparkle",
}"#]],
        );
    }

    #[test]
    fn line_comment() {
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "P.S. Comment",
            expect![["()"]],
        );
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "P.S. Comment\n",
            expect![["()"]],
        );
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "P.P.P.S. Comment\n",
            expect![["()"]],
        );
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "P.P.P.S. Comment\r\n",
            expect![["()"]],
        );
    }

    #[test]
    fn multiline_comment() {
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "(Comment)",
            expect![["()"]],
        );
        pst_fn_eq(
            |r| -> Result<(), ReadError> { Ok(r.eat_whitespace()) },
            "(Nested (Comment))",
            expect![["()"]],
        );
    }

    #[test]
    fn literal() {
        pst_fn_eq(
            |r| r.literal(),
            "\"string\"",
            expect![[r#"
Chars(
    "string",
)"#]],
        )
    }
}

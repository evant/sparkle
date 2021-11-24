use std::fs::read_to_string;
use std::io::Write;
use std::ops::RangeBounds;
use std::thread::sleep;

use annotate_snippets::display_list::FormatOptions;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};
use cranelift::codegen::isa::lookup;
use logos::{Lexer, Source};
use logos::{Logos, Span};
use nom::bytes::complete::take_while;
use nom::Slice;
use rowan::{GreenNode, GreenNodeBuilder, Language, SyntaxKind};

use crate::lexer::{Bit, Comment, Extras, Signature};
use crate::pst2::{Sparkle, SparkleNode};
use crate::read_error::ReadError;

pub struct Reader<'source> {
    origin: String,
    nom: Nomer<'source, Bit>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ReadError<'source>>,
}

pub struct Nomer<'source, Token: Logos<'source>> {
    inner: Lexer<'source, Token>,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<Token>>,
}

impl<'source, Token> Nomer<'source, Token>
where
    Token: Logos<'source, Source = str, Extras = Extras> + Copy,
{
    fn new(input: &'source str) -> Self {
        Self {
            inner: Token::lexer(input),
            peeked: None,
        }
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

    fn morph<Token2>(self) -> Nomer<'source, Token2>
    where
        Token2: Logos<'source, Source = str, Extras = Extras>,
    {
        Nomer {
            inner: self.inner.morph(),
            peeked: None,
        }
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
            origin,
            nom: Nomer::new(input),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn source(&self) -> &'source str {
        return self.nom.source();
    }

    pub fn read(mut self) -> (SparkleNode, Vec<ReadError<'source>>) {
        self.builder.start(Bit::Letter);
        self = self.read_letter();
        self.builder.finish_node();
        (SparkleNode::new_root(self.builder.finish()), self.errors)
    }

    fn read_letter(mut self) -> Self {
        self = self.read_heading();

        //TODO: read paragraphs

        self.read_signature()
    }

    fn read_heading(mut self) -> Self {
        self = self.eat_whitespace();
        self.expect(Bit::DearPrincessCelestia);
        self = self.eat_whitespace();
        self = self.read_identifier();
        self.expect(Bit::Punctuation);
        self.eat_whitespace()
    }

    fn read_signature(mut self) -> Self {
        self.expect(Bit::YourFaithfulStudent);
        let mut nom: Nomer<Signature> = self.nom.morph();
        loop {
            match nom.peek() {
                Some(Signature::Author) => {
                    self.builder.push(Bit::Author, nom.slice());
                    nom.next();
                }
                Some(Signature::Punctuation) => {
                    self.builder.push(Bit::Punctuation, nom.slice());
                    break;
                }
                _ => {
                    self.builder.push(Bit::Error, "");
                    break;
                }
            }
        }
        self.nom = nom.morph();
        self.eat_whitespace()
    }

    fn read_paragraph(mut self) -> Self {
        self.builder.start(Bit::Paragraph);
        if let Some(Bit::Today) = self.nom.peek() {
            self.bump();
            self = self.eat_whitespace();
        }
        self = self.pagragraph_opening();
        self = self.eat_whitespace();
        //TODO: statements
        self = self.pagragraph_closing();
        self.builder.finish_node();
        self
    }

    fn pagragraph_opening(mut self) -> Self {
        self.expect(Bit::ILearned);
        self = self.eat_whitespace();
        self = self.read_identifier();
        self.expect(Bit::Punctuation);
        self
    }

    fn pagragraph_closing(mut self) -> Self {
        self.expect(Bit::ThatsAllAbout);
        self = self.eat_whitespace();
        self = self.read_identifier();
        self.expect(Bit::Punctuation);
        self
    }

    fn read_identifier(mut self) -> Self {
        self.builder.start(Bit::Identifier);
        loop {
            match self.nom.peek() {
                Some(Bit::Word) => {
                    self.bump();
                }
                Some(Bit::Whitespace) => {
                    self.bump();
                }
                x => {
                    break;
                }
            }
        }
        self.builder.finish_node();
        self
    }

    fn expect(&mut self, expected: Bit) {
        let actual = self.nom.peek();
        if let Some(bit) = actual {
            if expected == bit {
                self.bump();
                return;
            }
            self.bump_with(Bit::Error);
        } else {
            self.builder.push(Bit::Error, "");
        }
        self.errors.push(ReadError::unexpected_bit(
            self.origin.clone(),
            &self.nom,
            expected,
            actual,
        ));
    }

    fn eat_whitespace(mut self) -> Self {
        loop {
            match self.nom.peek() {
                Some(Bit::Whitespace) | Some(Bit::LineComment) => {
                    self.bump();
                }
                Some(Bit::StartComment) => {
                    let mut nom: Nomer<Comment> = self.nom.morph();
                    let mut paren_count = 1;
                    loop {
                        match nom.peek() {
                            Some(Comment::Start) => paren_count += 1,
                            Some(Comment::End) => paren_count -= 1,
                            _ => self.builder.push(Bit::Comment, ""),
                        }
                        if paren_count <= 1 {
                            break;
                        }
                    }
                    self.nom = nom.morph();
                }
                _ => {
                    break;
                }
            }
        }
        self
    }

    fn bump(&mut self) {
        let (bit, text) = self.nom.next().unwrap();
        self.builder.push(bit, text);
    }

    fn bump_with(&mut self, bit: Bit) {
        let (_, text) = self.nom.next().unwrap();
        self.builder.push(bit, text);
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
    use std::io::Write;

    use annotate_snippets::display_list::DisplayList;
    use expect_test::{expect, Expect};

    use crate::lexer::Bit;
    use crate::pst2::SparkleNode;
    use crate::reader2::{GreenNodeBuilderExt, Reader};

    fn pst_eq(input: &str, expected: Expect) {
        let (pst, _) = Reader::new("".to_string(), input).read();
        let tree = format!("{:#?}", pst);

        expected.assert_eq(&tree);
    }

    fn pst_fn_eq<F>(f: F, input: &str, expected: Expect)
    where
        F: FnOnce(Reader) -> Reader,
    {
        let mut reader = Reader::new("".to_string(), input);
        reader.builder.start(Bit::Letter);
        let mut reader = f(reader);
        reader.builder.finish_node();
        let pst = SparkleNode::new_root(reader.builder.finish());
        let tree = format!("{:#?}", pst);

        expected.assert_eq(&tree);
    }

    fn pst_fn_errors<F>(f: F, input: &str, expected: Expect)
    where
        F: FnOnce(Reader) -> Reader,
    {
        let mut reader = Reader::new("".to_string(), input);
        reader.builder.start(Bit::Letter);
        let mut reader = f(reader);
        reader.builder.finish_node();
        let errors = reader.errors;
        let mut out = Vec::new();
        for error in errors {
            writeln!(&mut out, "{}", error).unwrap();
        }
        expected.assert_eq(unsafe { String::from_utf8_unchecked(out) }.as_ref());
    }

    #[test]
    fn identifier() {
        pst_fn_eq(
            |r| r.read_identifier(),
            "An example letter",
            expect![[r#"
Letter@0..17
  Identifier@0..17
    Word@0..2 "An"
    Whitespace@2..3 " "
    Word@3..10 "example"
    Whitespace@10..11 " "
    Word@11..17 "letter"
"#]],
        );
        pst_fn_eq(
            |r| r.read_identifier(),
            "An example letter.",
            expect![[r#"
Letter@0..17
  Identifier@0..17
    Word@0..2 "An"
    Whitespace@2..3 " "
    Word@3..10 "example"
    Whitespace@10..11 " "
    Word@11..17 "letter"
"#]],
        );
    }

    #[test]
    fn heading() {
        pst_fn_eq(
            |r| r.read_heading(),
            "Dear Princess Celestia: An example letter.",
            expect![[r#"
Letter@0..42
  DearPrincessCelestia@0..23 "Dear Princess Celestia:"
  Whitespace@23..24 " "
  Identifier@24..41
    Word@24..26 "An"
    Whitespace@26..27 " "
    Word@27..34 "example"
    Whitespace@34..35 " "
    Word@35..41 "letter"
  Punctuation@41..42 "."
"#]],
        );
        pst_fn_errors(
            |r| r.read_heading(),
            "Dear Princess Luna:",
            expect![[r#"
error: Oh my Celestia! I was expecting 'Dear Princess Celestia:' but I read 'Dear Princess ' instead!
--> :0:1
 |
0 | Dear Princess Luna:
 | ^^^^^^^^^^^^^^ Who is this?
 |
"#]],
        );
    }

    #[test]
    fn signature() {
        pst_fn_eq(
            |r| r.read_signature(),
            "Your faithful student: Twilight Sparkle.",
            expect![[r#"
Letter@0..40
  YourFaithfulStudent@0..22 "Your faithful student:"
  Author@22..39 " Twilight Sparkle"
  Punctuation@39..40 "."
"#]],
        );
    }

    #[test]
    fn paragraph_opening() {
        pst_fn_eq(
            |r| r.pagragraph_opening(),
            "I learned how to fly.",
            expect![[r#"
Letter@0..21
  ILearned@0..9 "I learned"
  Whitespace@9..10 " "
  Identifier@10..20
    Word@10..13 "how"
    Whitespace@13..14 " "
    Word@14..16 "to"
    Whitespace@16..17 " "
    Word@17..20 "fly"
  Punctuation@20..21 "."
"#]],
        );
    }

    #[test]
    fn paragraph_closing() {
        pst_fn_eq(
            |r| r.pagragraph_closing(),
            "That's all about how to fly.",
            expect![[r#"
Letter@0..28
  ThatsAllAbout@0..16 "That's all about"
  Whitespace@16..17 " "
  Identifier@17..27
    Word@17..20 "how"
    Whitespace@20..21 " "
    Word@21..23 "to"
    Whitespace@23..24 " "
    Word@24..27 "fly"
  Punctuation@27..28 "."
"#]],
        );
    }

    #[test]
    fn empty_paragraph() {
        pst_fn_eq(
            |r| r.read_paragraph(),
            "Today I learned how to fly.
That's all about how to fly.",
            expect![[r#"
Letter@0..56
  Paragraph@0..56
    Today@0..5 "Today"
    Whitespace@5..6 " "
    ILearned@6..15 "I learned"
    Whitespace@15..16 " "
    Identifier@16..26
      Word@16..19 "how"
      Whitespace@19..20 " "
      Word@20..22 "to"
      Whitespace@22..23 " "
      Word@23..26 "fly"
    Punctuation@26..27 "."
    Whitespace@27..28 "\n"
    ThatsAllAbout@28..44 "That's all about"
    Whitespace@44..45 " "
    Identifier@45..55
      Word@45..48 "how"
      Whitespace@48..49 " "
      Word@49..51 "to"
      Whitespace@51..52 " "
      Word@52..55 "fly"
    Punctuation@55..56 "."
"#]],
        );
    }

    #[test]
    fn empty_letter() {
        pst_eq(
            r#"
Dear Princess Celestia: I have nothing to say.
Your Faithful Student: Twilight Sparkle."#,
            expect![[r#"
Letter@0..88
  Whitespace@0..1 "\n"
  DearPrincessCelestia@1..24 "Dear Princess Celestia:"
  Whitespace@24..25 " "
  Identifier@25..46 "I have nothing to say"
  Punctuation@46..47 "."
  Whitespace@47..48 "\n"
  YourFaithfulStudent@48..70 "Your Faithful Student:"
  Author@70..87 " Twilight Sparkle"
  Punctuation@87..88 "."
"#]],
        );
    }
}

use std::ops::BitOr;

use enumset::{enum_set, EnumSet};
use logos::{Lexer, Source};
use logos::{Logos, Span};

use crate::lexer::{Bit, SparkleToken};
use crate::nomer::{Nomer, Result};
use crate::pst::{
    Arg, BinOperator, Call, Declaration, Expr, Literal, Paragraph, ParagraphDeclaration, Report,
    Statement,
};
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

    pub fn read(mut self) -> Result<'source, Report<'source>> {
        let report = self.read_letter()?;
        Ok(report)
    }

    fn read_letter(&mut self) -> Result<'source, Report<'source>> {
        let subject = self.read_heading()?;
        self.eat_whitespace();

        let mut declarations = vec![];
        while let Ok(paragraph) = self.read_paragraph() {
            declarations.push(Declaration::Paragraph(paragraph));
        }

        let author = self.read_signature()?;

        Ok(Report {
            name: subject,
            writer: author,
            declarations,
        })
    }

    fn read_heading(&mut self) -> Result<'source, &'source str> {
        self.eat_whitespace();
        self.nom
            .expect_with_label(Bit::DearPrincessCelestia, "Who is this?".to_string())?;
        self.eat_whitespace();
        let subject = self.read_identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(subject)
    }

    fn read_signature(&mut self) -> Result<'source, &'source str> {
        self.eat_whitespace();
        self.nom.expect(Bit::YourFaithfulStudent)?;
        self.eat_whitespace();
        self.nom.expect(Bit::Punctuation)?;
        self.eat_whitespace();
        let author = self.read_identifier_until(Bit::Punctuation)?;

        Ok(author)
    }

    fn read_paragraph(&mut self) -> Result<'source, Paragraph<'source>> {
        let mane = if let Some(Bit::Today) = self.nom.peek() {
            self.nom.next();
            self.eat_whitespace();
            true
        } else {
            false
        };
        let decl = self.paragraph_opening()?;
        self.eat_whitespace();
        let mut statements = vec![];
        while let Ok(statement) = self.statement() {
            statements.push(statement);
            self.eat_whitespace();
        }
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
            statements,
            decl,
        })
    }

    fn paragraph_opening(&mut self) -> Result<'source, ParagraphDeclaration<'source>> {
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

    fn paragraph_arg(&mut self) -> Result<'source, Arg<'source>> {
        self.nom.expect(enum_set!(Bit::A | Bit::The))?;
        self.eat_whitespace();
        let _type = self.declare_type()?;
        self.eat_whitespace();
        let name = self.read_identifier_until(enum_set!(Bit::Punctuation | Bit::And))?;

        Ok(Arg(_type, name))
    }

    fn declare_type(&mut self) -> Result<'source, Type> {
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

    fn paragraph_closing(&mut self) -> Result<'source, &'source str> {
        self.nom.expect(Bit::ThatsAllAbout)?;
        self.eat_whitespace();
        let topic = self.read_identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(topic)
    }

    fn statement(&mut self) -> Result<'source, Statement<'source>> {
        let statement = match self.nom.peek() {
            Some(Bit::ISaid) => {
                self.nom.next();
                self.eat_whitespace();
                let expr = self.expr()?;

                Statement::Print(expr)
            }
            other => {
                return Err(self.unexpected_token(other, enum_set!(Bit::ISaid)));
            }
        };
        self.eat_whitespace();
        self.nom.expect(Bit::Punctuation)?;

        Ok(statement)
    }

    fn expr_term(&mut self) -> Result<'source, Expr<'source>> {
        if let Ok(expr) = self.prefix_term() {
            return Ok(expr);
        }
        if let Ok(expr) = self.infix_term() {
            return Ok(expr);
        }
        if let Ok(expr) = self.value_expr() {
            return Ok(expr);
        }
        let actual = self.nom.peek();
        Err(self.unexpected_token(
            actual,
            enum_set!(
                Bit::Add
                    | Bit::Subtract
                    | Bit::Multiply
                    | Bit::Divide
                    | Bit::The
                    | Bit::Add
                    | Bit::Plus
                    | Bit::Minus
                    | Bit::Times
                    | Bit::Over
                    | Bit::Or
                    | Bit::Is
                    | Bit::Not
                    | Bit::CharsLit
                    | Bit::NumberLit
                    | Bit::TrueLit
                    | Bit::FalseLit
            ),
        ))
    }

    fn expr(&mut self) -> Result<'source, Expr<'source>> {
        fn is_chars_literal(expr: &Expr) -> bool {
            match expr {
                Expr::Lit(Literal::Chars(_)) => true,
                _ => false,
            }
        }
        let mut expr = self.expr_term()?;
        let mut acc = vec![];
        loop {
            self.eat_whitespace();
            // Alternate between expressions and char literals
            let next_expr = if is_chars_literal(&expr) {
                self.expr_term()
            } else {
                self.chars().map(|chars| Expr::Lit(chars))
            };
            acc.push(expr);
            match next_expr {
                Ok(next_expr) => {
                    expr = next_expr;
                    continue;
                }
                Err(_) => {
                    // If we only have one expression return that, otherwise we need to concatenate them.
                    return Ok(if acc.len() == 1 {
                        acc.remove(0)
                    } else {
                        Expr::Concat(acc)
                    });
                }
            }
        }
    }

    fn call(&mut self) -> Result<'source, Call<'source>> {
        let ident = self.read_identifier_until(enum_set!(
            Bit::Punctuation | Bit::Using | Bit::And | Bit::Is | Bit::Isnt | Bit::CharsLit
        ))?;
        let args = if let Some(Bit::Using) = self.nom.peek() {
            self.nom.next();
            self.eat_whitespace();
            let mut args = vec![self.expr_term()?];
            self.eat_whitespace();
            while let Some(Bit::And) = self.nom.peek() {
                self.nom.next();
                self.eat_whitespace();
                args.push(self.expr_term()?);
            }
            args
        } else {
            vec![]
        };

        Ok(Call(ident, args))
    }

    fn literal(&mut self) -> Result<'source, Literal<'source>> {
        if let Ok(chars) = self.chars() {
            return Ok(chars);
        }
        return match self.nom.peek() {
            Some(Bit::NumberLit) => {
                let (_, text) = self.nom.next().unwrap();
                Ok(Literal::Number(text.parse().unwrap()))
            }
            Some(Bit::TrueLit) => {
                self.nom.next();
                Ok(Literal::Boolean(true))
            }
            Some(Bit::FalseLit) => {
                self.nom.next();
                Ok(Literal::Boolean(false))
            }
            other => Err(self.unexpected_token(
                other,
                enum_set!(Bit::CharsLit | Bit::NumberLit | Bit::TrueLit | Bit::FalseLit),
            )),
        };
    }

    fn chars(&mut self) -> Result<'source, Literal<'source>> {
        return match self.nom.peek() {
            Some(Bit::CharsLit) => {
                let (_, text) = self.nom.next().unwrap();
                Ok(Literal::Chars(text.slice(1..text.len() - 1).unwrap()))
            }
            other => Err(self.unexpected_token(other, enum_set!(Bit::CharsLit))),
        };
    }

    fn infix_op(&mut self) -> Result<'source, BinOperator> {
        fn infix_cmp_op<'source>(nom: &mut Nomer<'source, Bit>) -> BinOperator {
            match nom.peek() {
                Some(Bit::LessThan) => {
                    nom.next();
                    BinOperator::LessThan
                }
                Some(Bit::MoreThan) => {
                    nom.next();
                    BinOperator::GreaterThan
                }
                _ => BinOperator::Equal,
            }
        }

        fn invert_infix_cmp_op<'source>(nom: &mut Nomer<'source, Bit>) -> BinOperator {
            match infix_cmp_op(nom) {
                BinOperator::Equal => BinOperator::NotEqual,
                BinOperator::LessThan => BinOperator::GreaterThanOrEqual,
                BinOperator::GreaterThan => BinOperator::LessThanOrEqual,
                _ => unreachable!(),
            }
        }

        return match self.nom.peek() {
            Some(Bit::Add | Bit::Plus) => {
                self.nom.next();
                Ok(BinOperator::AddOrAnd)
            }
            Some(Bit::Minus) => {
                self.nom.next();
                Ok(BinOperator::Sub)
            }
            Some(Bit::Times) => {
                self.nom.next();
                Ok(BinOperator::Mul)
            }
            Some(Bit::Over) => {
                self.nom.next();
                Ok(BinOperator::Div)
            }
            Some(Bit::Or) => {
                self.nom.next();
                Ok(BinOperator::Or)
            }
            Some(Bit::Is) => {
                self.nom.next();
                self.eat_whitespace();
                if let Some(Bit::Not) = self.nom.peek() {
                    self.nom.next();
                    self.eat_whitespace();
                    Ok(invert_infix_cmp_op(&mut self.nom))
                } else {
                    Ok(infix_cmp_op(&mut self.nom))
                }
            }
            Some(Bit::Isnt) => {
                self.nom.next();
                self.eat_whitespace();
                Ok(invert_infix_cmp_op(&mut self.nom))
            }
            other => Err(self.unexpected_token(
                other,
                enum_set!(
                    Bit::Add
                        | Bit::Plus
                        | Bit::Minus
                        | Bit::Times
                        | Bit::Over
                        | Bit::Or
                        | Bit::Is
                        | Bit::Isnt
                ),
            )),
        };
    }

    fn value_expr(&mut self) -> Result<'source, Expr<'source>> {
        if let Ok(expr) = self.prefix_not() {
            return Ok(expr);
        }
        if let Ok(lit) = self.literal() {
            return Ok(Expr::Lit(lit));
        }
        if let Ok(call) = self.call() {
            return Ok(Expr::Call(call));
        }
        let actual = self.nom.peek();
        Err(self.unexpected_token(
            actual,
            enum_set!(Bit::Not | Bit::CharsLit | Bit::NumberLit | Bit::TrueLit | Bit::FalseLit),
        ))
    }

    fn prefix_not(&mut self) -> Result<'source, Expr<'source>> {
        self.nom.expect(Bit::Not)?;
        self.eat_whitespace();
        let expr = self.value_expr()?;

        Ok(Expr::Not(Box::new(expr)))
    }

    fn infix_term(&mut self) -> Result<'source, Expr<'source>> {
        let mut expr = self.value_expr()?;
        self.eat_whitespace();
        while let Ok(op) = self.infix_op() {
            self.eat_whitespace();
            let right = self.value_expr()?;
            self.eat_whitespace();
            expr = Expr::BinOp(op, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn prefix_term(&mut self) -> Result<'source, Expr<'source>> {
        match self.nom.peek() {
            Some(Bit::Add) => {
                self.nom.next();
                self.eat_whitespace();
                let (left, right) =
                    self.read_infix(|s| s.value_expr(), Bit::And, |s| s.value_expr())?;

                Ok(Expr::BinOp(
                    BinOperator::AddOrAnd,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            Some(Bit::Subtract) => {
                self.nom.next();
                self.eat_whitespace();
                let (left, right) =
                    self.read_infix(|s| s.value_expr(), Bit::From, |s| s.value_expr())?;

                Ok(Expr::BinOp(
                    BinOperator::Sub,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            Some(Bit::Multiply) => {
                self.nom.next();
                self.eat_whitespace();
                let (left, right) = self.read_infix(
                    |s| s.value_expr(),
                    enum_set!(Bit::By | Bit::And),
                    |s| s.value_expr(),
                )?;

                Ok(Expr::BinOp(
                    BinOperator::Mul,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            Some(Bit::Divide) => {
                self.nom.next();
                self.eat_whitespace();
                let (left, right) = self.read_infix(
                    |s| s.value_expr(),
                    enum_set!(Bit::By | Bit::And),
                    |s| s.value_expr(),
                )?;

                Ok(Expr::BinOp(
                    BinOperator::Div,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            Some(Bit::The) => {
                self.nom.next();
                self.eat_whitespace();
                let (bit, _) = self.nom.expect(Bit::DifferenceBetween | Bit::ProductOf)?;
                self.eat_whitespace();
                let (left, right) =
                    self.read_infix(|s| s.value_expr(), Bit::And, |s| s.value_expr())?;
                let op = match bit {
                    Bit::DifferenceBetween => BinOperator::Sub,
                    Bit::ProductOf => BinOperator::Mul,
                    _ => unreachable!(),
                };

                Ok(Expr::BinOp(op, Box::new(left), Box::new(right)))
            }
            Some(Bit::Either) => {
                self.nom.next();
                self.eat_whitespace();
                let (left, right) =
                    self.read_infix(|s| s.value_expr(), Bit::Or, |s| s.value_expr())?;

                Ok(Expr::BinOp(
                    BinOperator::EitherOr,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            other => Err(self.unexpected_token(
                other,
                enum_set!(Bit::Add | Bit::Subtract | Bit::Multiply | Bit::Divide | Bit::The),
            )),
        }
    }

    fn read_infix<F1, F2, R1, R2>(
        &mut self,
        read1: F1,
        infix: impl Into<EnumSet<Bit>>,
        read2: F2,
    ) -> Result<'source, (R1, R2)>
    where
        F1: FnOnce(&mut Self) -> Result<'source, R1>,
        F2: FnOnce(&mut Self) -> Result<'source, R2>,
    {
        let left = read1(self)?;
        self.eat_whitespace();
        self.nom.expect(infix)?;
        self.eat_whitespace();
        let right = read2(self)?;

        return Ok((left, right));
    }

    fn read_identifier(&mut self) -> Result<'source, &'source str> {
        self.read_identifier_until(EnumSet::empty())
    }

    fn read_identifier_until(
        &mut self,
        until: impl Into<EnumSet<Bit>>,
    ) -> Result<'source, &'source str> {
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
                    if let Some(other) = other.clone() {
                        if !until.is_empty() && !until.contains(other) {
                            self.nom.next();
                            end = self.nom.span().end;
                            continue;
                        }
                    }
                    if start == end {
                        return Err(self.unexpected_token(other, enum_set!(Bit::Word)));
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

    fn unexpected_token(&self, actual: Option<Bit>, expected: EnumSet<Bit>) -> ReadError<'source> {
        return ReadError::unexpected_token(
            &self.nom,
            expected,
            actual,
            "What is this?".to_string(),
        );
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
                [1;38;5;9merror[0m: [1mOh my Celestia! I was expecting 'Dear Princess Celestia:' but I read 'Dear Princess ' instead![0m
                [1;38;5;12m-->[0m test.rs:0:1
                [1;38;5;12m |[0m
                [1;38;5;12m0 |[0m Dear Princess Luna:
                [1;38;5;12m |[0m[1;38;5;9m ^^^^^^^^^^^^^^[0m [1;38;5;9mWho is this?[0m
                [1;38;5;12m |[0m"#]],
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
    fn letter() {
        pst_eq(
            r#"Dear Princess Celestia: An example letter.

        Today I learned how to fly:
            I said "Fly!"!
        That's all about how to fly!

        Did you know that I had 100 (apples)?
    Your faithful student: Twilight Sparkle.

    P.S. This is ignored"#,
            expect![[]],
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
    fn call() {
        pst_fn_eq(
            |r| r.call(),
            "how to fly",
            expect![[r#"
                Call(
                    "how to fly",
                    [],
                )"#]],
        );
        pst_fn_eq(
            |r| r.call(),
            "how to fly using \"Twilight Sparkle\"",
            expect![[r#"
                Call(
                    "how to fly",
                    [
                        Lit(
                            Chars(
                                "Twilight Sparkle",
                            ),
                        ),
                    ],
                )"#]],
        );
        pst_fn_eq(
            |r| r.call(),
            "how to fly using Rainbow Dash and Fluttershy",
            expect![[r#"
                Call(
                    "how to fly",
                    [
                        Call(
                            Call(
                                "Rainbow Dash",
                                [],
                            ),
                        ),
                        Call(
                            Call(
                                "Fluttershy",
                                [],
                            ),
                        ),
                    ],
                )"#]],
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
        );
        pst_fn_eq(
            |r| r.literal(),
            "12",
            expect![[r#"
Number(
    12.0,
)"#]],
        );
        pst_fn_eq(
            |r| r.literal(),
            "-1.6",
            expect![[r#"
Number(
    -1.6,
)"#]],
        );
        pst_fn_eq(
            |r| r.literal(),
            "yes",
            expect![[r#"
Boolean(
    true,
)"#]],
        );
        pst_fn_eq(
            |r| r.literal(),
            "no",
            expect![[r#"
Boolean(
    false,
)"#]],
        );
    }

    #[test]
    fn infix_op() {
        pst_fn_eq(|r| r.infix_op(), "added to", expect![[r#"AddOrAnd"#]]);
        pst_fn_eq(|r| r.infix_op(), "minus", expect![[r#"Sub"#]]);
        pst_fn_eq(|r| r.infix_op(), "multiplied with", expect![[r#"Mul"#]]);
        pst_fn_eq(|r| r.infix_op(), "divided by", expect![[r#"Div"#]]);
        pst_fn_eq(|r| r.infix_op(), "or", expect![[r#"Or"#]]);
    }

    #[test]
    fn infix_term() {
        pst_fn_eq(
            |r| r.infix_term(),
            "1",
            expect![[r#"
                Lit(
                    Number(
                        1.0,
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "1 added to 2",
            expect![[r#"
                BinOp(
                    AddOrAnd,
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "2 plus 1 times 3",
            expect![[r#"
                BinOp(
                    Mul,
                    BinOp(
                        AddOrAnd,
                        Lit(
                            Number(
                                2.0,
                            ),
                        ),
                        Lit(
                            Number(
                                1.0,
                            ),
                        ),
                    ),
                    Lit(
                        Number(
                            3.0,
                        ),
                    ),
                )"#]],
        );
    }

    #[test]
    fn prefix_term() {
        pst_fn_eq(
            |r| r.prefix_term(),
            "add 1 and 2",
            expect![[r#"
                BinOp(
                    AddOrAnd,
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.prefix_term(),
            "the difference between 2 and 1",
            expect![[r#"
                BinOp(
                    Sub,
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.prefix_term(),
            "the product of 2 and 1",
            expect![[r#"
                BinOp(
                    Mul,
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.prefix_term(),
            "divide 2 by 1",
            expect![[r#"
                BinOp(
                    Div,
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.prefix_term(),
            "either true or false",
            expect![[r#"
                BinOp(
                    EitherOr,
                    Lit(
                        Boolean(
                            true,
                        ),
                    ),
                    Lit(
                        Boolean(
                            false,
                        ),
                    ),
                )"#]],
        );
    }

    #[test]
    fn not() {
        pst_fn_eq(
            |r| r.prefix_not(),
            "not true",
            expect![[r#"
Not(
    Lit(
        Boolean(
            true,
        ),
    ),
)"#]],
        );
        pst_fn_eq(
            |r| r.prefix_not(),
            "not not true",
            expect![[r#"
            Not(
                Not(
                    Lit(
                        Boolean(
                            true,
                        ),
                    ),
                ),
            )"#]],
        );
    }

    #[test]
    fn comparison() {
        pst_fn_eq(
            |r| r.infix_term(),
            "Rainbow Dash is cool",
            expect![[r#"
                BinOp(
                    Equal,
                    Call(
                        Call(
                            "Rainbow Dash",
                            [],
                        ),
                    ),
                    Call(
                        Call(
                            "cool",
                            [],
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "Fluttershy isn't loud",
            expect![[r#"
                BinOp(
                    NotEqual,
                    Call(
                        Call(
                            "Fluttershy",
                            [],
                        ),
                    ),
                    Call(
                        Call(
                            "loud",
                            [],
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "the number of cupcakes is less than 10",
            expect![[r#"
                BinOp(
                    LessThan,
                    Call(
                        Call(
                            "the number of cupcakes",
                            [],
                        ),
                    ),
                    Lit(
                        Number(
                            10.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "the number of pies is not less than 10",
            expect![[r#"
                BinOp(
                    GreaterThanOrEqual,
                    Call(
                        Call(
                            "the number of pies",
                            [],
                        ),
                    ),
                    Lit(
                        Number(
                            10.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "the number of cakes is more than 10",
            expect![[r#"
                BinOp(
                    GreaterThan,
                    Call(
                        Call(
                            "the number of cakes",
                            [],
                        ),
                    ),
                    Lit(
                        Number(
                            10.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "the number of cute animals isn't greater than 100",
            expect![[r#"
                BinOp(
                    LessThanOrEqual,
                    Call(
                        Call(
                            "the number of cute animals",
                            [],
                        ),
                    ),
                    Lit(
                        Number(
                            100.0,
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.infix_term(),
            "Applejack has more than 50",
            expect![[r#"
                BinOp(
                    GreaterThan,
                    Call(
                        Call(
                            "Applejack",
                            [],
                        ),
                    ),
                    Lit(
                        Number(
                            50.0,
                        ),
                    ),
                )"#]],
        );
    }

    #[test]
    fn concat() {
        pst_fn_eq(
            |r| r.expr(),
            "Applejack\" jugs of cider on the wall\"",
            expect![[r#"
                Concat(
                    [
                        Call(
                            Call(
                                "Applejack",
                                [],
                            ),
                        ),
                        Lit(
                            Chars(
                                " jugs of cider on the wall",
                            ),
                        ),
                    ],
                )"#]],
        );
        pst_fn_eq(
            |r| r.expr(),
            "\"It needs to be about \" 20 \"% cooler\"",
            expect![[r#"
                Concat(
                    [
                        Lit(
                            Chars(
                                "It needs to be about ",
                            ),
                        ),
                        Lit(
                            Number(
                                20.0,
                            ),
                        ),
                        Lit(
                            Chars(
                                "% cooler",
                            ),
                        ),
                    ],
                )"#]],
        );
        pst_fn_eq(
            |r| r.expr(),
            "\"but \" my favorite numbers using 3 \" is pretty tasty!\"",
            expect![[r#"
                Concat(
                    [
                        Lit(
                            Chars(
                                "but ",
                            ),
                        ),
                        Call(
                            Call(
                                "my favorite numbers",
                                [
                                    Lit(
                                        Number(
                                            3.0,
                                        ),
                                    ),
                                ],
                            ),
                        ),
                        Lit(
                            Chars(
                                " is pretty tasty!",
                            ),
                        ),
                    ],
                )"#]],
        );
    }

    #[test]
    fn print_statement() {
        pst_fn_eq(
            |r| r.statement(),
            "I wrote 1 added to 2.",
            expect![[r#"
            Print(
                BinOp(
                    AddOrAnd,
                    Lit(
                        Number(
                            1.0,
                        ),
                    ),
                    Lit(
                        Number(
                            2.0,
                        ),
                    ),
                ),
            )"#]],
        );
        pst_fn_eq(
            |r| r.statement(),
            "I sang the elements of harmony count.",
            expect![[r#"
                Print(
                    Call(
                        Call(
                            "elements of harmony count",
                            [],
                        ),
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.statement(),
            r#"I said "It needs to be about " 20 "% cooler"."#,
            expect![[r#"
                Print(
                    Concat(
                        [
                            Lit(
                                Chars(
                                    "It needs to be about ",
                                ),
                            ),
                            Lit(
                                Number(
                                    20.0,
                                ),
                            ),
                            Lit(
                                Chars(
                                    "% cooler",
                                ),
                            ),
                        ],
                    ),
                )"#]],
        );
        pst_fn_eq(
            |r| r.statement(),
            r#"I said Tantabus""!"#,
            expect![[r#"
            Print(
                Concat(
                    [
                        Call(
                            Call(
                                "Tantabus",
                                [],
                            ),
                        ),
                        Lit(
                            Chars(
                                "",
                            ),
                        ),
                    ],
                ),
            )"#]],
        );
    }
}

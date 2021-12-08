use std::io::SeekFrom::Start;

use cranelift::codegen::ir::HeapStyle::Static;
use enumset::{enum_set, EnumSet};
use logos::{Lexer, Source};
use logos::{Logos, Span};

use crate::lexer::{Bit, Extras, SparkleToken};
use crate::nomer::{Nomer, Result};
use crate::pst::{
    Arg, BinOperator, Call, Declaration, DeclareVar, Expr, LValue, Literal, Paragraph,
    ParagraphDeclaration, Report, Statement, Variable,
};
use crate::read_error::ReadError;
use crate::types::{ArrayType, Type};

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
        let report = self.letter()?;
        Ok(report)
    }

    fn letter(&mut self) -> Result<'source, Report<'source>> {
        self.whitespace0();
        let subject = self.heading()?;
        self.whitespace0();

        let mut declarations = vec![];
        loop {
            if let Ok(var) = self.declare_var() {
                declarations.push(Declaration::Var(var));
                self.whitespace0();
                self.nom.expect(Bit::Punctuation)?;
                continue;
            }
            if let Ok(paragraph) = self.paragraph() {
                declarations.push(Declaration::Paragraph(paragraph));
                self.whitespace0();
                continue;
            }
            break;
        }

        self.whitespace0();
        let author = self.signature()?;

        Ok(Report {
            name: subject,
            writer: author,
            declarations,
        })
    }

    fn heading(&mut self) -> Result<'source, &'source str> {
        self.nom.expect(Bit::Dear)?;
        self.whitespace1()?;
        self.nom.expect(Bit::Princess)?;
        self.whitespace1()?;
        self.nom
            .expect_with_label(Bit::Celestia, "Who is this?".to_string())?;
        self.whitespace0();
        let subject = self.identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(subject)
    }

    fn signature(&mut self) -> Result<'source, &'source str> {
        self.nom.expect(Bit::Your)?;
        self.whitespace1()?;
        self.nom.expect(Bit::Faithful)?;
        self.whitespace1()?;
        self.nom.expect(Bit::Student)?;
        self.whitespace0();
        self.nom.expect(Bit::Punctuation)?;
        self.whitespace0();
        let author = self.identifier_until(Bit::Punctuation)?;

        Ok(author)
    }

    fn declare_var(&mut self) -> Result<'source, DeclareVar<'source>> {
        self.nom.expect(Bit::Did)?;
        self.whitespace1()?;
        self.nom.expect(Bit::You)?;
        self.whitespace1()?;
        self.nom.expect(Bit::Know)?;
        self.whitespace1()?;
        self.nom.expect(Bit::That)?;
        self.whitespace1()?;

        let name =
            self.identifier_until(enum_set!(Bit::Always | Bit::Is | Bit::Have | Bit::Like))?;

        let is_const = if let Some(Bit::Always) = self.nom.peek() {
            self.nom.next();
            self.whitespace1()?;
            true
        } else {
            false
        };

        self.nom
            .expect(enum_set!(Bit::Is | Bit::Have | Bit::Like))?;
        self.whitespace1()?;

        let type_ = self.declare_type().ok();
        let exprs = match type_ {
            Some(type_) => {
                if let Ok(_) = self.whitespace1() {
                    let exprs = if let Type::Array(_) = type_ {
                        if let Ok(expr) = self.expr() {
                            let mut exprs = vec![expr];
                            while let Some(Bit::And) = self.nom.peek() {
                                self.nom.next();
                                self.whitespace1();
                                exprs.push(self.expr()?);
                            }
                            Some(exprs)
                        } else {
                            None
                        }
                    } else {
                        self.expr().ok().map(|e| vec![e])
                    };
                    exprs
                } else {
                    None
                }
            }
            None => self.expr().ok().map(|e| vec![e]),
        };

        Ok(DeclareVar(Variable(name), type_, exprs, is_const))
    }

    fn paragraph(&mut self) -> Result<'source, Paragraph<'source>> {
        let mane = if let Some(Bit::Today) = self.nom.peek() {
            self.nom.next();
            self.whitespace1();
            true
        } else {
            false
        };
        let decl = self.paragraph_opening()?;
        self.whitespace0();
        let mut statements = vec![];
        while let Ok(statement) = self.statement() {
            statements.push(statement);
            self.whitespace0();
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
        self.nom.expect(Bit::I)?;
        println!("I! peek: {:?}", self.nom.peek());
        self.whitespace1()?;
        println!("-Learned!");
        self.nom.expect(Bit::Learned)?;
        self.whitespace1();
        let topic = self.identifier_until(enum_set!(Bit::Punctuation | Bit::With | Bit::Using))?;
        let return_type = if let Some(Bit::With) = self.nom.peek() {
            self.nom.next();
            self.whitespace1();
            let return_type = self.declare_type()?;
            self.whitespace0();
            Some(return_type)
        } else {
            None
        };
        let mut args = Vec::new();
        if let Some(Bit::Using) = self.nom.peek() {
            self.nom.next();
            self.whitespace1();
            loop {
                let arg = self.paragraph_arg()?;
                args.push(arg);
                self.whitespace0();
                if let Some(Bit::And) = self.nom.peek() {
                    self.nom.next();
                    self.whitespace1();
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
        let type_ = self.declare_type()?;
        self.whitespace0();
        let name = self.identifier_until(enum_set!(Bit::Punctuation | Bit::And))?;

        Ok(Arg(type_, name))
    }

    fn declare_type(&mut self) -> Result<'source, Type> {
        let (bit, _) = self.nom.expect(Bit::A | Bit::The | Bit::Many)?;
        self.whitespace1();

        let expected_types = match bit {
            Bit::A => enum_set!(Bit::Number | Bit::Chars | Bit::Boolean),
            Bit::Many => enum_set!(Bit::NumberArray | Bit::CharsArray | Bit::BooleanArray),
            Bit::The => enum_set!(
                Bit::Number
                    | Bit::NumberArray
                    | Bit::Chars
                    | Bit::CharsArray
                    | Bit::Boolean
                    | Bit::BooleanArray
            ),
            _ => unreachable!(),
        };
        let (_type, _) = self.nom.expect(expected_types)?;
        Ok(match _type {
            Bit::Number => Type::Number,
            Bit::Chars => Type::Chars,
            Bit::Boolean => Type::Boolean,
            Bit::NumberArray => Type::Array(ArrayType::Number),
            Bit::CharsArray => Type::Array(ArrayType::Chars),
            Bit::BooleanArray => Type::Array(ArrayType::Boolean),
            _ => unreachable!(),
        })
    }

    fn type_(&mut self) -> Result<'source, Type> {
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
        self.whitespace0();
        let topic = self.identifier_until(Bit::Punctuation)?;
        self.nom.expect(Bit::Punctuation)?;

        Ok(topic)
    }

    fn statement(&mut self) -> Result<'source, Statement<'source>> {
        let statement = match self.nom.peek() {
            Some(Bit::I) => {
                self.nom.next();
                self.whitespace1()?;
                match self.nom.peek() {
                    Some(Bit::Said) => {
                        self.nom.next();
                        self.whitespace1()?;
                        let expr = self.expr()?;
                        Statement::Print(expr)
                    }
                    Some(Bit::Heard) => {
                        self.nom.next();
                        self.whitespace1()?;
                        let var = self.identifier_until(enum_set!(
                            Bit::Punctuation | Bit::The | Bit::CharsLit
                        ))?;
                        let type_ = if let Some(Bit::The) = self.nom.peek() {
                            self.nom.next();
                            self.whitespace1()?;
                            self.nom.expect(Bit::Next)?;
                            self.whitespace1()?;
                            Some(self.type_()?)
                        } else {
                            None
                        };
                        let next_is_whitespace = self.whitespace1().is_ok();
                        let prompt = if type_.is_none() || next_is_whitespace {
                            if let Ok(expr) = self.expr() {
                                Some(expr)
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        Statement::Read(LValue::Variable(var), type_, prompt)
                    }
                    other => {
                        return Err(self.unexpected_token(other, enum_set!(Bit::Said)));
                    }
                }
            }
            Some(Bit::Did) => {
                let decl = self.declare_var()?;
                Statement::Declare(decl)
            }
            other => {
                return Err(self.unexpected_token(other, enum_set!(Bit::I)));
            }
        };
        self.whitespace0();
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
            self.whitespace0();
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
        let ident = self.identifier_until(enum_set!(
            Bit::Punctuation | Bit::Using | Bit::And | Bit::Is | Bit::Isnt | Bit::CharsLit
        ))?;
        let args = if let Some(Bit::Using) = self.nom.peek() {
            self.nom.next();
            self.whitespace1();
            let mut args = vec![self.expr_term()?];
            self.whitespace0();
            while let Some(Bit::And) = self.nom.peek() {
                self.nom.next();
                self.whitespace1();
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
        fn infix_cmp_op(nom: &mut Nomer<Bit>) -> BinOperator {
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

        fn invert_infix_cmp_op(nom: &mut Nomer<Bit>) -> BinOperator {
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
                self.whitespace1();
                if let Some(Bit::Not) = self.nom.peek() {
                    self.nom.next();
                    self.whitespace1();
                    Ok(invert_infix_cmp_op(&mut self.nom))
                } else {
                    Ok(infix_cmp_op(&mut self.nom))
                }
            }
            Some(Bit::Isnt) => {
                self.nom.next();
                self.whitespace1();
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
        self.whitespace1();
        let expr = self.value_expr()?;

        Ok(Expr::Not(Box::new(expr)))
    }

    fn infix_term(&mut self) -> Result<'source, Expr<'source>> {
        let mut expr = self.value_expr()?;
        self.whitespace0();
        while let Ok(op) = self.infix_op() {
            self.whitespace1();
            let right = self.value_expr()?;
            self.whitespace0();
            expr = Expr::BinOp(op, Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn prefix_term(&mut self) -> Result<'source, Expr<'source>> {
        match self.nom.peek() {
            Some(Bit::Add) => {
                self.nom.next();
                self.whitespace1();
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
                self.whitespace1();
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
                self.whitespace1();
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
                self.whitespace1();
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
                self.whitespace1();
                let (bit, _) = self.nom.expect(Bit::DifferenceBetween | Bit::ProductOf)?;
                self.whitespace1();
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
                self.whitespace1();
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
        self.whitespace0();
        self.nom.expect(infix)?;
        self.whitespace1();
        let right = read2(self)?;

        return Ok((left, right));
    }

    fn identifier(&mut self) -> Result<'source, &'source str> {
        self.identifier_until(EnumSet::empty())
    }

    fn identifier_until(
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
                Some(Bit::Whitespace | Bit::OpenParen) => {
                    self.whitespace0();
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

    fn whitespace0(&mut self) {
        loop {
            match self.nom.peek() {
                Some(Bit::Whitespace | Bit::LineComment) => {
                    self.nom.next();
                }
                Some(Bit::OpenParen) => {
                    self.remaining_multiline_comment();
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn whitespace1(&mut self) -> Result<'source, ()> {
        let (bit, _) = self.nom.expect(enum_set!(
            Bit::Whitespace | Bit::LineComment | Bit::OpenParen
        ))?;
        if bit == Bit::OpenParen {
            self.remaining_multiline_comment();
        }
        self.whitespace0();
        Ok(())
    }

    fn remaining_multiline_comment(&mut self) {
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

    use insta::assert_debug_snapshot;

    use crate::read_error::ReadError;
    use crate::reader2::Reader;

    type Result = crate::nomer::Result<'static, ()>;

    fn read(input: &str) -> Reader {
        Reader::new("test.rs".to_string(), input)
    }

    #[test]
    fn identifier() -> Result {
        let pst = read("An example letter").identifier()?;
        assert_debug_snapshot!(pst, @r###""An example letter""###);

        let pst = read("An example letter.").identifier()?;
        assert_debug_snapshot!(pst, @r###""An example letter""###);

        let pst = read("An example letter .").identifier()?;
        assert_debug_snapshot!(pst, @r###""An example letter""###);

        Ok(())
    }

    #[test]
    fn heading() -> Result {
        let pst = read("Dear Princess Celestia: An example letter.").heading()?;
        assert_debug_snapshot!(pst, @r###""An example letter""###);

        let error = read("Dear Princess Luna:")
            .heading()
            .expect_err("should error");
        assert_debug_snapshot!(error, @r###"
        ReadError {
            origin: "test.rs",
            text: "Oh my Celestia! I was expecting 'Celestia:' but I read 'Luna' instead!",
            line_num: 0,
            source: "Dear Princess Luna:",
            span: (
                "Who is this?",
                14..18,
            ),
        }
        "###);

        Ok(())
    }

    #[test]
    fn signature() -> Result {
        let pst = read("Your faithful student: Twilight Sparkle.").signature()?;
        assert_debug_snapshot!(pst, @r###""Twilight Sparkle""###);

        let pst = read("Your faithful student, Applejack's hat!").signature()?;
        assert_debug_snapshot!(pst, @r###""Applejack's hat""###);

        Ok(())
    }

    #[test]
    fn paragraph_opening() -> Result {
        let pst = read("I learned how to fly.").paragraph_opening()?;
        assert_debug_snapshot!(pst, @r###"
        ParagraphDeclaration {
            name: "how to fly",
            args: [],
            return_type: None,
        }
        "###);

        let pst = read("I learned to say hello with a number:").paragraph_opening()?;
        assert_debug_snapshot!(pst, @r###"
        ParagraphDeclaration {
            name: "to say hello",
            args: [],
            return_type: Some(
                Number,
            ),
        }
        "###);

        let pst = read("I learned to make friends with a phrase using the number of elements of harmony and the word hello:").paragraph_opening()?;
        assert_debug_snapshot!(pst, @r###"
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
        }
        "###);

        Ok(())
    }

    #[test]
    fn paragraph_arg() -> Result {
        let pst = read("the word hello").paragraph_arg()?;
        assert_debug_snapshot!(pst, @r###"
        Arg(
            Chars,
            "hello",
        )
        "###);

        Ok(())
    }

    #[test]
    fn paragraph_closing() -> Result {
        let pst = read("That's all about how to fly.").paragraph_closing()?;
        assert_debug_snapshot!(pst, @r###""how to fly""###);

        Ok(())
    }

    #[test]
    fn empty_paragraph() -> Result {
        let pst = read(
            r#"Today I learned how to fly.
               That's all about how to fly."#,
        )
        .paragraph()?;
        assert_debug_snapshot!(pst, @r###"
        Paragraph {
            decl: ParagraphDeclaration {
                name: "how to fly",
                args: [],
                return_type: None,
            },
            mane: true,
            closing_name: "how to fly",
            statements: [],
        }
        "###);

        Ok(())
    }

    #[test]
    fn empty_letter() -> Result {
        let pst = read(
            r#"Dear Princess Celestia: I have nothing to say.
                     Your faithful student: Twilight Sparkle."#,
        )
        .letter()?;
        assert_debug_snapshot!(pst, @r###"
        Report {
            name: "I have nothing to say",
            declarations: [],
            writer: "Twilight Sparkle",
        }
        "###);

        Ok(())
    }

    #[test]
    #[ignore]
    fn letter() -> Result {
        let pst = read(
            r#"Dear Princess Celestia: An example letter.

                     Today I learned how to fly:
                     I said "Fly!"!
                     That's all about how to fly!

                     Did you know that I had 100 (apples)?
                     Your faithful student: Twilight Sparkle.

                     P.S. This is ignored"#,
        )
        .letter()?;
        assert_debug_snapshot!(pst, @"");

        Ok(())
    }

    #[test]
    fn line_comment() -> Result {
        let pst = read("P.S. Comment").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        let pst = read("P.S. Comment\n").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        let pst = read("P.P.P.S. Comment\n").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        let pst = read("P.P.P.S. Comment\r\n").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        Ok(())
    }

    #[test]
    fn multiline_comment() -> Result {
        let pst = read("(Comment)").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        let pst = read("(Nested (Comment))").whitespace0();
        assert_debug_snapshot!(pst, @"()");

        Ok(())
    }

    #[test]
    fn call() -> Result {
        let pst = read("how to fly").call()?;
        assert_debug_snapshot!(pst, @r###"
        Call(
            "how to fly",
            [],
        )
        "###);

        let pst = read(r#"how to fly using "Twilight Sparkle""#).call()?;
        assert_debug_snapshot!(pst, @r###"
        Call(
            "how to fly",
            [
                Lit(
                    Chars(
                        "Twilight Sparkle",
                    ),
                ),
            ],
        )
        "###);

        let pst = read("how to fly using Rainbow Dash and Fluttershy").call()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn literal() -> Result {
        let pst = read("\"string\"").literal()?;
        assert_debug_snapshot!(pst, @r###"
        Chars(
            "string",
        )
        "###);

        let pst = read("12").literal()?;
        assert_debug_snapshot!(pst, @r###"
        Number(
            12.0,
        )
        "###);

        let pst = read("-1.6").literal()?;
        assert_debug_snapshot!(pst, @r###"
        Number(
            -1.6,
        )
        "###);

        let pst = read("yes").literal()?;
        assert_debug_snapshot!(pst, @r###"
        Boolean(
            true,
        )
        "###);

        let pst = read("no").literal()?;
        assert_debug_snapshot!(pst, @r###"
        Boolean(
            false,
        )
        "###);

        Ok(())
    }

    #[test]
    fn infix_op() -> Result {
        let pst = read("added to").infix_op()?;
        assert_debug_snapshot!(pst, @"AddOrAnd");

        let pst = read("minus").infix_op()?;
        assert_debug_snapshot!(pst, @"Sub");

        let pst = read("multiplied with").infix_op()?;
        assert_debug_snapshot!(pst, @"Mul");

        let pst = read("divided by").infix_op()?;
        assert_debug_snapshot!(pst, @"Div");

        let pst = read("or").infix_op()?;
        assert_debug_snapshot!(pst, @"Or");

        Ok(())
    }

    #[test]
    fn infix_term() -> Result {
        let pst = read("1").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
        Lit(
            Number(
                1.0,
            ),
        )
        "###);

        let pst = read("1 added to 2").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("2 plus 1 times 3").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn prefix_term() -> Result {
        let pst = read("add 1 and 2").prefix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the difference between 2 and 1").prefix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the product of 2 and 1").prefix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("divide 2 by 1").prefix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("either true or false").prefix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn not() -> Result {
        let pst = read("not true").prefix_not()?;
        assert_debug_snapshot!(pst, @r###"
        Not(
            Lit(
                Boolean(
                    true,
                ),
            ),
        )
        "###);

        let pst = read("not not true").prefix_not()?;
        assert_debug_snapshot!(pst, @r###"
        Not(
            Not(
                Lit(
                    Boolean(
                        true,
                    ),
                ),
            ),
        )
        "###);

        Ok(())
    }

    #[test]
    fn comparison() -> Result {
        let pst = read("Rainbow Dash is cool").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("Fluttershy isn't loud").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the number of cupcakes is less than 10").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the number of pies is not less than 10").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the number of cakes is more than 10").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("the number of cute animals isn't greater than 100").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("Applejack has more than 50").infix_term()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn concat() -> Result {
        let pst = read(r#"Applejack" jugs of cider on the wall""#).expr()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read(r#"It needs to be about " 20 "% cooler""#).expr()?;
        assert_debug_snapshot!(pst, @r###"
        Concat(
            [
                Call(
                    Call(
                        "It needs to be about",
                        [],
                    ),
                ),
                Lit(
                    Chars(
                        " 20 ",
                    ),
                ),
                Call(
                    Call(
                        "% cooler\"",
                        [],
                    ),
                ),
            ],
        )
        "###);

        let pst = read(r#""but" my favorite numbers using 3 " is pretty tasty!""#).expr()?;
        assert_debug_snapshot!(pst, @r###"
        Concat(
            [
                Lit(
                    Chars(
                        "but",
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn declare_var() -> Result {
        let pst =
            read("Did you know that the elements of harmony count is a number").declare_var()?;
        assert_debug_snapshot!(pst, @r###"
        DeclareVar(
            Variable(
                "the elements of harmony count",
            ),
            Some(
                Number,
            ),
            None,
            false,
        )
        "###);

        let pst =
            read(r#"Did you know that Applejack's hat has the name "Talluah""#).declare_var()?;
        assert_debug_snapshot!(pst, @r###"
        DeclareVar(
            Variable(
                "Applejack's hat",
            ),
            Some(
                Chars,
            ),
            Some(
                [
                    Lit(
                        Chars(
                            "Talluah",
                        ),
                    ),
                ],
            ),
            false,
        )
        "###);

        let pst = read("Did you know that Pinkie Pie always is right").declare_var()?;
        assert_debug_snapshot!(pst, @r###"
        DeclareVar(
            Variable(
                "Pinkie Pie",
            ),
            None,
            Some(
                [
                    Lit(
                        Boolean(
                            true,
                        ),
                    ),
                ],
            ),
            true,
        )
        "###);

        let pst = read(r#"Did you know that my cakes have the names "chocolate" and "apple cinnamon" and "fruit""#).declare_var()?;
        assert_debug_snapshot!(pst, @r###"
        DeclareVar(
            Variable(
                "my cakes",
            ),
            Some(
                Array(
                    Chars,
                ),
            ),
            Some(
                [
                    Lit(
                        Chars(
                            "chocolate",
                        ),
                    ),
                    Lit(
                        Chars(
                            "apple cinnamon",
                        ),
                    ),
                    Lit(
                        Chars(
                            "fruit",
                        ),
                    ),
                ],
            ),
            false,
        )
        "###);

        Ok(())
    }

    #[test]
    fn print_statement() -> Result {
        let pst = read("I wrote 1 added to 2.").statement()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read("I sang the elements of harmony count.").statement()?;
        assert_debug_snapshot!(pst, @r###"
        Print(
            Call(
                Call(
                    "elements of harmony count",
                    [],
                ),
            ),
        )
        "###);

        let pst = read(r#"I said "It needs to be about " 20 "% cooler"."#).statement()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        let pst = read(r#"I said Tantabus""!"#).statement()?;
        assert_debug_snapshot!(pst, @r###"
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
        )
        "###);

        Ok(())
    }

    #[test]
    fn read_statement() -> Result {
        let pst = read("I heard Applejack's speech.").statement()?;
        assert_debug_snapshot!(pst, @r###"
        Read(
            Variable(
                "Applejack's speech",
            ),
            None,
            None,
        )
        "###);

        let pst = read("I read Twilight the next number.").statement()?;
        assert_debug_snapshot!(pst, @r###"
        Read(
            Variable(
                "Twilight",
            ),
            Some(
                Number,
            ),
            None,
        )
        "###);

        let pst = read(r#"I asked Spike "How many gems are left?"."#).statement()?;
        assert_debug_snapshot!(pst, @r###"
        Read(
            Variable(
                "Spike",
            ),
            None,
            Some(
                Lit(
                    Chars(
                        "How many gems are left?",
                    ),
                ),
            ),
        )
        "###);

        let pst = read(r#"I asked Applejack the next number "How many apples do you have?"."#)
            .statement()?;
        assert_debug_snapshot!(pst, @r###"
        Read(
            Variable(
                "Applejack",
            ),
            Some(
                Number,
            ),
            Some(
                Lit(
                    Chars(
                        "How many apples do you have?",
                    ),
                ),
            ),
        )
        "###);

        Ok(())
    }
}

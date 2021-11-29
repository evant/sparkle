use std::fmt::{Debug, Display, Formatter};

use enumset::EnumSetType;
use logos::{Lexer, Logos};

pub struct Extras {
    pub line_num: usize,
}

impl Default for Extras {
    fn default() -> Self {
        Self { line_num: 0usize }
    }
}

pub trait SparkleToken<'source>:
    Logos<'source, Source = str, Extras = Extras> + EnumSetType + Copy + PartialEq + Display + Debug
{
}

impl<
        'source,
        T: Logos<'source, Source = str, Extras = Extras>
            + EnumSetType
            + Copy
            + PartialEq
            + Display
            + Debug,
    > SparkleToken<'source> for T
{
}

#[derive(Debug, Hash, PartialOrd, Ord, Logos, EnumSetType)]
#[logos(extras = Extras)]
pub enum Bit {
    #[error]
    Error,
    #[regex("[^ \t\r\n!,.:?…‽\"()]+", priority = 1)]
    Word,
    #[regex(r"[ \t\r\n]+", update_newline)]
    Whitespace,
    #[regex(r"P\.(S\.)[^\n]*\r?\n?", update_newline)]
    LineComment,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("always")]
    Always,
    #[regex("is|was|ha(s|d)")]
    Is,
    #[regex("isn't|wasn't|ha(s|d)n't")]
    Isnt,
    #[token("have")]
    Have,
    #[regex("like(s|d)?")]
    Like,
    #[token("not")]
    Not,
    #[token("and")]
    And,
    #[regex("plus|added[ \t\r\n]+to")]
    Plus,
    #[token("add")]
    Add,
    #[regex("minus|without")]
    Minus,
    #[regex("difference[ \t\r\n]+between")]
    DifferenceBetween,
    #[token("subtract")]
    Subtract,
    #[token("from")]
    From,
    #[regex("times|multiplied[ \t\r\n]+with")]
    Times,
    #[regex("multiply")]
    Multiply,
    #[token("by")]
    By,
    #[regex("product[ \t\r\n]+of")]
    ProductOf,
    #[regex("over|divided[ \t\r\n]+by")]
    Over,
    #[token("divide")]
    Divide,
    #[token("either")]
    Either,
    #[token("or")]
    Or,
    #[regex("got[ \t\r\n]+one[ \t\r\n]+more[ \t\r\n]+")]
    OneMore,
    #[regex("got[ \t\r\n]+one[ \t\r\n]+less[ \t\r\n]+")]
    OneLess,
    #[token("using")]
    Using,
    #[token("at")]
    At,
    #[regex("next")]
    Next,
    #[regex("(with|to[ \t\r\n]+get)")]
    With,
    #[token("a")]
    A,
    #[token("the")]
    The,
    #[regex("I[ \t\r\n]+(said|sang|wrote)")]
    ISaid,
    #[regex("I[ \t\r\n]+(heard|read|asked)")]
    IHeard,
    #[regex("If|When")]
    If,
    #[regex("Otherwise|Or[ \t\r\n]+else")]
    Otherwise,
    #[regex("That's[ \t\r\n]+what[ \t\r\n]+I[ \t\r\n]+would[ \t\r\n]+do")]
    ThatsWhatIWouldDo,
    #[regex(
        "As[ \t\r\n]+long[ \t\r\n]+as|Here's[ \t\r\n]+what[ \t\r\n]+I[ \t\r\n]+did[ \t\r\n]+while"
    )]
    AsLongAs,
    #[regex("That's[ \t\r\n]+what[ \t\r\n]+I[ \t\r\n]+did")]
    ThatsWhatIDid,
    #[regex("Here's[ \t\r\n]+what[ \t\r\n]+I[ \t\r\n]+did")]
    HeresWhatIDid,
    #[regex("I[ \t\r\n]+did[ \t\r\n]+this[ \t\r\n]+(while|as[ \t\r\n]+long[ \t\r\n]+as)")]
    IDidThisWhile,
    #[regex("For[ \t\r\n]+every")]
    ForEvery,
    #[regex("Then[ \t\r\n]+you[ \t\r\n]+get")]
    ThenYouGet,
    #[regex("Today")]
    Today,
    #[regex("I[ \t\r\n]+learned")]
    ILearned,
    #[regex("That's[ \t\r\n]+all[ \t\r\n]+about")]
    ThatsAllAbout,
    #[regex("Did[ \t\r\n]+you[ \t\r\n]+know[ \t\r\n]+that")]
    DidYouKnowThat,
    #[regex("Dear[ \t\r\n]+Princess[ \t\r\n]+Celestia:")]
    DearPrincessCelestia,
    #[regex("Your[ \t\r\n]+faithful[ \t\r\n]+student")]
    YourFaithfulStudent,
    #[regex("[!,.:?…‽]")]
    Punctuation,
    #[token("number")]
    Number,
    #[regex("word|phrase|sentence|quote|name")]
    Chars,
    #[regex("logic|argument")]
    Boolean,
    #[regex("\"[^\"]*\"")]
    CharsLit,
    #[regex(r"-?[0-9]+(\.[0-9]+)?", priority = 2)]
    NumberLit,
    #[regex("yes|true|right|correct")]
    TrueLit,
    #[regex("no|false|wrong|incorrect")]
    FalseLit,
}

fn update_newline<'source, Token>(lex: &mut Lexer<'source, Token>)
where
    Token: Logos<'source, Source = str, Extras = Extras>,
{
    if lex.slice().as_bytes().last() == Some(&b'\n') {
        lex.extras.line_num += 1;
    }
}

impl Display for Bit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Bit::DearPrincessCelestia => write!(f, "'Dear Princess Celestia:'"),
            Bit::YourFaithfulStudent => write!(f, "'Your faithful student'"),
            Bit::Punctuation => write!(f, "one of !,.:?…‽"),
            Bit::A => write!(f, "'a'"),
            Bit::The => write!(f, "'the'"),
            Bit::CharsLit => write!(f, "chars"),
            Bit::NumberLit => write!(f, "number"),
            Bit::TrueLit => write!(f, "yes"),
            Bit::FalseLit => write!(f, "no"),
            other => write!(f, "{:#?}", other),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_lex_eq {
        ($lexer:expr) => {
            assert_eq!($lexer.next(), None, "slice: {}", $lexer.slice());
        };
        ($lexer:expr, $bit:expr) => {
            assert_eq!($lexer.next(), Some($bit), "slice: {}", $lexer.slice());
            assert_lex_eq!($lexer);
        };
        ($lexer:expr, $bit:expr, $($bits:expr),* $(,)?) => {
            assert_eq!($lexer.next(), Some($bit), "slice: {}", $lexer.slice());
            assert_lex_eq!($lexer, $($bits),+);
        };
    }

    #[test]
    fn whitespace() {
        let mut lexer = Bit::lexer("   ");

        assert_lex_eq!(lexer, Bit::Whitespace);
    }

    #[test]
    fn line_comment() {
        for bit in [
            "P.S.\n",
            "P.S. ignore everything here\n",
            "P.S.S here as well",
        ] {
            let mut lexer = Bit::lexer(bit);

            assert_lex_eq!(lexer, Bit::LineComment);
        }
    }

    #[test]
    fn multiline_comment() {
        {
            let mut lexer = Bit::lexer("()");
            assert_lex_eq!(lexer, Bit::OpenParen, Bit::CloseParen);
        }
        {
            let mut lexer = Bit::lexer("(ignore this)");

            assert_lex_eq!(
                lexer,
                Bit::OpenParen,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word,
                Bit::CloseParen,
            );
        }
        {
            let mut lexer = Bit::lexer("(ignore\nthis\nas\nwell)");

            assert_lex_eq!(
                lexer,
                Bit::OpenParen,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word,
                Bit::CloseParen,
            );
        }
        {
            let mut lexer = Bit::lexer("(ignore (nested) parens)");

            assert_lex_eq!(
                lexer,
                Bit::OpenParen,
                Bit::Word,
                Bit::Whitespace,
                Bit::OpenParen,
                Bit::Word,
                Bit::CloseParen,
                Bit::Whitespace,
                Bit::Word,
                Bit::CloseParen,
            );
        }
    }

    #[test]
    fn words() {
        {
            let mut lexer = Bit::lexer("test");

            assert_lex_eq!(lexer, Bit::Word);
        }
        {
            let mut lexer = Bit::lexer("An example letter");

            assert_lex_eq!(
                lexer,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word,
                Bit::Whitespace,
                Bit::Word
            );
        }
        {
            let mut lexer = Bit::lexer("tabs\ttest");

            assert_lex_eq!(lexer, Bit::Word, Bit::Whitespace, Bit::Word,);
        }
        {
            let mut lexer = Bit::lexer("punctuation?");

            assert_lex_eq!(lexer, Bit::Word, Bit::Punctuation);
        }
    }

    #[test]
    fn always() {
        let mut lexer = Bit::lexer("always");

        assert_lex_eq!(lexer, Bit::Always);
    }

    #[test]
    fn is() {
        for bit in ["is", "has", "had", "was"] {
            let mut lexer = Bit::lexer(bit);

            assert_lex_eq!(lexer, Bit::Is);
        }
    }

    #[test]
    fn have() {
        let mut lexer = Bit::lexer("have");

        assert_lex_eq!(lexer, Bit::Have);
    }

    #[test]
    fn like() {
        for bit in ["like", "likes", "liked"] {
            let mut lexer = Bit::lexer(bit);

            assert_lex_eq!(lexer, Bit::Like);
        }
    }

    #[test]
    fn and() {
        let mut lexer = Bit::lexer("and");

        assert_lex_eq!(lexer, Bit::And);
    }

    #[test]
    fn plus() {
        for bit in ["plus", "added to"] {
            let mut lexer = Bit::lexer(bit);

            assert_lex_eq!(lexer, Bit::Plus);
        }
    }

    #[test]
    fn add() {
        let mut lexer = Bit::lexer("add");

        assert_lex_eq!(lexer, Bit::Add);
    }

    #[test]
    fn i_learned() {
        let mut lexer = Bit::lexer("I learned");

        assert_lex_eq!(lexer, Bit::ILearned);
    }
}

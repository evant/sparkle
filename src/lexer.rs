use std::fmt::{Display, Formatter};

use logos::{Lexer, Logos, Skip};
use num_derive::{FromPrimitive, ToPrimitive};

pub struct Extras {
    pub line_num: usize,
}

impl Default for Extras {
    fn default() -> Self {
        Self { line_num: 0usize }
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Logos, FromPrimitive, ToPrimitive,
)]
#[logos(extras = Extras)]
pub enum Bit {
    #[error]
    Error,
    #[regex(r"[ \t\r\n]+", update_newline)]
    Whitespace,
    #[regex(r"P\.(S\.)[^\n]*\r?\n?", update_newline)]
    LineComment,
    #[token("(")]
    StartComment,
    #[regex("[^ \t\r\n!,.:?…‽\"]+")]
    Word,
    #[token("always")]
    Always,
    #[regex("(is|are)[ \t\r\n]+now|now[ \t\r\n]+likes?|becomes?")]
    Assign,
    #[regex("is|was|ha(s|d)")]
    Is,
    #[token("have")]
    Have,
    #[regex("like(s|d)?")]
    Like,
    #[token("and")]
    And,
    #[regex("plus|added[ \t\r\n]+to")]
    Plus,
    #[token("add")]
    Add,
    #[regex("minus|without")]
    Minus,
    #[regex("the[ \t\r\n]+difference[ \t\r\n]+between")]
    Difference,
    #[token("subtract")]
    Subtract,
    #[token("from")]
    From,
    #[regex("times|multiplied[ \t\r\n]+with")]
    Times,
    #[regex("over|divided[ \t\r\n]+by")]
    Over,
    #[regex("got[ \t\r\n]+one[ \t\r\n]+more[ \t\r\n]+")]
    OneMore,
    #[regex("got[ \t\r\n]+one[ \t\r\n]+less[ \t\r\n]+")]
    OneLess,
    #[token("using")]
    Using,
    #[token("at")]
    At,
    #[regex("the[ \t\r\n]+next")]
    TheNext,
    // #[regex("with|to[ \t\r\n]+get")]
    With,
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
    #[regex("Your[ \t\r\n]+faithful[ \t\r\n]+student:")]
    YourFaithfulStudent,
    #[regex("[!,.:?…‽]")]
    Punctuation,

    Letter,
    Topic,
    Paragraph,
    Var,
    Comment,
    Author,
    Identifier,
}

fn update_newline<'source, Token>(lex: &mut Lexer<'source, Token>)
where
    Token: Logos<'source, Source = str, Extras = Extras>,
{
    if lex.slice().as_bytes().last() == Some(&b'\n') {
        lex.extras.line_num += 1;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Logos)]
#[logos(extras = Extras)]
pub enum Comment {
    #[error]
    #[regex(r"[^()]+")]
    Skip,
    #[token("(")]
    Start,
    #[token(")")]
    End,
}

#[derive(Debug, Copy, Clone, PartialEq, Logos)]
#[logos(extras = Extras)]
pub enum Signature {
    #[error]
    #[regex(r"[^!,.:?…‽]+")]
    Author,
    #[regex("[!,.:?…‽]")]
    Punctuation,
}

impl Display for Bit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Bit::DearPrincessCelestia => "Dear Princess Celestia:",
                _ => "mystery",
            }
        )
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
        ($lexer:expr, $bit:expr, $($bits:expr),+) => {
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
            assert_eq!(lexer.next(), Some(Bit::StartComment));

            let mut lexer: Lexer<Comment> = lexer.morph();
            assert_lex_eq!(lexer, Comment::End);
        }
        {
            let mut lexer = Bit::lexer("(ignore this)");
            assert_eq!(lexer.next(), Some(Bit::StartComment));

            let mut lexer: Lexer<Comment> = lexer.morph();
            assert_lex_eq!(lexer, Comment::Skip, Comment::End);
        }
        {
            let mut lexer = Bit::lexer("(ignore\nthis\nas\nwell)");
            assert_eq!(lexer.next(), Some(Bit::StartComment));

            let mut lexer: Lexer<Comment> = lexer.morph();
            assert_lex_eq!(lexer, Comment::Skip, Comment::End);
        }
        {
            let mut lexer = Bit::lexer("(ignore (nested) parens)");
            assert_eq!(lexer.next(), Some(Bit::StartComment));

            let mut lexer: Lexer<Comment> = lexer.morph();
            assert_lex_eq!(
                lexer,
                Comment::Skip,
                Comment::Start,
                Comment::Skip,
                Comment::End,
                Comment::Skip,
                Comment::End
            );
        }
    }

    #[test]
    fn words() {
        {
            let mut lexer = Bit::lexer("word");

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
            let mut lexer = Bit::lexer("words\twith\ttabs");

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
    fn assign() {
        for bit in [
            "is now",
            "are now",
            "now like",
            "now likes",
            "become",
            "becomes",
        ] {
            let mut lexer = Bit::lexer(bit);

            assert_lex_eq!(lexer, Bit::Assign);
        }
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

    #[test]
    fn signature() {
        let mut lexer = Signature::lexer(" Twilight Sparkle.");

        assert_lex_eq!(lexer, Signature::Author, Signature::Punctuation);
    }
}

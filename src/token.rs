use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Kind {
    Colon,
    Comma,
    Divide,
    DoubleEquals,
    EndOfFile,
    EqualSign,
    FloatingPoint,
    Function,
    Greater,
    GreaterOrEqual,
    Identifier,
    Integer,
    LeftBrace,
    LeftParen,
    LeftSqBracket,
    Less,
    LessOrEqual,
    Let,
    Minus,
    Mut,
    Not,
    NotEquals,
    Period,
    Plus,
    Return,
    RightBrace,
    RightParen,
    RightSqBracket,
    SemiColon,
    Star,
    String,
    Unknown,
    Whitespace,
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    text: &'a [u8],
    offset: usize,
    kind: Kind,
}

impl<'a> Token<'a> {
    pub const fn new(text: &'a [u8], offset: usize, kind: Kind) -> Token<'a> {
        Token { text, offset, kind }
    }

    pub const fn eof(offset: usize) -> Token<'static> {
        Token {
            text: &[],
            offset,
            kind: Kind::EndOfFile,
        }
    }

    pub const fn kind(&self) -> Kind {
        self.kind
    }

    pub fn text(&self) -> String {
        String::from_utf8(self.text.to_vec()).unwrap()
    }

    pub const fn offset(&self) -> usize {
        self.offset
    }
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("text", &String::from_utf8_lossy(self.text))
            .field("kind", &self.kind)
            .finish()
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} '{}'",
            self.kind,
            self.text()
        )
    }
}

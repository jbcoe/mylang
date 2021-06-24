use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Colon,
    Comma,
    Divide,
    DoubleEquals,
    EndOfFile,
    EqualSign,
    FloatingPoint,
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
    kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn new(text: &'a [u8], offset: usize, kind: TokenKind) -> Token<'a> {
        Token { text, offset, kind }
    }

    pub fn eof(offset: usize) -> Token<'static> {
        Token {
            text: &[],
            offset,
            kind: TokenKind::EndOfFile,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn text(&self) -> String {
        String::from_utf8(self.text.to_vec()).unwrap()
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("text", &String::from_utf8_lossy(&self.text))
            .field("kind", &self.kind)
            .finish()
    }
}

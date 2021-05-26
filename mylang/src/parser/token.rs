use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(crate) enum TokenKind {
    Colon,
    Comma,
    DoubleEquals,
    EOF,
    Plus,
    Minus,
    Divide,
    Star,
    EqualSign,
    Identifier,
    Integer,
    LeftBrace,
    LeftParen,
    LeftSqBracket,
    Let,
    Mut,
    Period,
    Return,
    RightBrace,
    RightParen,
    RightSqBracket,
    SemiColon,
    String,
    Unknown,
    Whitespace,
}

pub(crate) struct Token<'a> {
    text: &'a [u8],
    kind: TokenKind,
}

impl<'a> Token<'a> {
    pub(crate) fn new(text: &'a [u8], kind: TokenKind) -> Token {
        Token { text, kind }
    }

    pub(crate) fn kind(&self) -> TokenKind {
        return self.kind;
    }

    pub(crate) fn text(&self) -> String {
        return String::from_utf8(self.text.to_vec()).unwrap();
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

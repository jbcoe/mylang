use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub(crate) enum Kind {
    Colon,
    Comma,
    Comment,
    Divide,
    DoubleEquals,
    EndOfFile,
    EqualSign,
    False,
    Float,
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
    True,
    Unknown,
    Whitespace,
}

#[derive(Clone, Copy)]
pub(crate) struct Token<'a> {
    text: &'a [u8],
    offset: usize,
    kind: Kind,
}

impl<'a> Token<'a> {
    pub(crate) const fn new(text: &'a [u8], offset: usize, kind: Kind) -> Token<'a> {
        Token { text, offset, kind }
    }

    pub(crate) const fn end_of_file(offset: usize) -> Token<'static> {
        Token {
            text: &[],
            offset,
            kind: Kind::EndOfFile,
        }
    }

    pub(crate) const fn kind(&self) -> Kind {
        self.kind
    }

    pub(crate) fn text(&self) -> String {
        String::from_utf8(self.text.to_vec()).unwrap()
    }

    pub(crate) const fn offset(&self) -> usize {
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
        write!(f, "{:?} '{}'", self.kind, self.text())
    }
}

#[cfg(test)]
mod tests {

    use super::{Kind, Token};

    #[test]
    fn utf8_token_can_be_constructed_and_unicode_read_from_it() {
        // https://unicode-table.com/en/1F496/
        let sparkle_bytes = vec![240, 159, 146, 150];
        let sparkle_heart = Token::new(&sparkle_bytes, 0, Kind::Identifier);
        assert_eq!(sparkle_heart.text(), "\u{1f496}");

        // Simple checks to ensure code coverage is high and accessor functions do what is expected.
        assert_eq!(sparkle_heart.kind(), Kind::Identifier);
        assert_eq!(sparkle_heart.offset(), 0);
    }

    #[test]
    fn end_of_file_token() {
        let end_of_file = Token::end_of_file(0);
        assert_eq!(end_of_file.text(), "");

        // Simple checks to ensure code coverage is high and accessor functions do what is expected.
        assert_eq!(end_of_file.kind(), Kind::EndOfFile);
        assert_eq!(end_of_file.offset(), 0);
    }
}

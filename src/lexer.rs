use std::fmt;
use std::str;
use std::vec::Vec;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Colon,
    Comma,
    DoubleEquals,
    Eof,
    Plus,
    Minus,
    Divide,
    Star,
    EqualSign,
    Identifier,
    Integer,
    FloatingPoint,
    LeftBrace,
    LeftParen,
    LeftSqBracket,
    Let,
    Mut,

    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    NotEquals,
    Not,

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

#[derive(Clone, Copy)]
pub struct Token<'a> {
    text: &'a [u8],
    offset: usize,
    kind: TokenKind,
}

impl<'a> Token<'a> {
    pub fn eof(offset: usize) -> Token<'static> {
        Token {
            text: &[],
            offset,
            kind: TokenKind::Eof,
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

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn is_empty(&self) -> bool {
        self.text.len() == 0
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

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    byte: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let mut lexer = Lexer {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            byte: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.byte = 0;
            return;
        } else {
            self.byte = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        if self.position >= self.input.len() {
            self.byte = 0;
        } else {
            self.byte = self.input[self.position];
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        }
        char::from(self.input[self.read_position])
    }

    fn text_range(&self, start: usize) -> &'a [u8] {
        &self.input[start..self.read_position]
    }

    // Consumes the Lexer
    pub fn tokens(mut self) -> Vec<Token<'a>> {
        let mut tokens = vec![];
        loop {
            let t = self.next_token();
            tokens.push(t);
            if tokens.last().unwrap().kind() == TokenKind::Eof {
                return tokens;
            }
        }
    }

    pub(crate) fn next_token(&mut self) -> Token<'a> {
        let token: Token;

        let c = char::from(self.byte);
        match c {
            '\0' => {
                token = Token::eof(self.read_position);
            }
            '+' => {
                token = self.char_token(TokenKind::Plus);
            }
            '-' => {
                token = self.char_token(TokenKind::Minus);
            }
            '*' => {
                token = self.char_token(TokenKind::Star);
            }
            '/' => {
                token = self.char_token(TokenKind::Divide);
            }
            '.' => {
                if self.peek_char().is_ascii_digit() {
                    if let Some(t) = self.read_decimal_part(self.position) {
                        token = t;
                    } else {
                        token = self.char_token(TokenKind::Period);
                    }
                } else {
                    token = self.char_token(TokenKind::Period);
                }
            }
            '=' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    token = self.text_token(start, TokenKind::DoubleEquals);
                }
                _ => {
                    token = self.char_token(TokenKind::EqualSign);
                }
            },
            '>' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    token = self.text_token(start, TokenKind::GreaterOrEqual);
                }
                _ => {
                    token = self.char_token(TokenKind::Greater);
                }
            },
            '<' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    token = self.text_token(start, TokenKind::LessOrEqual);
                }
                _ => {
                    token = self.char_token(TokenKind::Less);
                }
            },
            '!' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    token = self.text_token(start, TokenKind::NotEquals);
                }
                _ => {
                    token = self.char_token(TokenKind::Not);
                }
            },
            ',' => {
                token = self.char_token(TokenKind::Comma);
            }
            ';' => {
                token = self.char_token(TokenKind::SemiColon);
            }
            ':' => {
                token = self.char_token(TokenKind::Colon);
            }
            '(' => {
                token = self.char_token(TokenKind::LeftParen);
            }
            ')' => {
                token = self.char_token(TokenKind::RightParen);
            }
            '{' => {
                token = self.char_token(TokenKind::LeftBrace);
            }
            '}' => {
                token = self.char_token(TokenKind::RightBrace);
            }
            '[' => {
                token = self.char_token(TokenKind::LeftSqBracket);
            }
            ']' => {
                token = self.char_token(TokenKind::RightSqBracket);
            }
            '"' => {
                token = self.read_string();
            }
            _ => {
                // read whitespace
                if c.is_whitespace() {
                    token = self.read_whitespace();
                }
                // read keyword or identifier
                else if c.is_ascii_alphabetic() {
                    if let Some(t) = self.read_keyword() {
                        token = t;
                    } else if let Some(t) = self.read_identifier() {
                        token = t;
                    } else {
                        token = self.read_junk();
                    }
                }
                // read number
                else if c.is_ascii_digit() {
                    if let Some(t) = self.read_number() {
                        token = t;
                    } else {
                        token = self.read_junk();
                    }
                }
                // read junk
                else {
                    token = self.read_junk();
                }
            }
        }

        self.read_char();
        token
    }

    fn char_token(&self, kind: TokenKind) -> Token<'a> {
        self.text_token(self.position, kind)
    }

    fn text_token(&self, start: usize, kind: TokenKind) -> Token<'a> {
        return Token {
            text: &self.text_range(start),
            offset: start,
            kind,
        };
    }

    fn read_junk(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() {
            self.read_char();
        }

        self.text_token(start, TokenKind::Unknown)
    }

    fn read_whitespace(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        self.text_token(start, TokenKind::Whitespace)
    }

    fn read_identifier(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.read_char();
        }

        Some(self.text_token(start, TokenKind::Identifier))
    }

    fn read_keyword(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() {
            self.read_char();
        }

        let token_text = str::from_utf8(self.text_range(start));
        match token_text {
            Ok("let") => Some(self.text_token(start, TokenKind::Let)),
            Ok("mut") => Some(self.text_token(start, TokenKind::Mut)),
            Ok("return") => Some(self.text_token(start, TokenKind::Return)),
            _ => {
                self.reset(start);
                None
            }
        }
    }

    fn read_string(&mut self) -> Token<'a> {
        let start = self.position;
        self.read_char(); // Advance past '"'.
        loop {
            match self.peek_char() {
                '\0' => {
                    return self.text_token(start, TokenKind::Unknown);
                }
                '"' => {
                    break;
                }
                _ => {
                    self.read_char();
                }
            }
        }
        self.read_char(); // Consume closing '"'.
        self.text_token(start, TokenKind::String)
    }

    fn read_number(&mut self) -> Option<Token<'a>> {
        let start = self.position;

        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        let p = self.peek_char();
        match p {
            '.' => self.read_decimal_part(start),
            'e' | 'E' => self.read_exponent(start),
            _ => self.read_number_final_integer(start),
        }
    }

    fn read_number_final_integer(&mut self, start: usize) -> Option<Token<'a>> {
        let p = self.peek_char();
        if p.is_ascii_alphabetic() {
            self.reset(start);
            return None;
        }

        Some(self.text_token(start, TokenKind::Integer))
    }

    fn read_decimal_part(&mut self, start: usize) -> Option<Token<'a>> {
        self.read_char(); // consume the '.'

        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        let p = self.peek_char();
        match p {
            'e' | 'E' => self.read_exponent(start),
            _ => self.read_number_final_floating_point(start),
        }
    }

    fn read_exponent(&mut self, start: usize) -> Option<Token<'a>> {
        self.read_char(); // consume the 'e' or 'E'

        let p = self.peek_char();
        if p == '+' || p == '-' {
            self.read_char();
        } else if !p.is_ascii_digit() {
            return self.read_number_cleanup_junk(start);
        }

        self.read_number_final_floating_point(start)
    }

    fn read_number_final_floating_point(&mut self, start: usize) -> Option<Token<'a>> {
        while self.peek_char().is_ascii_digit() {
            self.read_char()
        }

        let p = self.peek_char();
        if p.is_ascii_alphabetic() || p == '.' {
            // We no longer expect any decimal points, so encountering an
            // alphabetic char or a period indicates we have a junk token.
            // Other symbols, including '+' and '-', are not considered here
            // as they form the beginning of the next token.
            return self.read_number_cleanup_junk(start);
        }

        Some(self.text_token(start, TokenKind::FloatingPoint))
    }

    fn read_number_cleanup_junk(&mut self, start: usize) -> Option<Token<'a>> {
        // What we have read is not compatible with a number.  We cannot
        // rely on the general junk reader to tidy up because we may have
        // decimal points, exponent symbols and sign symbols in the mix.
        //
        // While trying to clean up after a malformed numeric token, we
        // want to consume any characters that could potentially be part
        // of a numeric token. This may consume characters that would
        // otherwise form a valid subsequent token, but if we do not do
        // this, we can end up with `Unknown` tokens that look like they
        // should be valid tokens. We therefore opt to greedily consume
        // characters to make the unknown token visually distinct from
        // a valid token.
        fn in_numeric_charset(ch: char) -> bool {
            if ch.is_ascii_alphanumeric() {
                return true;
            }
            matches!(ch, 'e' | 'E' | '.' | '+' | '-')
        }

        while in_numeric_charset(self.peek_char()) {
            self.read_char();
        }

        Some(self.text_token(start, TokenKind::Unknown))
    }
}

#[cfg(test)]
mod lexer_test {
    use super::Lexer;
    use super::TokenKind;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
        skip_whitespace: bool,
        expected_tokens: Vec<(&'static str, TokenKind)>,
    }

    #[test]
    fn lexer_tests() {
        let test_cases = vec![
            TestCase {
                input: "let myPet  =  10dog01 ;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    ("  ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    ("  ", TokenKind::Whitespace),
                    ("10dog01", TokenKind::Unknown),
                    (" ", TokenKind::Whitespace),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let myPet = "Timmy the dog";"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    (r#""Timmy the dog""#, TokenKind::String),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let mut myPet = "Timmy the dog just goes on forever"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("mut", TokenKind::Mut),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    // Note the unbalanced quotes in the string below.
                    (r#""Timmy the dog just goes on forever"#, TokenKind::Unknown),
                ],
            },
            TestCase {
                input: "let myNumber = 1001;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myNumber", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("1001", TokenKind::Integer),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let myValue = anotherValue;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myValue", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("anotherValue", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let value = value0;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("value", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("value0", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let my_value = 10;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    ("my_value", TokenKind::Identifier),
                    ("=", TokenKind::EqualSign),
                    ("10", TokenKind::Integer),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let add = (lhs, rhs) { return lhs + rhs; };",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    ("add", TokenKind::Identifier),
                    ("=", TokenKind::EqualSign),
                    ("(", TokenKind::LeftParen),
                    ("lhs", TokenKind::Identifier),
                    (",", TokenKind::Comma),
                    ("rhs", TokenKind::Identifier),
                    (")", TokenKind::RightParen),
                    ("{", TokenKind::LeftBrace),
                    ("return", TokenKind::Return),
                    ("lhs", TokenKind::Identifier),
                    ("+", TokenKind::Plus),
                    ("rhs", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                    ("}", TokenKind::RightBrace),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a > b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    (">", TokenKind::Greater),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a < b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("<", TokenKind::Less),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a >= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    (">=", TokenKind::GreaterOrEqual),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a <= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("<=", TokenKind::LessOrEqual),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a == b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("==", TokenKind::DoubleEquals),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a != b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("!=", TokenKind::NotEquals),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "3",
                skip_whitespace: false,
                expected_tokens: vec![("3", TokenKind::Integer)],
            },
            TestCase {
                input: "314",
                skip_whitespace: false,
                expected_tokens: vec![("314", TokenKind::Integer)],
            },
            TestCase {
                input: "3.14",
                skip_whitespace: false,
                expected_tokens: vec![("3.14", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "3.",
                skip_whitespace: false,
                expected_tokens: vec![("3.", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: ".14",
                skip_whitespace: false,
                expected_tokens: vec![(".14", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "3e8",
                skip_whitespace: false,
                expected_tokens: vec![("3e8", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "0.314e1",
                skip_whitespace: false,
                expected_tokens: vec![("0.314e1", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "9.1e-31",
                skip_whitespace: false,
                expected_tokens: vec![("9.1e-31", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "6.02e+23",
                skip_whitespace: false,
                expected_tokens: vec![("6.02e+23", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "2e",
                skip_whitespace: false,
                expected_tokens: vec![("2e", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.e",
                skip_whitespace: false,
                expected_tokens: vec![("2.e", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.4f",
                skip_whitespace: false,
                expected_tokens: vec![("2.4f", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.4e3a",
                skip_whitespace: false,
                expected_tokens: vec![("2.4e3a", TokenKind::Unknown)],
            },
            TestCase {
                input: "123f+4.2e-3",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("123f", TokenKind::Unknown),
                    ("+", TokenKind::Plus),
                    ("4.2e-3", TokenKind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4.56",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4.56", TokenKind::Unknown)],
            },
            TestCase {
                input: "1.23e-4+3.2e-5",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("1.23e-4", TokenKind::FloatingPoint),
                    ("+", TokenKind::Plus),
                    ("3.2e-5", TokenKind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4e-3.2",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4e-3.2", TokenKind::Unknown)],
            },
        ];

        for test_case in test_cases.iter() {
            let mut lexer = Lexer::new(test_case.input);
            for expected_token in test_case.expected_tokens.iter() {
                let mut t = lexer.next_token();
                while t.kind() == TokenKind::Whitespace && test_case.skip_whitespace {
                    t = lexer.next_token();
                }
                assert_eq!(t.text(), expected_token.0);
                assert_eq!(t.kind(), expected_token.1);
            }
            assert_eq!(lexer.next_token().kind(), TokenKind::Eof);
        }
    }
}

use crate::token::{Kind, Token};
use std::{str, vec::Vec};

pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    byte: u8,
}

impl<'a> Lexer<'a> {
    #[must_use]
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
        self.byte = match self.input.get(self.read_position) {
            None => 0,
            Some(b) => {
                self.position = self.read_position;
                self.read_position += 1;
                *b
            }
        }
    }

    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        self.byte = match self.input.get(self.position) {
            None => 0,
            Some(b) => *b,
        }
    }

    fn peek_char(&self) -> char {
        match self.input.get(self.read_position) {
            None => '\0',
            Some(c) => char::from(*c),
        }
    }

    fn text_range(&self, start: usize) -> &'a [u8] {
        &self.input[start..self.read_position]
    }

    #[must_use]
    /// Consumes the lexer, producing a Vec of lexed Tokens

    pub fn tokens(mut self) -> Vec<Token<'a>> {
        let mut tokens = vec![];
        loop {
            let t = self.next_token();
            tokens.push(t);
            if t.kind() == Kind::EndOfFile {
                return tokens;
            }
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        let c = char::from(self.byte);
        let token = match c {
            '\0' => Token::eof(self.read_position),
            '+' => self.char_token(Kind::Plus),
            '-' => self.char_token(Kind::Minus),
            '*' => self.char_token(Kind::Star),
            '/' => self.char_token(Kind::Divide),
            '.' => {
                if self.peek_char().is_ascii_digit() {
                    self.read_decimal_part(self.position)
                } else {
                    self.char_token(Kind::Period)
                }
            }
            '=' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    self.text_token(start, Kind::DoubleEquals)
                }
                _ => self.char_token(Kind::EqualSign),
            },
            '>' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    self.text_token(start, Kind::GreaterOrEqual)
                }
                _ => self.char_token(Kind::Greater),
            },
            '<' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    self.text_token(start, Kind::LessOrEqual)
                }
                _ => self.char_token(Kind::Less),
            },
            '!' => match self.peek_char() {
                '=' => {
                    let start = self.position;
                    self.read_char();
                    self.text_token(start, Kind::NotEquals)
                }
                _ => self.char_token(Kind::Not),
            },
            ',' => self.char_token(Kind::Comma),
            ';' => self.char_token(Kind::SemiColon),
            ':' => self.char_token(Kind::Colon),
            '(' => self.char_token(Kind::LeftParen),
            ')' => self.char_token(Kind::RightParen),
            '{' => self.char_token(Kind::LeftBrace),
            '}' => self.char_token(Kind::RightBrace),
            '[' => self.char_token(Kind::LeftSqBracket),
            ']' => self.char_token(Kind::RightSqBracket),
            '"' => self.read_string(),
            _ => {
                // read whitespace
                if c.is_whitespace() {
                    self.read_whitespace()
                }
                // read keyword or identifier
                else if c.is_ascii_alphabetic() {
                    self.read_keyword()
                        .map_or_else(|| self.read_identifier(), |t| t)
                }
                // read number
                else if c.is_ascii_digit() {
                    self.read_number().map_or_else(|| self.read_junk(), |t| t)
                }
                // read junk
                else {
                    self.read_junk()
                }
            }
        };

        self.read_char();
        token
    }

    fn char_token(&self, kind: Kind) -> Token<'a> {
        self.text_token(self.position, kind)
    }

    fn text_token(&self, start: usize, kind: Kind) -> Token<'a> {
        return Token::new(self.text_range(start), start, kind);
    }

    fn read_junk(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() {
            self.read_char();
        }

        self.text_token(start, Kind::Unknown)
    }

    fn read_whitespace(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        self.text_token(start, Kind::Whitespace)
    }

    fn read_identifier(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.read_char();
        }

        self.text_token(start, Kind::Identifier)
    }

    fn read_keyword(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() {
            self.read_char();
        }

        let token_text = str::from_utf8(self.text_range(start));
        match token_text {
            Ok("func") => Some(self.text_token(start, Kind::Function)),
            Ok("let") => Some(self.text_token(start, Kind::Let)),
            Ok("mut") => Some(self.text_token(start, Kind::Mut)),
            Ok("return") => Some(self.text_token(start, Kind::Return)),
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
                    return self.text_token(start, Kind::Unknown);
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
        self.text_token(start, Kind::String)
    }

    fn read_number(&mut self) -> Option<Token<'a>> {
        let start = self.position;

        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        let p = self.peek_char();
        match p {
            '.' => Some(self.read_decimal_part(start)),
            'e' | 'E' => Some(self.read_exponent(start)),
            _ => self.read_number_final_integer(start),
        }
    }

    fn read_number_final_integer(&mut self, start: usize) -> Option<Token<'a>> {
        let p = self.peek_char();
        if p.is_ascii_alphabetic() {
            self.reset(start);
            return None;
        }

        Some(self.text_token(start, Kind::Integer))
    }

    fn read_decimal_part(&mut self, start: usize) -> Token<'a> {
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

    fn read_exponent(&mut self, start: usize) -> Token<'a> {
        self.read_char(); // consume the 'e' or 'E'

        let p = self.peek_char();
        if p == '+' || p == '-' {
            self.read_char();
            self.read_number_final_floating_point(start)
        } else if p.is_ascii_digit() {
            self.read_number_final_floating_point(start)
        } else {
            self.read_number_cleanup_junk(start)
        }
    }

    fn read_number_final_floating_point(&mut self, start: usize) -> Token<'a> {
        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        let p = self.peek_char();
        if p.is_ascii_alphabetic() || p == '.' {
            // We no longer expect any decimal points, so encountering an
            // alphabetic char or a period indicates we have a junk token.
            // Other symbols, including '+' and '-', are not considered here
            // as they form the beginning of the next token.
            return self.read_number_cleanup_junk(start);
        }

        self.text_token(start, Kind::FloatingPoint)
    }

    fn read_number_cleanup_junk(&mut self, start: usize) -> Token<'a> {
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
        const fn in_numeric_charset(ch: char) -> bool {
            if ch.is_ascii_alphanumeric() {
                return true;
            }
            matches!(ch, 'e' | 'E' | '.' | '+' | '-')
        }

        while in_numeric_charset(self.peek_char()) {
            self.read_char();
        }

        self.text_token(start, Kind::Unknown)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
        skip_whitespace: bool,
        expected_tokens: Vec<(&'static str, Kind)>,
    }

    #[test]
    fn lexer() {
        let test_cases = vec![
            TestCase {
                input: "let myPet  =  10dog01 ;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    ("  ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    ("  ", Kind::Whitespace),
                    ("10dog01", Kind::Unknown),
                    (" ", Kind::Whitespace),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let myPet = "Timmy the dog";"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    (r#""Timmy the dog""#, Kind::String),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let mut myPet = "Timmy the dog just goes on forever"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("mut", Kind::Mut),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    // Note the unbalanced quotes in the string below.
                    (r#""Timmy the dog just goes on forever"#, Kind::Unknown),
                ],
            },
            TestCase {
                input: "let myNumber = 1001;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myNumber", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("1001", Kind::Integer),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let myValue = anotherValue;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myValue", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("anotherValue", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let value = value0;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("value", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("value0", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let my_value = 10;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    ("my_value", Kind::Identifier),
                    ("=", Kind::EqualSign),
                    ("10", Kind::Integer),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let add = func (lhs, rhs) { return lhs + rhs; };",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    ("add", Kind::Identifier),
                    ("=", Kind::EqualSign),
                    ("func", Kind::Function),
                    ("(", Kind::LeftParen),
                    ("lhs", Kind::Identifier),
                    (",", Kind::Comma),
                    ("rhs", Kind::Identifier),
                    (")", Kind::RightParen),
                    ("{", Kind::LeftBrace),
                    ("return", Kind::Return),
                    ("lhs", Kind::Identifier),
                    ("+", Kind::Plus),
                    ("rhs", Kind::Identifier),
                    (";", Kind::SemiColon),
                    ("}", Kind::RightBrace),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a > b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    (">", Kind::Greater),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a < b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("<", Kind::Less),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a >= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    (">=", Kind::GreaterOrEqual),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a <= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("<=", Kind::LessOrEqual),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a == b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("==", Kind::DoubleEquals),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a != b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("!=", Kind::NotEquals),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "3",
                skip_whitespace: false,
                expected_tokens: vec![("3", Kind::Integer)],
            },
            TestCase {
                input: "314",
                skip_whitespace: false,
                expected_tokens: vec![("314", Kind::Integer)],
            },
            TestCase {
                input: "3.14",
                skip_whitespace: false,
                expected_tokens: vec![("3.14", Kind::FloatingPoint)],
            },
            TestCase {
                input: "3.",
                skip_whitespace: false,
                expected_tokens: vec![("3.", Kind::FloatingPoint)],
            },
            TestCase {
                input: ".14",
                skip_whitespace: false,
                expected_tokens: vec![(".14", Kind::FloatingPoint)],
            },
            TestCase {
                input: "3e8",
                skip_whitespace: false,
                expected_tokens: vec![("3e8", Kind::FloatingPoint)],
            },
            TestCase {
                input: "0.314e1",
                skip_whitespace: false,
                expected_tokens: vec![("0.314e1", Kind::FloatingPoint)],
            },
            TestCase {
                input: "9.1e-31",
                skip_whitespace: false,
                expected_tokens: vec![("9.1e-31", Kind::FloatingPoint)],
            },
            TestCase {
                input: "6.02e+23",
                skip_whitespace: false,
                expected_tokens: vec![("6.02e+23", Kind::FloatingPoint)],
            },
            TestCase {
                input: "2e",
                skip_whitespace: false,
                expected_tokens: vec![("2e", Kind::Unknown)],
            },
            TestCase {
                input: "2.e",
                skip_whitespace: false,
                expected_tokens: vec![("2.e", Kind::Unknown)],
            },
            TestCase {
                input: "2.4f",
                skip_whitespace: false,
                expected_tokens: vec![("2.4f", Kind::Unknown)],
            },
            TestCase {
                input: "2.4e3a",
                skip_whitespace: false,
                expected_tokens: vec![("2.4e3a", Kind::Unknown)],
            },
            TestCase {
                input: "123f+4.2e-3",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("123f", Kind::Unknown),
                    ("+", Kind::Plus),
                    ("4.2e-3", Kind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4.56",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4.56", Kind::Unknown)],
            },
            TestCase {
                input: "1.23e-4+3.2e-5",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("1.23e-4", Kind::FloatingPoint),
                    ("+", Kind::Plus),
                    ("3.2e-5", Kind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4e-3.2",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4e-3.2", Kind::Unknown)],
            },
        ];

        for test_case in test_cases.iter() {
            let mut lexer = Lexer::new(test_case.input);
            for expected_token in test_case.expected_tokens.iter() {
                let mut t = lexer.next_token();
                while t.kind() == Kind::Whitespace && test_case.skip_whitespace {
                    t = lexer.next_token();
                }
                assert_eq!(t.text(), expected_token.0);
                assert_eq!(t.kind(), expected_token.1);
            }
            assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
        }
    }
}

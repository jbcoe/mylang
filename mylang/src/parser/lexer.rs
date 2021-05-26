use super::token::{Token, TokenKind};
use std::str;
pub(crate) struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    byte: u8,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Lexer {
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
        self.read_position = self.read_position + 1;
    }

    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        if self.position >= self.input.len() {
            self.byte = 0;
            return;
        } else {
            self.byte = self.input[self.position];
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        }
        return char::from(self.input[self.read_position]);
    }

    fn text_range(&self, start: usize) -> &'a [u8] {
        return &self.input[start..self.read_position];
    }

    pub(crate) fn next_token(&mut self) -> Token<'a> {
        let token: Token;
        loop {
            let c = char::from(self.byte);
            match c {
                '\0' => {
                    token = self.char_token(TokenKind::EOF);
                    break;
                }
                '+' => {
                    token = self.char_token(TokenKind::Plus);
                    break;
                }
                '-' => {
                    token = self.char_token(TokenKind::Minus);
                    break;
                }
                '*' => {
                    token = self.char_token(TokenKind::Star);
                    break;
                }
                '/' => {
                    token = self.char_token(TokenKind::Divide);
                    break;
                }
                '.' => {
                    token = self.char_token(TokenKind::Period);
                    break;
                }
                '=' => match self.peek_char() {
                    '=' => {
                        let start = self.position;
                        self.read_char();
                        token = self.text_token(start, TokenKind::DoubleEquals);
                        break;
                    }
                    _ => {
                        token = self.char_token(TokenKind::EqualSign);
                        break;
                    }
                },
                ',' => {
                    token = self.char_token(TokenKind::Comma);
                    break;
                }
                ';' => {
                    token = self.char_token(TokenKind::SemiColon);
                    break;
                }
                ':' => {
                    token = self.char_token(TokenKind::Colon);
                    break;
                }
                '(' => {
                    token = self.char_token(TokenKind::LeftParen);
                    break;
                }
                ')' => {
                    token = self.char_token(TokenKind::RightParen);
                    break;
                }
                '{' => {
                    token = self.char_token(TokenKind::LeftBrace);
                    break;
                }
                '}' => {
                    token = self.char_token(TokenKind::RightBrace);
                    break;
                }
                '[' => {
                    token = self.char_token(TokenKind::LeftSqBracket);
                    break;
                }
                ']' => {
                    token = self.char_token(TokenKind::RightSqBracket);
                    break;
                }
                '"' => {
                    token = self.read_string();
                    break;
                }
                _ => {
                    // read whitespace
                    if c.is_whitespace() {
                        token = self.read_whitespace();
                        break;
                    }
                    // read keyword or identifier
                    if c.is_alphabetic() {
                        if let Some(t) = self.read_keyword() {
                            token = t;
                            break;
                        }
                        if let Some(t) = self.read_identifier() {
                            token = t;
                            break;
                        }
                    }
                    // read integer
                    if c.is_numeric() {
                        if let Some(t) = self.read_integer() {
                            token = t;
                            break;
                        }
                    }
                    // read junk
                    token = self.read_junk();
                    break;
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
        return Token::new(&self.text_range(start), kind);
    }

    fn read_junk(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_alphanumeric() {
            self.read_char();
        }

        return self.text_token(start, TokenKind::Unknown);
    }

    fn read_whitespace(&mut self) -> Token<'a> {
        let start = self.position;
        while self.peek_char().is_whitespace() {
            self.read_char();
        }

        return self.text_token(start, TokenKind::Whitespace);
    }

    fn read_identifier(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_alphanumeric() {
            self.read_char();
        }

        return Some(self.text_token(start, TokenKind::Identifier));
    }

    fn read_keyword(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_alphanumeric() {
            self.read_char();
        }

        let token_text = str::from_utf8(self.text_range(start));
        match token_text {
            Ok("let") => {
                return Some(self.text_token(start, TokenKind::Let));
            }
            Ok("mut") => {
                return Some(self.text_token(start, TokenKind::Mut));
            }
            Ok("return") => {
                return Some(self.text_token(start, TokenKind::Return));
            }
            _ => {
                self.reset(start);
                return None;
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
        return self.text_token(start, TokenKind::String);
    }

    fn read_integer(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_numeric() {
            self.read_char();
        }

        let p = self.peek_char();
        if p.is_alphabetic() {
            self.reset(start);
            return None;
        }

        return Some(self.text_token(start, TokenKind::Integer));
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
            assert_eq!(lexer.next_token().kind(), TokenKind::EOF);
        }
    }
}

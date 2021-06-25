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
        if self.read_position >= self.input.len() {
            return '\0';
        }
        char::from(self.input[self.read_position])
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

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

    pub(crate) fn tokens(mut self) -> Vec<Token<'a>> {
        let mut tokens = vec![];
        loop {
            let t = self.next_token();
            tokens.push(t);
            if t.kind() == Kind::EndOfFile {
                break;
            }
        }
        tokens
    }

    pub(crate) fn next_token(&mut self) -> Token<'a> {
        let c = char::from(self.byte);
        let token = match c {
            '\0' => Token::end_of_file(self.read_position),
            '+' => self.char_token(Kind::Plus),
            '-' => self.char_token(Kind::Minus),
            '*' => self.char_token(Kind::Star),
            '/' => self.char_token(Kind::Divide),
            '.' => self.read_period_or_decimal_token(),
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
            '#' => self.read_comment(),
            '"' => self.read_string(),
            _ => self.read_non_special_token(c),
        };

        self.read_char();
        token
    }

    fn char_token(&self, kind: Kind) -> Token<'a> {
        self.text_token(self.position, kind)
    }

    fn text_token(&self, start: usize, kind: Kind) -> Token<'a> {
        Token::new(self.text_range(start), start, kind)
    }

    fn read_non_special_token(&mut self, c: char) -> Token<'a> {
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

    fn read_period_or_decimal_token(&mut self) -> Token<'a> {
        if self.peek_char().is_ascii_digit() {
            self.read_decimal_part(self.position)
        } else {
            self.char_token(Kind::Period)
        }
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

    fn read_comment(&mut self) -> Token<'a> {
        let start = self.position;
        self.read_char();
        loop {
            match char::from(self.byte) {
                '\n' | '\0' => break,
                _ => self.read_char(),
            }
        }
        self.text_token(start, Kind::Comment)
    }

    fn read_keyword(&mut self) -> Option<Token<'a>> {
        let start = self.position;
        while self.peek_char().is_ascii_alphanumeric() {
            self.read_char();
        }

        let token_text = str::from_utf8(self.text_range(start));
        if let Ok(text) = token_text {
            if let Some(kind) = Token::keyword(text) {
                Some(self.text_token(start, kind))
            } else {
                self.reset(start);
                None
            }
        } else {
            self.reset(start);
            None
        }
    }
    fn read_string(&mut self) -> Token<'a> {
        let start = self.position;
        self.read_char(); // Advance past '"'.
        let mut kind = Kind::String;
        loop {
            match self.peek_char() {
                '\0' => {
                    kind = Kind::Unknown;
                    break;
                }
                '"' => {
                    self.read_char(); // Consume closing '"'.
                    break;
                }
                _ => self.read_char(),
            }
        }
        self.text_token(start, kind)
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
            None
        } else {
            Some(self.text_token(start, Kind::Integer))
        }
    }

    fn read_decimal_part(&mut self, start: usize) -> Token<'a> {
        self.read_char(); // consume the '.'

        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        match self.peek_char() {
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
            self.read_number_cleanup_junk(start)
        } else {
            self.text_token(start, Kind::Float)
        }
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
            ch.is_ascii_alphanumeric() || matches!(ch, 'e' | 'E' | '.' | '+' | '-')
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

    macro_rules! lexer_test_case {
        ( name: $test_name:ident, input: $input:expr, expected_tokens:$expected_tokens:expr,) => {
            #[test]
            fn $test_name() {
                let mut lexer = Lexer::new($input);
                for expected_token in $expected_tokens {
                    let mut t = lexer.next_token();
                    while t.kind() == Kind::Whitespace {
                        t = lexer.next_token();
                    }
                    assert_eq!(t.text(), expected_token.0);
                    assert_eq!(t.kind(), expected_token.1);
                }
                assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
            }
        };
        ( name: $test_name:ident, input: $input:expr, expected_tokens:$expected_tokens:expr, check_whitespace: true,) => {
            #[test]
            fn $test_name() {
                let mut lexer = Lexer::new($input);
                for expected_token in $expected_tokens {
                    let t = lexer.next_token();
                    assert_eq!(t.text(), expected_token.0);
                    assert_eq!(t.kind(), expected_token.1);
                }
                assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
            }
        };
    }

    lexer_test_case! {
        name: let_statement_with_string,
        input: r#"let myPet = "Timmy the dog";"#,
        expected_tokens: &[
            ("let", Kind::Let),
            (" ", Kind::Whitespace),
            ("myPet", Kind::Identifier),
            (" ", Kind::Whitespace),
            ("=", Kind::EqualSign),
            (" ", Kind::Whitespace),
            (r#""Timmy the dog""#, Kind::String),
            (";", Kind::SemiColon),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_statement_with_bad_identifier,
        input: "let myPet  =  10dog01 ;",
        expected_tokens: &[
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
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_statement_with_unfinished_string,
        input: r#"let mut myPet = "string with unbalanced quotes"#,
        expected_tokens: &[
            ("let", Kind::Let),
            (" ", Kind::Whitespace),
            ("mut", Kind::Mut),
            (" ", Kind::Whitespace),
            ("myPet", Kind::Identifier),
            (" ", Kind::Whitespace),
            ("=", Kind::EqualSign),
            (" ", Kind::Whitespace),
            (r#""string with unbalanced quotes"#, Kind::Unknown),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_statement_with_integer,
        input: "let myNumber = 1001;",
        expected_tokens: &[
            ("let", Kind::Let),
            (" ", Kind::Whitespace),
            ("myNumber", Kind::Identifier),
            (" ", Kind::Whitespace),
            ("=", Kind::EqualSign),
            (" ", Kind::Whitespace),
            ("1001", Kind::Integer),
            (";", Kind::SemiColon),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_statement_with_identifier,
        input: "let myValue = anotherValue;",
        expected_tokens: &[
            ("let", Kind::Let),
            (" ", Kind::Whitespace),
            ("myValue", Kind::Identifier),
            (" ", Kind::Whitespace),
            ("=", Kind::EqualSign),
            (" ", Kind::Whitespace),
            ("anotherValue", Kind::Identifier),
            (";", Kind::SemiColon),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
    name: let_statement_with_identifier_containing_number,
    input: "let value = value0;",
        expected_tokens: &[
            ("let", Kind::Let),
            (" ", Kind::Whitespace),
            ("value", Kind::Identifier),
            (" ", Kind::Whitespace),
            ("=", Kind::EqualSign),
            (" ", Kind::Whitespace),
            ("value0", Kind::Identifier),
            (";", Kind::SemiColon),
        ],
        check_whitespace: true,
    }

    lexer_test_case! {
        name: let_statement_with_function_definition,
        input: "let add = func (lhs, rhs) { return lhs + rhs; };",
        expected_tokens: &[
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
    }

    lexer_test_case! {
        name: greater_than,
        input: "a > b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            (">", Kind::Greater),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: less_than,
        input: "a < b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            ("<", Kind::Less),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: greater_than_or_equal,
        input: "a >= b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            (">=", Kind::GreaterOrEqual),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: less_than_or_equal,
        input: "a <= b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            ("<=", Kind::LessOrEqual),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: equal_to,
        input: "a == b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            ("==", Kind::DoubleEquals),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: not_equal_to,
        input: "a != b;",
        expected_tokens: &[
            ("a", Kind::Identifier),
            ("!=", Kind::NotEquals),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: unary_not,
        input: "let a = !b;",
        expected_tokens: &[
            ("let", Kind::Let),
            ("a", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("!", Kind::Not),
            ("b", Kind::Identifier),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: star,
        input: "let x = 3 * 4;",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("3", Kind::Integer),
            ("*", Kind::Star),
            ("4", Kind::Integer),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: divide,
        input: "let x = 4 / 2;",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("4", Kind::Integer),
            ("/", Kind::Divide),
            ("2", Kind::Integer),
            (";", Kind::SemiColon),
            ],
    }

    lexer_test_case! {
        name: digit,
        input: "3",
        expected_tokens: &[("3", Kind::Integer)],
    }

    lexer_test_case! {
        name: integer,
        input: "314",
        expected_tokens: &[("314", Kind::Integer)],
    }

    lexer_test_case! {
        name: float,
        input: "3.14",
        expected_tokens: &[("3.14", Kind::Float)],
    }

    lexer_test_case! {
        name: boolean_true,
        input: "True",
        expected_tokens: &[("True", Kind::True)],
    }

    lexer_test_case! {
        name: boolean_false,
        input: "False",
        expected_tokens: &[("False", Kind::False)],
    }

    lexer_test_case! {
        name: float_with_trailing_dot,
        input: "3.",
        expected_tokens: &[("3.", Kind::Float)],
    }

    lexer_test_case! {
        name: float_with_leading_dot,
        input: ".14",
        expected_tokens: &[(".14", Kind::Float)],
    }

    lexer_test_case! {
        name: float_with_exponent,
        input: "3e8",
        expected_tokens: &[("3e8", Kind::Float)],
    }

    lexer_test_case! {
        name: float_with_exponent_and_dot,
        input: "0.314e1",
        expected_tokens: &[("0.314e1", Kind::Float)],
    }

    lexer_test_case! {
        name: float_with_negative_exponent_and_dot,
        input: "9.1e-31",
        expected_tokens: &[("9.1e-31", Kind::Float)],
    }

    lexer_test_case! {
        name: float_with_positive_exponent_and_dot,
        input: "6.02e+23",
        expected_tokens: &[("6.02e+23", Kind::Float)],
    }

    lexer_test_case! {
        name: bad_float_with_exponent_and_no_following_number,
        input: "2e",
        expected_tokens: &[("2e", Kind::Unknown)],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_exponent_and_no_following_number,
        input: "2.e",
        expected_tokens: &[("2.e", Kind::Unknown)],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_f_exponent_and_no_following_number,
        input: "2.4f",
        expected_tokens: &[("2.4f", Kind::Unknown)],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_exponent_and_trailing_letter,
        input: "2.4e3a",
        expected_tokens: &[("2.4e3a", Kind::Unknown)],
    }

    lexer_test_case! {
        name: bad_float_with_confused_exponent,
        input: "123f+4.2e-3",
        expected_tokens: &[
            ("123f", Kind::Unknown),
            ("+", Kind::Plus),
            ("4.2e-3", Kind::Float),
        ],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_exponent_with_dot,
        input: "1.23e-4.56",
        expected_tokens: &[("1.23e-4.56", Kind::Unknown)],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_exponent_with_float,
        input: "1.23e-4+3.2e-5",
        expected_tokens: &[
            ("1.23e-4", Kind::Float),
            ("+", Kind::Plus),
            ("3.2e-5", Kind::Float),
        ],
    }

    lexer_test_case! {
        name: bad_float_with_dot_and_exponent_with_negative_float,
        input: "1.23e-4e-3.2",
        expected_tokens: &[("1.23e-4e-3.2", Kind::Unknown)],
    }

    lexer_test_case! {
        name: let_with_dot_and_parens,
        input: "let x = self.x();",
        expected_tokens: &[
            ("let", Kind::Let),
            ("x", Kind::Identifier),
            ("=", Kind::EqualSign),
            ("self", Kind::Identifier),
            (".", Kind::Period),
            ("x", Kind::Identifier),
            ("(", Kind::LeftParen),
            (")", Kind::RightParen),
            (";", Kind::SemiColon),
        ],
    }

    lexer_test_case! {
    name: braces_and_colon,
    input: "{a: 3}",
    expected_tokens: &[
        ("{", Kind::LeftBrace),
        ("a", Kind::Identifier),
        (":", Kind::Colon),
        ("3", Kind::Integer),
        ("}", Kind::RightBrace),
        ],
    }

    lexer_test_case! {
        name: square_bracket_and_comma,
        input: "[1, 2, 3]",
        expected_tokens: &[
            ("[", Kind::LeftSqBracket),
            ("1", Kind::Integer),
            (",", Kind::Comma),
            ("2", Kind::Integer),
            (",", Kind::Comma),
            ("3", Kind::Integer),
            ("]", Kind::RightSqBracket),
            ],
    }

    lexer_test_case! {
        name: at_sigil,
        input: "@123",
        expected_tokens: &[("@123", Kind::Unknown)],
    }

    lexer_test_case! {
        name: line_comment,
        input: "# comment",
        expected_tokens: &[("# comment", Kind::Comment)],
    }

    lexer_test_case! {
        name: end_of_line_comment,
        input: "a; # comment",
        expected_tokens: &[
            ("a", Kind::Identifier),
            (";", Kind::SemiColon),
            ("# comment", Kind::Comment),
            ],
    }
}

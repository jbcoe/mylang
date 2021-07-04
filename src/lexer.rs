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
                break;
            }
        }
        tokens
    }

    pub fn next_token(&mut self) -> Token<'a> {
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
        match token_text {
            Ok("False") => Some(self.text_token(start, Kind::False)),
            Ok("func") => Some(self.text_token(start, Kind::Function)),
            Ok("let") => Some(self.text_token(start, Kind::Let)),
            Ok("mut") => Some(self.text_token(start, Kind::Mut)),
            Ok("return") => Some(self.text_token(start, Kind::Return)),
            Ok("True") => Some(self.text_token(start, Kind::True)),
            _ => {
                self.reset(start);
                None
            }
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
            self.text_token(start, Kind::FloatingPoint)
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
        ( $test_name:ident, $test_case:expr ) => {
            #[test]
            fn $test_name() {
                let mut lexer = Lexer::new($test_case.input);
                for expected_token in &$test_case.expected_tokens {
                    let mut t = lexer.next_token();
                    while t.kind() == Kind::Whitespace && $test_case.skip_whitespace {
                        t = lexer.next_token();
                    }
                    assert_eq!(t.text(), expected_token.0);
                    assert_eq!(t.kind(), expected_token.1);
                }
                assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
            }
        };
    }

    lexer_test_case![
        let_statement_with_string,
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
        }
    ];

    lexer_test_case![
        let_statement_with_bad_identifier,
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
        }
    ];

    lexer_test_case![
        let_statement_with_unfinished_string,
        TestCase {
            input: r#"let mut myPet = "string with unbalanced quotes"#,
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
                (r#""string with unbalanced quotes"#, Kind::Unknown),
            ],
        }
    ];

    lexer_test_case![
        let_statement_with_integer,
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
        }
    ];

    lexer_test_case![
        let_statement_with_identifier,
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
        }
    ];

    lexer_test_case![
        let_statement_with_identifier_containing_number,
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
        }
    ];

    lexer_test_case![
        let_statement_with_function_definition,
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
        }
    ];

    lexer_test_case![
        greater_than,
        TestCase {
            input: "a > b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                (">", Kind::Greater),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        less_than,
        TestCase {
            input: "a < b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                ("<", Kind::Less),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        greater_than_or_equal,
        TestCase {
            input: "a >= b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                (">=", Kind::GreaterOrEqual),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        less_than_or_equal,
        TestCase {
            input: "a <= b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                ("<=", Kind::LessOrEqual),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        equal_to,
        TestCase {
            input: "a == b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                ("==", Kind::DoubleEquals),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        not_equal_to,
        TestCase {
            input: "a != b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                ("!=", Kind::NotEquals),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        unary_not,
        TestCase {
            input: "let a = !b;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("let", Kind::Let),
                ("a", Kind::Identifier),
                ("=", Kind::EqualSign),
                ("!", Kind::Not),
                ("b", Kind::Identifier),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        star,
        TestCase {
            input: "let x = 3 * 4;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("let", Kind::Let),
                ("x", Kind::Identifier),
                ("=", Kind::EqualSign),
                ("3", Kind::Integer),
                ("*", Kind::Star),
                ("4", Kind::Integer),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        divide,
        TestCase {
            input: "let x = 4 / 2;",
            skip_whitespace: true,
            expected_tokens: vec![
                ("let", Kind::Let),
                ("x", Kind::Identifier),
                ("=", Kind::EqualSign),
                ("4", Kind::Integer),
                ("/", Kind::Divide),
                ("2", Kind::Integer),
                (";", Kind::SemiColon),
            ],
        }
    ];

    lexer_test_case![
        digit,
        TestCase {
            input: "3",
            skip_whitespace: false,
            expected_tokens: vec![("3", Kind::Integer)],
        }
    ];

    lexer_test_case![
        integer,
        TestCase {
            input: "314",
            skip_whitespace: false,
            expected_tokens: vec![("314", Kind::Integer)],
        }
    ];

    lexer_test_case![
        float,
        TestCase {
            input: "3.14",
            skip_whitespace: false,
            expected_tokens: vec![("3.14", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        boolean_true,
        TestCase {
            input: "True",
            skip_whitespace: false,
            expected_tokens: vec![("True", Kind::True)],
        }
    ];

    lexer_test_case![
        boolean_false,
        TestCase {
            input: "False",
            skip_whitespace: false,
            expected_tokens: vec![("False", Kind::False)],
        }
    ];

    lexer_test_case![
        float_with_trailing_dot,
        TestCase {
            input: "3.",
            skip_whitespace: false,
            expected_tokens: vec![("3.", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        float_with_leading_dot,
        TestCase {
            input: ".14",
            skip_whitespace: false,
            expected_tokens: vec![(".14", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        float_with_exponent,
        TestCase {
            input: "3e8",
            skip_whitespace: false,
            expected_tokens: vec![("3e8", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        float_with_exponent_and_dot,
        TestCase {
            input: "0.314e1",
            skip_whitespace: false,
            expected_tokens: vec![("0.314e1", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        float_with_negative_exponent_and_dot,
        TestCase {
            input: "9.1e-31",
            skip_whitespace: false,
            expected_tokens: vec![("9.1e-31", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        float_with_positive_exponent_and_dot,
        TestCase {
            input: "6.02e+23",
            skip_whitespace: false,
            expected_tokens: vec![("6.02e+23", Kind::FloatingPoint)],
        }
    ];

    lexer_test_case![
        bad_float_with_exponent_and_no_following_number,
        TestCase {
            input: "2e",
            skip_whitespace: false,
            expected_tokens: vec![("2e", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_exponent_and_no_following_number,
        TestCase {
            input: "2.e",
            skip_whitespace: false,
            expected_tokens: vec![("2.e", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_f_exponent_and_no_following_number,
        TestCase {
            input: "2.4f",
            skip_whitespace: false,
            expected_tokens: vec![("2.4f", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_exponent_and_trailing_letter,
        TestCase {
            input: "2.4e3a",
            skip_whitespace: false,
            expected_tokens: vec![("2.4e3a", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        bad_float_with_confused_exponent,
        TestCase {
            input: "123f+4.2e-3",
            skip_whitespace: false,
            expected_tokens: vec![
                ("123f", Kind::Unknown),
                ("+", Kind::Plus),
                ("4.2e-3", Kind::FloatingPoint),
            ],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_exponent_with_dot,
        TestCase {
            input: "1.23e-4.56",
            skip_whitespace: false,
            expected_tokens: vec![("1.23e-4.56", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_exponent_with_float,
        TestCase {
            input: "1.23e-4+3.2e-5",
            skip_whitespace: false,
            expected_tokens: vec![
                ("1.23e-4", Kind::FloatingPoint),
                ("+", Kind::Plus),
                ("3.2e-5", Kind::FloatingPoint),
            ],
        }
    ];

    lexer_test_case![
        bad_float_with_dot_and_exponent_with_negative_float,
        TestCase {
            input: "1.23e-4e-3.2",
            skip_whitespace: false,
            expected_tokens: vec![("1.23e-4e-3.2", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        let_with_dot_and_parens,
        TestCase {
            input: "let x = self.x();",
            skip_whitespace: true,
            expected_tokens: vec![
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
    ];

    lexer_test_case![
        braces_and_colon,
        TestCase {
            input: "{a: 3}",
            skip_whitespace: true,
            expected_tokens: vec![
                ("{", Kind::LeftBrace),
                ("a", Kind::Identifier),
                (":", Kind::Colon),
                ("3", Kind::Integer),
                ("}", Kind::RightBrace),
            ],
        }
    ];

    lexer_test_case![
        square_bracket_and_comma,
        TestCase {
            input: "[1, 2, 3]",
            skip_whitespace: true,
            expected_tokens: vec![
                ("[", Kind::LeftSqBracket),
                ("1", Kind::Integer),
                (",", Kind::Comma),
                ("2", Kind::Integer),
                (",", Kind::Comma),
                ("3", Kind::Integer),
                ("]", Kind::RightSqBracket),
            ],
        }
    ];

    lexer_test_case![
        at_sigil,
        TestCase {
            input: "@123",
            skip_whitespace: true,
            expected_tokens: vec![("@123", Kind::Unknown)],
        }
    ];

    lexer_test_case![
        line_comment,
        TestCase {
            input: "# comment",
            skip_whitespace: true,
            expected_tokens: vec![("# comment", Kind::Comment)],
        }
    ];

    lexer_test_case![
        end_of_line_comment,
        TestCase {
            input: "a; # comment",
            skip_whitespace: true,
            expected_tokens: vec![
                ("a", Kind::Identifier),
                (";", Kind::SemiColon),
                ("# comment", Kind::Comment),
            ],
        }
    ];
}

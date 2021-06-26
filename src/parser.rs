use crate::token::{Kind, Token};

#[derive(Debug)]
pub struct StringLiteralExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct IntegerExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct FloatingPointExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Debug)]
pub enum UnaryPlusExpression {
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
}
#[derive(Debug)]
pub enum UnaryMinusExpression {
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
}

#[derive(Debug)]
pub enum ExpressionStatement {
    StringLiteral(StringLiteralExpression),
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
    Identifier(IdentifierExpression),
    UnaryPlus(UnaryPlusExpression),
    UnaryMinus(UnaryMinusExpression),
}
#[derive(Debug)]
pub struct LetStatement {
    pub mutable: bool,
    pub identifier: String,
    pub expression: Box<ExpressionStatement>,
}
#[derive(Debug)]
pub enum Statement {
    //    Expression(ExpressionStatement),
    Let(LetStatement),
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    pub statements: Vec<Statement>,
    errors: Vec<String>,
}

impl AbstractSyntaxTree {
    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }
}
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    position: usize,
    read_position: usize,
    token: Token<'a>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    /// Create a new Parser from a Vec<Token>, probably from a Lexer
    #[must_use]
    pub fn new(input: Vec<Token>) -> Parser {
        let mut tokens = vec![];
        for token in input {
            if token.kind() != Kind::Whitespace {
                tokens.push(token);
            }
        }
        Parser {
            tokens,
            position: 0,
            read_position: 0,
            token: Token::eof(0),
            errors: vec![],
        }
    }

    // Consumes the parser.
    #[must_use]
    pub fn ast(mut self) -> AbstractSyntaxTree {
        let mut statements = vec![];
        while let Some(stmt) = self.parse_next() {
            statements.push(stmt);
        }
        AbstractSyntaxTree {
            statements,
            errors: self.errors,
        }
    }

    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        if self.position >= self.tokens.len() {
            self.token = Token::eof(self.position);
        } else {
            self.token = self.tokens[self.position];
        }
    }

    fn read_token(&mut self) {
        if self.read_position >= self.tokens.len() {
            self.token = Token::eof(self.read_position);
            return;
        }
        self.token = self.tokens[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_token(&self) -> Token<'a> {
        if self.read_position >= self.tokens.len() {
            return Token::eof(self.read_position);
        }
        self.tokens[self.read_position]
    }

    // Matches:
    //   "let identifier = expression;"
    //   "let mut identifier = expression;"
    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let start = self.position;
        self.read_token(); // consume let

        let mut mutable = false;
        if self.peek_token().kind() == Kind::Mut {
            mutable = true;
            self.read_token(); // consume mut
        }

        if self.token.kind() != Kind::Identifier {
            self.errors
                .push(format!("expected identifier, got {:?}", self.token));
            self.reset(start);
            return None;
        }
        let identifier = self.token.text();
        self.read_token(); // consume identifier

        if self.token.kind() != Kind::EqualSign {
            self.errors
                .push(format!("expected equal sign, got {:?}", self.token));
            self.reset(start);
            return None;
        }
        self.read_token(); // consume equals sign

        match self.parse_expression() {
            None => {
                self.errors
                    .push(format!("expected expression, got {:?}", self.token));
                self.reset(start);
                None
            }
            Some(expression) => Some(LetStatement {
                mutable,
                identifier,
                expression: Box::new(expression),
            }),
        }
    }

    fn parse_expression(&mut self) -> Option<ExpressionStatement> {
        match self.token.kind() {
            Kind::Identifier => {
                if let Some(identifier) = self.parse_identifier_expression() {
                    return Some(ExpressionStatement::Identifier(identifier));
                }
                None
            }
            Kind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(ExpressionStatement::Integer(integer));
                }
                None
            }
            Kind::FloatingPoint => {
                if let Some(floating_point) = self.parse_floating_point_expression() {
                    return Some(ExpressionStatement::FloatingPoint(floating_point));
                }
                None
            }
            Kind::String => {
                if let Some(string) = self.parse_string_literal_expression() {
                    return Some(ExpressionStatement::StringLiteral(string));
                }
                None
            }
            Kind::Plus => {
                if let Some(expr) = self.parse_unary_plus_expression() {
                    return Some(ExpressionStatement::UnaryPlus(expr));
                }
                None
            }
            Kind::Minus => {
                if let Some(expr) = self.parse_unary_minus_expression() {
                    return Some(ExpressionStatement::UnaryMinus(expr));
                }
                None
            }
            _ => {
                self.errors.push(format!(
                    "Parse error when parsing expression {:?}",
                    self.token
                ));
                None
            }
        }
    }

    // Matches: "name;"
    fn parse_identifier_expression(&mut self) -> Option<IdentifierExpression> {
        assert!(self.token.kind() == Kind::Identifier);
        match self.token.kind() {
            Kind::Identifier => {
                if self.peek_token().kind() != Kind::SemiColon {
                    return None;
                }
                let name = self.token.text();
                self.read_token(); // consume `name`
                self.read_token(); // consume `;`
                Some(IdentifierExpression { name })
            }
            _ => None,
        }
    }

    fn parse_unary_plus_expression(&mut self) -> Option<UnaryPlusExpression> {
        assert!(self.token.kind() == Kind::Plus);
        self.read_token(); // consume `+`
        match self.token.kind() {
            Kind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(UnaryPlusExpression::Integer(integer));
                }
                None
            }
            Kind::FloatingPoint => {
                if let Some(floating_point) = self.parse_floating_point_expression() {
                    return Some(UnaryPlusExpression::FloatingPoint(floating_point));
                }
                None
            }
            _ => {
                self.errors.push(format!(
                    "Parse error when parsing unary plus expression {:?}",
                    self.token
                ));
                None
            }
        }
    }

    fn parse_unary_minus_expression(&mut self) -> Option<UnaryMinusExpression> {
        assert!(self.token.kind() == Kind::Minus);
        self.read_token(); // consume `-`
        match self.token.kind() {
            Kind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(UnaryMinusExpression::Integer(integer));
                }
                None
            }
            Kind::FloatingPoint => {
                if let Some(floating_point) = self.parse_floating_point_expression() {
                    return Some(UnaryMinusExpression::FloatingPoint(floating_point));
                }
                None
            }
            _ => {
                self.errors.push(format!(
                    "Parse error when parsing unary minus expression {:?}",
                    self.token
                ));
                None
            }
        }
    }

    fn parse_integer_expression(&mut self) -> Option<IntegerExpression> {
        assert!(self.token.kind() == Kind::Integer);
        match self.token.kind() {
            Kind::Integer => {
                if self.peek_token().kind() != Kind::SemiColon {
                    return None;
                }
                let value = self.token.text();
                self.read_token(); // consume `value`
                self.read_token(); // consume `;`
                Some(IntegerExpression { value })
            }
            _ => None,
        }
    }

    fn parse_floating_point_expression(&mut self) -> Option<FloatingPointExpression> {
        assert!(self.token.kind() == Kind::FloatingPoint);
        match self.token.kind() {
            Kind::FloatingPoint => {
                if self.peek_token().kind() != Kind::SemiColon {
                    return None;
                }
                let value = self.token.text();
                self.read_token(); // consume `value`
                self.read_token(); // consume `;`
                Some(FloatingPointExpression { value })
            }
            _ => None,
        }
    }

    fn parse_string_literal_expression(&mut self) -> Option<StringLiteralExpression> {
        assert!(self.token.kind() == Kind::String);
        match self.token.kind() {
            Kind::String => {
                if self.peek_token().kind() != Kind::SemiColon {
                    return None;
                }
                let value = self.token.text();
                self.read_token(); // consume `value`
                self.read_token(); // consume `;`
                Some(StringLiteralExpression { value })
            }
            _ => None,
        }
    }

    fn parse_next(&mut self) -> Option<Statement> {
        self.read_token();
        match self.token.kind() {
            Kind::Let => {
                if let Some(stmt) = self.parse_let_statement() {
                    return Some(Statement::Let(stmt));
                }
                self.errors.push(format!(
                    "Parse error when parsing let-statement {:?}",
                    self.token
                ));
                None
            }
            Kind::EndOfFile => None,
            _ => {
                self.errors.push(format!(
                    "Parse error: unexpected TokenKind when parsing statement {:?}",
                    self.token.kind()
                ));
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
        expected_errors: Vec<&'static str>,
    }

    #[test]
    fn parser() {
        let test_cases = vec![
            // Success cases
            TestCase {
                input: "let x = a;",
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = 5;",
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = 3.14159;",
                expected_errors: vec![],
            },
            TestCase {
                input: r#"let x = "Hello";"#,
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = +1;",
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = -1;",
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = +3.14159;",
                expected_errors: vec![],
            },
            TestCase {
                input: "let x = -3.14159;",
                expected_errors: vec![],
            },
            // Error cases
            TestCase {
                input: "let 123 = x;",
                expected_errors: vec![
                    r#"expected identifier, got Token { text: "123", kind: Integer }"#,
                    r#"Parse error when parsing let-statement Token { text: "let", kind: Let }"#,
                ],
            },
        ];

        for test_case in test_cases.iter() {
            let tokens = Lexer::new(test_case.input).tokens();
            let parser = Parser::new(tokens);
            let ast = parser.ast();
            let errors = ast.errors();

            for (expected, actual) in test_case.expected_errors.iter().zip(errors.iter()) {
                assert_eq!(
                    expected, actual,
                    "Parse error mismatch while parsing {}",
                    test_case.input
                );
            }

            if test_case.expected_errors.len() > errors.len() {
                for expected in &test_case.expected_errors[errors.len()..] {
                    assert_eq!(
                        *expected, "",
                        "Expected parse error not encountered while parsing {}",
                        test_case.input
                    );
                }
            } else if errors.len() > test_case.expected_errors.len() {
                for error in &errors[test_case.expected_errors.len()..] {
                    assert_eq!(
                        error, "",
                        "Unexpected parse error encountered while parsing {}",
                        test_case.input
                    );
                }
            }
        }
    }
}

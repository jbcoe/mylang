use crate::token::{Kind, Token};

#[derive(Debug)]
pub struct StringLiteralExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct IntegerExpression {
    pub value: i32,
}
#[derive(Debug)]
pub struct FloatingPointExpression {
    pub value: f64,
}
#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Debug)]
pub enum UnaryPlusExpression {
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
    Identifier(IdentifierExpression),
}
#[derive(Debug)]
pub enum UnaryMinusExpression {
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
    Identifier(IdentifierExpression),
}

#[derive(Debug)]
pub struct FunctionExpression {
    identifiers: Vec<String>,
    body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Expression {
    StringLiteral(StringLiteralExpression),
    Integer(IntegerExpression),
    FloatingPoint(FloatingPointExpression),
    Identifier(IdentifierExpression),
    UnaryPlus(UnaryPlusExpression),
    UnaryMinus(UnaryMinusExpression),
    Function(FunctionExpression),
}
#[derive(Debug)]
pub struct LetStatement {
    pub mutable: bool,
    pub identifier: String,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    // Expression(Expression),
    Let(LetStatement),
    Return(ReturnStatement),
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    statements: Vec<Statement>,
    errors: Vec<String>,
}

impl AbstractSyntaxTree {
    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }
    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
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
            token: Token::end_of_file(0),
            errors: vec![],
        }
    }

    // Consumes the parser.
    #[must_use]
    pub fn ast(mut self) -> AbstractSyntaxTree {
        let mut statements = vec![];
        self.read_token();
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
            self.token = Token::end_of_file(self.position);
        } else {
            self.token = self.tokens[self.position];
        }
    }

    fn read_token(&mut self) {
        if self.read_position >= self.tokens.len() {
            self.token = Token::end_of_file(self.read_position);
        } else {
            self.token = self.tokens[self.read_position];
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        assert!(self.token.kind() == Kind::Return);
        let start = self.position;
        self.read_token(); // consume return
        match self.parse_expression() {
            None => {
                self.errors
                    .push(format!("expected expression, got {:?}", self.token));
                self.reset(start);
                None
            }
            Some(expression) => {
                if self.token.kind() != Kind::SemiColon {
                    self.errors
                        .push(format!("expected ';', got {:?}", self.token));
                    self.reset(start);
                    return None;
                }
                Some(ReturnStatement {
                    expression: Box::new(expression),
                })
            }
        }
    }

    // Matches:
    //   "let identifier = expression;"
    //   "let mut identifier = expression;"
    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        assert!(self.token.kind() == Kind::Let);
        let start = self.position;
        self.read_token(); // consume let

        let mut mutable = false;
        if self.token.kind() == Kind::Mut {
            mutable = true;
            self.read_token(); // consume mut
        }

        if self.token.kind() == Kind::Identifier {
            let identifier = self.token.text();
            self.read_token(); // consume identifier

            if self.token.kind() == Kind::EqualSign {
                self.read_token(); // consume equals sign

                match self.parse_expression() {
                    None => {
                        self.errors
                            .push(format!("expected expression, got {:?}", self.token));
                        self.reset(start);
                        None
                    }
                    Some(expression) => {
                        if self.token.kind() != Kind::SemiColon {
                            self.errors
                                .push(format!("expected ';', got {:?}", self.token));
                            self.reset(start);
                            return None;
                        }
                        Some(LetStatement {
                            mutable,
                            identifier,
                            expression: Box::new(expression),
                        })
                    }
                }
            } else {
                self.errors
                    .push(format!("expected equal sign, got {:?}", self.token));
                self.reset(start);
                None
            }
        } else {
            self.errors
                .push(format!("expected identifier, got {:?}", self.token));
            self.reset(start);
            None
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        match self.token.kind() {
            Kind::Identifier => self
                .parse_identifier_expression()
                .map(Expression::Identifier),
            Kind::Integer => self.parse_integer_expression().map(Expression::Integer),
            Kind::FloatingPoint => self
                .parse_floating_point_expression()
                .map(Expression::FloatingPoint),
            Kind::String => self
                .parse_string_literal_expression()
                .map(Expression::StringLiteral),
            Kind::Function => self.parse_function_expression().map(Expression::Function),
            Kind::Plus => self
                .parse_unary_plus_expression()
                .map(Expression::UnaryPlus),
            Kind::Minus => self
                .parse_unary_minus_expression()
                .map(Expression::UnaryMinus),
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
                let name = self.token.text();
                self.read_token(); // consume `name`
                Some(IdentifierExpression { name })
            }
            _ => None,
        }
    }

    fn parse_unary_plus_expression(&mut self) -> Option<UnaryPlusExpression> {
        assert!(self.token.kind() == Kind::Plus);
        self.read_token(); // consume `+`
        match self.token.kind() {
            Kind::Integer => self
                .parse_integer_expression()
                .map(UnaryPlusExpression::Integer),
            Kind::FloatingPoint => self
                .parse_floating_point_expression()
                .map(UnaryPlusExpression::FloatingPoint),
            Kind::Identifier => self
                .parse_identifier_expression()
                .map(UnaryPlusExpression::Identifier),
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
            Kind::Integer => self
                .parse_integer_expression()
                .map(UnaryMinusExpression::Integer),
            Kind::FloatingPoint => self
                .parse_floating_point_expression()
                .map(UnaryMinusExpression::FloatingPoint),
            Kind::Identifier => self
                .parse_identifier_expression()
                .map(UnaryMinusExpression::Identifier),
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
                if let Ok(value) = self.token.text().parse::<i32>() {
                    self.read_token(); // consume `value`
                    Some(IntegerExpression { value })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_floating_point_expression(&mut self) -> Option<FloatingPointExpression> {
        assert!(self.token.kind() == Kind::FloatingPoint);
        match self.token.kind() {
            Kind::FloatingPoint => {
                if let Ok(value) = self.token.text().parse::<f64>() {
                    self.read_token(); // consume `value`
                    Some(FloatingPointExpression { value })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_string_literal_expression(&mut self) -> Option<StringLiteralExpression> {
        assert!(self.token.kind() == Kind::String);
        match self.token.kind() {
            Kind::String => {
                let value = self.token.text();
                self.read_token(); // consume `value`
                Some(StringLiteralExpression { value })
            }
            _ => None,
        }
    }

    // Matches:
    //   "func ( identifier* ) {
    //     statement*
    //   };"
    // Indenting is not checked.
    fn parse_function_expression(&mut self) -> Option<FunctionExpression> {
        assert!(self.token.kind() == Kind::Function);
        let start = self.position;
        self.read_token(); // consume "fn"

        if self.token.kind() != Kind::LeftParen {
            self.errors
                .push(format!("expected '(', got {:?}", self.token));
            self.reset(start);
            return None;
        }
        self.read_token(); // consume "("

        let mut identifiers = vec![];
        loop {
            match self.token.kind() {
                Kind::Identifier => {
                    identifiers.push(self.token.text());
                    self.read_token();
                }
                Kind::Comma => self.read_token(),
                Kind::RightParen => {
                    break;
                }
                _ => {
                    self.errors
                        .push(format!("expected ',' or identifier, got {:?}", self.token));
                    self.reset(start);
                    return None;
                }
            }
        }

        assert!(self.token.kind() == Kind::RightParen);
        self.read_token(); // consume ')'

        if self.token.kind() != Kind::LeftBrace {
            self.errors
                .push(format!("expected '{{', got {:?}", self.token));
            self.reset(start);
            return None;
        }
        self.read_token(); // consume "{"

        let mut statements = vec![];
        loop {
            match self.token.kind() {
                Kind::RightBrace => break,
                _ => {
                    if let Some(s) = self.parse_statement() {
                        statements.push(s);
                    } else {
                        self.errors.push(format!(
                            "expected function-body statement, got {:?}",
                            self.token
                        ));
                        self.reset(start);
                        return None;
                    }
                }
            }
        }
        assert_eq!(self.token.kind(), Kind::RightBrace);
        self.read_token(); // consume "}"

        Some(FunctionExpression {
            identifiers,
            body: statements,
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let statement;

        match self.token.kind() {
            Kind::Let => {
                if let Some(stmt) = self.parse_let_statement() {
                    statement = Statement::Let(stmt);
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing let-statement {:?}",
                        self.token
                    ));
                    return None;
                }
            }
            Kind::Return => {
                if let Some(stmt) = self.parse_return_statement() {
                    statement = Statement::Return(stmt);
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing return-statement {:?}",
                        self.token
                    ));
                    return None;
                }
            }
            _ => {
                self.errors.push(format!(
                    "Parse error: unexpected token kind when parsing statement {:?}",
                    self.token.kind()
                ));
                return None;
            }
        }
        self.read_token(); // consume ';'
        Some(statement)
    }

    fn parse_next(&mut self) -> Option<Statement> {
        match self.token.kind() {
            Kind::EndOfFile => None,
            _ => self.parse_statement(),
        }
    }
}

#[cfg(test)]
mod tests {

    use std::cmp::Ordering;

    use super::*;
    use crate::lexer::Lexer;

    #[derive(Debug)]
    struct ParserErrorTestCase {
        input: &'static str,
        expected_errors: Vec<&'static str>,
    }

    #[test]
    fn check_errors() {
        let test_cases = vec![
            // Success cases
            ParserErrorTestCase {
                input: "let x = a;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = 5;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = 3.14159;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: r#"let x = "Hello";"#,
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = +1;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = -1;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = +3.14159;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = -3.14159;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = -a;",
                expected_errors: vec![],
            },
            ParserErrorTestCase {
                input: "let x = +a;",
                expected_errors: vec![],
            },
            // Error cases
            ParserErrorTestCase {
                input: "let 123 = x;",
                expected_errors: vec![
                    r#"expected identifier, got Token { text: "123", kind: Integer }"#,
                    r#"Parse error when parsing let-statement Token { text: "let", kind: Let }"#,
                ],
            },
        ];

        for test_case in &test_cases {
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

            match test_case.expected_errors.len().cmp(&errors.len()) {
                Ordering::Greater => {
                    for expected in &test_case.expected_errors[errors.len()..] {
                        assert_eq!(
                            *expected, "",
                            "Expected parse error not encountered while parsing {}",
                            test_case.input
                        );
                    }
                }
                Ordering::Less => {
                    for error in &errors[test_case.expected_errors.len()..] {
                        assert_eq!(
                            error, "",
                            "Unexpected parse error encountered while parsing {}",
                            test_case.input
                        );
                    }
                }
                Ordering::Equal => {}
            }
        }
    }

    #[derive(Debug)]
    struct ParseLetStatementTest {
        input: &'static str,
        identifier: &'static str,
        mutable: bool,
    }

    #[test]
    fn test_parse_let_statement() {
        let test_cases = vec![
            ParseLetStatementTest {
                input: "let x = a;",
                identifier: "x",
                mutable: false,
            },
            ParseLetStatementTest {
                input: "let mut minus_pi = -3.14159;",
                identifier: "minus_pi",
                mutable: true,
            },
            ParseLetStatementTest {
                input: r#"let x = "Hello";"#,
                identifier: "x",
                mutable: false,
            },
            ParseLetStatementTest {
                input: "let first = func (a, b) { return a; };",
                identifier: "first",
                mutable: false,
            },
        ];

        for test_case in &test_cases {
            let tokens = Lexer::new(test_case.input).tokens();
            let parser = Parser::new(tokens);
            let ast = parser.ast();

            assert!(ast.errors().is_empty());
            assert!(ast.statements.len() == 1);
            match &ast.statements[0] {
                Statement::Let(let_statement) => {
                    assert_eq!(let_statement.identifier, test_case.identifier);
                    assert_eq!(let_statement.mutable, test_case.mutable);
                    // TODO: Check some property of the expression.
                }
                Statement::Return(_) => panic!("Expected a let statement"),
            }
        }
    }

    struct ParseReturnStatementTest {
        input: &'static str,
    }

    #[test]
    fn parse_return_statement_test() {
        let test_cases = vec![
            ParseReturnStatementTest {
                input: "return 42;",
            },
            ParseReturnStatementTest {
                input: r#"return "the solution";"#,
            },
        ];

        for test_case in &test_cases {
            let tokens = Lexer::new(test_case.input).tokens();
            let parser = Parser::new(tokens);
            let ast = parser.ast();
            let errors = ast.errors();

            assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
            assert_eq!(ast.statements.len(), 1);
            match &ast.statements[0] {
                Statement::Return(_) => {
                    // TODO: Check some property of the expression.
                }
                Statement::Let(_) => panic!("Expected a return statement."),
            }
        }
    }
}

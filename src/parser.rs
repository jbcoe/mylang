use std::rc::Rc;

use crate::{
    ast::{
        AbstractSyntaxTree, BinaryOp, Call, Expression, Function, Let, OpName, Statement, UnaryOp,
    },
    token::{Kind, Token},
};
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
    pub(crate) fn new(input: Vec<Token>) -> Parser {
        let mut tokens = vec![];
        for token in input {
            match token.kind() {
                // Drop whitespace and comments as they are only for humans.
                Kind::Comment | Kind::Whitespace => (),
                _ => tokens.push(token),
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
    pub(crate) fn ast(mut self) -> AbstractSyntaxTree {
        let mut statements = vec![];
        self.read_token();
        while let Some(stmt) = self.parse_next() {
            statements.push(stmt);
        }
        AbstractSyntaxTree::new(statements, self.errors)
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

    fn parse_return(&mut self) -> Option<Statement> {
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
                if self.token.kind() == Kind::SemiColon {
                    Some(Statement::Return(Box::new(expression)))
                } else {
                    self.errors
                        .push(format!("expected ';', got {:?}", self.token));
                    self.reset(start);
                    None
                }
            }
        }
    }

    // Matches:
    //   "let identifier = expression;"
    //   "let mut identifier = expression;"
    fn parse_let(&mut self) -> Option<Let> {
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
                        if self.token.kind() == Kind::SemiColon {
                            Some(Let {
                                mutable,
                                identifier,
                                expression: Box::new(expression),
                            })
                        } else {
                            self.errors
                                .push(format!("expected ';', got {:?}", self.token));
                            self.reset(start);
                            None
                        }
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
        let start = self.position;

        if let Some(subexpression) = self.parse_subexpression() {
            let boxed = Box::new(subexpression);
            match self.token.kind() {
                Kind::Plus => self.binary_parse(start, boxed, OpName::Plus),
                Kind::Minus => self.binary_parse(start, boxed, OpName::Minus),
                Kind::Divide => self.binary_parse(start, boxed, OpName::Divide),
                Kind::Star => self.binary_parse(start, boxed, OpName::Multiply),
                Kind::SemiColon
                | Kind::Comma
                | Kind::LeftParen
                | Kind::RightParen
                | Kind::LeftBrace
                | Kind::RightBrace => Some(*boxed),
                _ => {
                    self.reset(start);
                    None
                }
            }
        } else {
            self.reset(start);
            None
        }
    }

    fn binary_parse(
        &mut self,
        start: usize,
        left: Box<Expression>,
        operation: OpName,
    ) -> Option<Expression> {
        self.read_token();
        self.parse_expression().map_or_else(
            || {
                self.reset(start);
                None
            },
            |rhs| {
                Some(Expression::BinaryOp(BinaryOp {
                    left,
                    right: Box::new(rhs),
                    operation,
                }))
            },
        )
    }

    fn parse_subexpression(&mut self) -> Option<Expression> {
        match self.token.kind() {
            Kind::Identifier => match self.parse_call() {
                Some(function_call) => Some(Expression::Call(function_call)),
                None => Some(Expression::Identifier(self.parse_string())),
            },
            Kind::Integer => self.parse_integer().map(Expression::Integer),
            Kind::Float => self.parse_float().map(Expression::Float),
            Kind::String => Some(Expression::StringLiteral(self.parse_string())),
            Kind::Function => self.parse_function().map(Expression::Function),
            Kind::Plus => self.parse_unary_op(OpName::Plus).map(Expression::UnaryOp),
            Kind::Minus => self.parse_unary_op(OpName::Minus).map(Expression::UnaryOp),
            Kind::True | Kind::False => Some(Expression::Boolean(self.parse_bool())),
            _ => {
                self.errors.push(format!(
                    "Parse error when parsing expression {:?}",
                    self.token
                ));
                None
            }
        }
    }

    fn parse_call(&mut self) -> Option<Call> {
        assert!(self.token.kind() == Kind::Identifier);
        let start = self.position;
        let name = self.token.text();
        self.read_token(); // consume identifier.

        if self.token.kind() != Kind::LeftParen {
            self.reset(start);
            return None;
        }
        self.read_token(); // consume '('.

        let mut argument_expressions = vec![];
        loop {
            match self.token.kind() {
                Kind::Comma => self.read_token(),
                Kind::RightParen => {
                    break;
                }
                _ => {
                    if let Some(expression) = self.parse_expression() {
                        argument_expressions.push(expression);
                    } else {
                        self.errors
                            .push(format!("expected ',' or identifier, got {:?}", self.token));
                        self.reset(start);
                        return None;
                    }
                }
            }
        }

        if self.token.kind() == Kind::RightParen {
            self.read_token(); // consume ')'.

            Some(Call {
                name,
                arguments: argument_expressions,
            })
        } else {
            self.reset(start);
            None
        }
    }

    fn parse_unary_op(&mut self, operation: OpName) -> Option<UnaryOp> {
        assert!(self.token.kind() == Kind::Plus || self.token.kind() == Kind::Minus);

        self.read_token(); // consume `+` or `-`
        let operand = self.parse_expression();
        operand.map(|target| UnaryOp {
            operation,
            target: Box::new(target),
        })
    }

    fn parse_bool(&mut self) -> bool {
        assert!(self.token.kind() == Kind::True || self.token.kind() == Kind::False);
        let result = self.token.kind() == Kind::True;
        self.read_token(); // consume `value`
        result
    }

    fn parse_integer(&mut self) -> Option<i32> {
        assert!(self.token.kind() == Kind::Integer);

        match self.token.kind() {
            Kind::Integer => {
                if let Ok(value) = self.token.text().parse::<i32>() {
                    self.read_token(); // consume `value`
                    Some(value)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_float(&mut self) -> Option<f64> {
        assert!(self.token.kind() == Kind::Float);
        match self.token.kind() {
            Kind::Float => {
                if let Ok(value) = self.token.text().parse::<f64>() {
                    self.read_token(); // consume `value`
                    Some(value)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // Matches: "name;"
    // Can be used on Strings or Identifiers
    fn parse_string(&mut self) -> String {
        assert!(self.token.kind() == Kind::String || self.token.kind() == Kind::Identifier);
        let name = self.token.text();
        self.read_token(); // consume `name`
        name
    }

    // Matches:
    //   "func ( identifier* ) {
    //     statement*
    //   };"
    // Indenting is not checked.
    fn parse_function(&mut self) -> Option<Rc<Function>> {
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

        let mut arguments = vec![];
        loop {
            match self.token.kind() {
                Kind::Identifier => {
                    arguments.push(self.token.text());
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

        let mut body = vec![];
        loop {
            match self.token.kind() {
                Kind::RightBrace => break,
                _ => {
                    if let Some(s) = self.parse_statement() {
                        body.push(s);
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

        Some(Rc::new(Function { arguments, body }))
    }

    fn parse_expression_statement(&mut self) -> Option<Expression> {
        self.parse_expression()
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.token.kind() {
            Kind::Let => {
                if let Some(stmt) = self.parse_let() {
                    self.read_token(); // consume ';'
                    Some(Statement::Let(stmt))
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing let-statement {:?}",
                        self.token
                    ));
                    None
                }
            }
            Kind::Return => {
                if let Some(stmt) = self.parse_return() {
                    self.read_token(); // consume ';'
                    Some(stmt)
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing return-statement {:?}",
                        self.token
                    ));
                    None
                }
            }
            _ => {
                if let Some(stmt) = self.parse_expression_statement() {
                    self.read_token(); // consume ';'
                    Some(Statement::Expression(stmt))
                } else {
                    self.errors.push(format!(
                        "Parse error: unexpected token kind when parsing statement {:?}",
                        self.token.kind()
                    ));
                    None
                }
            }
        }
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

    macro_rules! parser_error_test_case {
        ($test_name:ident, $test_case:expr) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($test_case.input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                for (expected, actual) in $test_case.expected_errors.iter().zip(errors.iter()) {
                    assert_eq!(
                        expected, actual,
                        "Parse error mismatch while parsing {}",
                        $test_case.input
                    );
                }

                match $test_case.expected_errors.len().cmp(&errors.len()) {
                    Ordering::Greater => {
                        for expected in &$test_case.expected_errors[errors.len()..] {
                            assert_eq!(
                                *expected, "",
                                "Expected parse error not encountered while parsing {}",
                                $test_case.input
                            );
                        }
                    }
                    Ordering::Less => {
                        for error in &errors[$test_case.expected_errors.len()..] {
                            assert_eq!(
                                error, "",
                                "Unexpected parse error encountered while parsing {}",
                                $test_case.input
                            );
                        }
                    }
                    Ordering::Equal => {}
                }
            }
        };
    }

    parser_error_test_case![
        let_assigns_to_an_integer,
        // Error cases
        ParserErrorTestCase {
            input: "let 123 = x;",
            expected_errors: vec![
                r#"expected identifier, got Token { text: "123", kind: Integer }"#,
                r#"Parse error when parsing let-statement Token { text: "let", kind: Let }"#,
            ],
        }
    ];

    #[derive(Debug)]
    struct ParseLetStatementTest {
        input: &'static str,
        identifier: &'static str,
        mutable: bool,
    }

    macro_rules! parse_let_statement_test_case {
        ($test_name:ident, $test_case:expr) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($test_case.input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                assert_eq!(ast.statements().len(), 1);
                match &ast.statements()[0] {
                    Statement::Let(let_statement) => {
                        assert_eq!(let_statement.identifier, $test_case.identifier);
                        assert_eq!(let_statement.mutable, $test_case.mutable);
                        // TODO: Check some property of the expression.
                    }
                    Statement::Expression(_) | Statement::Return(_) => {
                        panic!("Expected a let statement")
                    }
                }
            }
        };
    }

    parse_let_statement_test_case![
        let_identifier,
        ParseLetStatementTest {
            input: "let x = a;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_mutable_float,
        ParseLetStatementTest {
            input: "let mut minus_pi = -3.14159;",
            identifier: "minus_pi",
            mutable: true,
        }
    ];

    parse_let_statement_test_case![
        let_string,
        ParseLetStatementTest {
            input: r#"let x = "Hello";"#,
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_true,
        ParseLetStatementTest {
            input: r"let x = True;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_false,
        ParseLetStatementTest {
            input: r"let x = False;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_function,
        ParseLetStatementTest {
            input: "let first = func (a, b) { return a; };",
            identifier: "first",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_function_call,
        ParseLetStatementTest {
            input: "let max = largest (a, b);",
            identifier: "max",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_binary_add,
        ParseLetStatementTest {
            input: "let x = a + b;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_binary_subtract,
        ParseLetStatementTest {
            input: "let x = a - b;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_binary_multiply,
        ParseLetStatementTest {
            input: "let x = a * b;",
            identifier: "x",
            mutable: false,
        }
    ];

    parse_let_statement_test_case![
        let_binary_divide,
        ParseLetStatementTest {
            input: "let x = a / b;",
            identifier: "x",
            mutable: false,
        }
    ];

    struct ParseReturnStatementTest {
        input: &'static str,
    }

    macro_rules! parse_return_statement_test_case {
        ($test_name:ident, $test_case:expr) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($test_case.input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                assert_eq!(ast.statements().len(), 1);
                match &ast.statements()[0] {
                    Statement::Return(_) => {
                        // TODO: Check some property of the expression.
                    }
                    Statement::Expression(_) | Statement::Let(_) => {
                        panic!("Expected a return statement.")
                    }
                }
            }
        };
    }

    parse_return_statement_test_case![
        return_integer,
        ParseReturnStatementTest {
            input: "return 42;",
        }
    ];

    parse_return_statement_test_case![
        return_string_literal,
        ParseReturnStatementTest {
            input: r#"return "the solution";"#,
        }
    ];
}

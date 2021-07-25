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
    /// Creates a new Parser from a Vec<Token>, probably from a Lexer
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

    /// Creates an Abstract Syntax Tree from the parser.
    // Consumes the parser.
    #[must_use]
    pub(crate) fn ast(mut self) -> AbstractSyntaxTree {
        let mut statements = vec![];
        self.read_token();
        while let Some(statement) = self.parse_next() {
            statements.push(statement);
        }
        AbstractSyntaxTree::new(statements, self.errors)
    }

    /// Resets the read position updating the `token` field.
    fn reset(&mut self, position: usize) {
        self.position = position;
        self.read_position = position + 1;
        if self.position >= self.tokens.len() {
            self.token = Token::end_of_file(self.position);
        } else {
            self.token = self.tokens[self.position];
        }
    }

    /// Reads the next token updating the `token` field.
    fn read_token(&mut self) {
        if self.read_position >= self.tokens.len() {
            self.token = Token::end_of_file(self.read_position);
        } else {
            self.token = self.tokens[self.read_position];
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    /// Tries to parse a return statement.
    // Matches:
    //   `return expression;`
    fn parse_return(&mut self) -> Option<Statement> {
        assert_eq!(self.token.kind(), Kind::Return);
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

    /// Tries to parse a 'let' statement.
    // Matches:
    //   `let identifier = expression;`
    //   `let mut identifier = expression;`
    fn parse_let(&mut self) -> Option<Let> {
        assert_eq!(self.token.kind(), Kind::Let);
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

    fn parse_parenthesised_expression(&mut self) -> Option<Expression> {
        assert_eq!(self.token.kind(), Kind::LeftParen);
        let start = self.position;
        self.read_token(); // consume '('
        if let Some(expression) = self.parse_expression() {
            if self.token.kind() == Kind::RightParen {
                self.read_token(); // consume ')'
                Some(expression)
            } else {
                self.reset(start);
                None
            }
        } else {
            self.reset(start);
            None
        }
    }

    /// Tries to parse an expression.
    fn parse_expression(&mut self) -> Option<Expression> {
        if self.token.kind() == Kind::LeftParen {
            return self.parse_parenthesised_expression();
        }

        let start = self.position;

        let mut ungrouped_subexpressions: Vec<Expression> = vec![];
        let mut ungrouped_operators: Vec<OpName> = vec![];
        loop {
            if let Some(subexpr) = self.parse_subexpression() {
                ungrouped_subexpressions.push(subexpr);
                match self.token.kind() {
                    Kind::Plus => {
                        self.read_token();
                        ungrouped_operators.push(OpName::Plus);
                    }
                    Kind::Minus => {
                        self.read_token();
                        ungrouped_operators.push(OpName::Minus);
                    }
                    Kind::Divide => {
                        self.read_token();
                        ungrouped_operators.push(OpName::Divide);
                    }
                    Kind::Star => {
                        self.read_token();
                        ungrouped_operators.push(OpName::Multiply);
                    }
                    Kind::LeftParen => {
                        if let Some(paren_expression) = self.parse_expression() {
                            self.read_token();
                            ungrouped_subexpressions.push(paren_expression);
                        } else {
                            panic!("Parse nested subexpressions failed.",);
                        }
                    }
                    Kind::SemiColon | Kind::Comma | Kind::RightParen => {
                        break;
                    }
                    _ => {
                        panic!(
                            "Parse subexpressions failed, unexpected token {}, sub-expressions: {:?}, operators: {:?}",
                            self.token, ungrouped_subexpressions, ungrouped_operators
                        );
                    }
                }
            } else {
                self.reset(start);
                return None;
            }
        }

        if ungrouped_subexpressions.len() == 1 && ungrouped_operators.len() == 0 {
            return Some(ungrouped_subexpressions.pop()?);
        }

        if ungrouped_subexpressions.len() == 2 && ungrouped_operators.len() == 1 {
            let right = ungrouped_subexpressions.pop()?;
            let left = ungrouped_subexpressions.pop()?;
            let operation = ungrouped_operators.pop()?;
            return Some(Expression::BinaryOp(BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                operation,
            }));
        }

        panic!(
            "Parse subexpressions failed sub-expressions: {:?}, operators: {:?}",
            ungrouped_subexpressions, ungrouped_operators
        );
    }

    /// Tries to parse a subexpression.
    fn parse_subexpression(&mut self) -> Option<Expression> {
        match self.token.kind() {
            Kind::Identifier => match self.parse_call() {
                Some(function_call) => Some(Expression::Call(function_call)),
                None => Some(Expression::Identifier(self.parse_string())),
            },
            Kind::Integer => self.parse_integer().map(Expression::Integer),
            Kind::Float => self.parse_float().map(Expression::Float),
            Kind::String => Some(Expression::StringLiteral(Rc::new(self.parse_string()))),
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

    /// Tries to parse a call expression.
    fn parse_call(&mut self) -> Option<Call> {
        assert_eq!(self.token.kind(), Kind::Identifier);
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

    /// Tries to parse a unary operation expression.
    fn parse_unary_op(&mut self, operation: OpName) -> Option<UnaryOp> {
        assert!(self.token.kind() == Kind::Plus || self.token.kind() == Kind::Minus);

        self.read_token(); // consume `+` or `-`
        let operand = self.parse_expression();
        operand.map(|target| UnaryOp {
            operation,
            target: Box::new(target),
        })
    }

    /// Tries to parse a boolean expression.
    fn parse_bool(&mut self) -> bool {
        assert!(self.token.kind() == Kind::True || self.token.kind() == Kind::False);
        let result = self.token.kind() == Kind::True;
        self.read_token(); // consume `value`
        result
    }

    /// Tries to parse an integer expression.
    fn parse_integer(&mut self) -> Option<i32> {
        assert_eq!(self.token.kind(), Kind::Integer);

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

    /// Tries to parse a float expression.
    fn parse_float(&mut self) -> Option<f64> {
        assert_eq!(self.token.kind(), Kind::Float);
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

    /// Tries to parse a string-literal expression.
    // Matches: "name;"
    // Can be used on Strings or Identifiers
    fn parse_string(&mut self) -> String {
        assert!(self.token.kind() == Kind::String || self.token.kind() == Kind::Identifier);
        let name = self.token.text();
        self.read_token(); // consume `name`
        name
    }

    /// Tries to parse a function expression.
    // Matches:
    //   "func ( identifier* ) {
    //     statement*
    //   };"
    // Indenting is not checked.
    fn parse_function(&mut self) -> Option<Rc<Function>> {
        assert_eq!(self.token.kind(), Kind::Function);
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

        assert_eq!(self.token.kind(), Kind::RightParen);
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

    /// Tries to parse an expression statement.
    // Matches:
    //  expression
    fn parse_expression_statement(&mut self) -> Option<Expression> {
        self.parse_expression()
    }

    /// Tries to parse a statement.
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.token.kind() {
            Kind::Let => {
                if let Some(statement) = self.parse_let() {
                    self.read_token(); // consume ';'
                    Some(Statement::Let(statement))
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing let-statement {:?}",
                        self.token
                    ));
                    None
                }
            }
            Kind::Return => {
                if let Some(statement) = self.parse_return() {
                    self.read_token(); // consume ';'
                    Some(statement)
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing return-statement {:?}",
                        self.token
                    ));
                    None
                }
            }
            _ => {
                if let Some(statement) = self.parse_expression_statement() {
                    self.read_token(); // consume ';'
                    Some(Statement::Expression(statement))
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

    /// Tries to parse a top-level statement.
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
    use crate::{lexer::Lexer, matcher::*};

    macro_rules! parser_error_test_case {
        (name: $test_name:ident, input: $input:expr, expected_errors: $expected_errors:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                for (expected, actual) in $expected_errors.iter().zip(errors.iter()) {
                    assert_eq!(
                        expected, actual,
                        "Parse error mismatch while parsing {}",
                        $input
                    );
                }

                match $expected_errors.len().cmp(&errors.len()) {
                    Ordering::Greater => {
                        for expected in &$expected_errors[errors.len()..] {
                            assert_eq!(
                                *expected, "",
                                "Expected parse error not encountered while parsing {}",
                                $input
                            );
                        }
                    }
                    Ordering::Less => {
                        for error in &errors[$expected_errors.len()..] {
                            assert_eq!(
                                error, "",
                                "Unexpected parse error encountered while parsing {}",
                                $input
                            );
                        }
                    }
                    Ordering::Equal => {}
                }
            }
        };
    }

    parser_error_test_case! {
        name: let_assigns_to_an_integer,
        input: "let 123 = x;",
        expected_errors: &[
            r#"expected identifier, got Token { text: "123", kind: Integer }"#,
            r#"Parse error when parsing let-statement Token { text: "let", kind: Let }"#,
        ],
    }

    parser_error_test_case! {
        name: return_a_keyword,
        input: "return let;",
        expected_errors: &[
            r#"Parse error when parsing expression Token { text: "let", kind: Let }"#,
            r#"expected expression, got Token { text: "let", kind: Let }"#,
            r#"Parse error when parsing return-statement Token { text: "return", kind: Return }"#,
        ],
    }

    macro_rules! parse_statement_matcher_test_case {
        (name: $test_name:ident, input: $input:expr, matcher: $matcher:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                assert_eq!(ast.statements().len(), 1);
                assert!(
                    $matcher.matches(&ast.statements()[0]),
                    "Failed to match {}",
                    $input
                );
            }
        };
    }

    parse_statement_matcher_test_case! {
        name: let_identifier,
        input: "let x = a;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_identifier!("a".to_string())
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_mutable_float,
        input: "let mut x = 8.24;",
        matcher: match_mutable_let_statement!(
            "x".to_string(),
            match_float!(8.24)
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_string,
        input: r#"let x = "Hello";"#,
        matcher: match_let_statement!(
            "x".to_string(),
            match_string!("\"Hello\"".to_string())
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_true,
        input: r"let x = True;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_boolean!(true)
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_false,
        input: r"let x = False;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_boolean!( false)
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_function,
        input: "let first = func (a, b) { return a; };",
        matcher: match_let_statement!(
            "first".to_string(),
            match_function!(match_return_statement!(match_identifier!("a".to_string())))
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_function_call,
        input: "let max = largest (a, b);",
        matcher: match_let_statement!(
            "max".to_string(),
            match_call!(
                "largest".to_string(),
                match_identifier!("a".to_string()),
                match_identifier!("b".to_string())
            )
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_binary_add,
        input: "let x = a + b;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_binary_op!(
                match_identifier!("a".to_string()),
                match_identifier!("b".to_string()),
                OpName::Plus
            )
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_binary_subtract,
        input: "let x = a - b;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_binary_op!(
                match_identifier!("a".to_string()),
                match_identifier!("b".to_string()),
                OpName::Minus
            )
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_binary_multiply,
        input: "let x = a * b;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_binary_op!(
                match_identifier!("a".to_string()),
                match_identifier!("b".to_string()),
                OpName::Multiply
            )
        ),
    }

    parse_statement_matcher_test_case! {
        name: let_binary_divide,
        input: "let x = a / b;",
        matcher: match_let_statement!(
            "x".to_string(),
            match_binary_op!(
                match_identifier!("a".to_string()),
                match_identifier!("b".to_string()),
                OpName::Divide
            )
        ),
    }

    // RETURN
    parse_statement_matcher_test_case! {
        name: return_integer,
        input: "return 42;",
        matcher: match_return_statement!(match_integer!(42)),
    }

    parse_statement_matcher_test_case! {
        name: return_string_literal,
        input: r#"return "the solution";"#,
        matcher: match_return_statement!(match_string!("\"the solution\"".to_string())),
    }

    macro_rules! parse_expression_matcher_test_case {
        (name: $test_name:ident, input: $input:expr, matcher: $matcher:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                assert_eq!(ast.statements().len(), 1);

                match &ast.statements()[0] {
                    Statement::Expression(expr) => {
                        if !$matcher.matches(&expr) {
                            panic!("Failed to match {}", expr)
                        }
                    }
                    Statement::Let(_) | Statement::Return(_) => {
                        panic!("Expected an expression statement")
                    }
                }
            }
        };
    }

    parse_expression_matcher_test_case! {
        name: add_expression,
        input: "x + y;",
        matcher: match_binary_op!(
            match_identifier!("x".to_string()),
            match_identifier!("y".to_string()),
            OpName::Plus
        ),
    }

    parse_expression_matcher_test_case! {
        name: parenthesised_identifier,
        input: "(x);",
        matcher: match_identifier!("x".to_string()),
    }

    parse_expression_matcher_test_case! {
        name: nested_parenthesised_identifier,
        input: "(((x)));",
        matcher: match_identifier!("x".to_string()),
    }

    parse_expression_matcher_test_case! {
        name: parenthesised_chained_binary_operators_2,
        input: "x + (y + z);",
        matcher: match_binary_op!(
            match_identifier!("x".to_string()),
            match_identifier!("y".to_string()),
            OpName::Plus
        ),
    }

    parse_expression_matcher_test_case! {
        // This test currently passes by coincidence rather than
        // due to a robust implementation of subexpression parsing.
        // Operator precedence and parentheses are not yet handled.
        name: add_multiply_expression,
        input: "x + y * z;",
        matcher: match_binary_op!(
            match_identifier!("x".to_string()),
            match_binary_op!(
                match_identifier!("y".to_string()),
                match_identifier!("z".to_string()),
                OpName::Multiply
            ),
            OpName::Plus
        ),
    }

    parse_expression_matcher_test_case! {
        // This test currently fails due to an imperfect implementation
        // of subexpression parsing.
        // Operator precedence and parentheses are not yet handled.
        name: multiply_add_expression,
        input: "x * y + z;",
        matcher: match_binary_op!(
            match_binary_op!(
                match_identifier!("x".to_string()),
                match_identifier!("y".to_string()),
                OpName::Multiply
            ),
            match_identifier!("z".to_string()),
            OpName::Plus
        ),
    }
}

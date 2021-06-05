use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct StringLiteralExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct IntegerExpression {
    pub value: String,
}
#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

#[derive(Debug)]
pub enum ExpressionStatement {
    StringLiteralExpression(StringLiteralExpression),
    IntegerExpression(IntegerExpression),
    IdentifierExpression(IdentifierExpression),
}
#[derive(Debug)]
pub struct LetStatement {
    pub mutable: bool,
    pub identifier: String,
    pub expression: Box<ExpressionStatement>,
}
#[derive(Debug)]
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    pub statements: Vec<Statement>,
    errors: Vec<String>,
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    position: usize,
    read_position: usize,
    token: Token<'a>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        let mut tokens = vec![];
        for token in Lexer::new(input).tokens() {
            if token.kind() != TokenKind::Whitespace {
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
        if self.peek_token().kind() == TokenKind::Mut {
            mutable = true;
            self.read_token(); // consume mut
        }

        if self.token.kind() != TokenKind::Identifier {
            self.errors
                .push(format!("expected identifier, got {:?}", self.token));
            self.reset(start);
            return None;
        }
        let identifier = self.token.text();
        self.read_token(); // consume identifier

        if self.token.kind() != TokenKind::EqualSign {
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
            TokenKind::Identifier => {
                if let Some(identifier) = self.parse_identifier_expression() {
                    return Some(ExpressionStatement::IdentifierExpression(identifier));
                }
                None
            }
            TokenKind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(ExpressionStatement::IntegerExpression(integer));
                }
                None
            }
            TokenKind::String => {
                if let Some(string) = self.parse_string_literal_expression() {
                    return Some(ExpressionStatement::StringLiteralExpression(string));
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
        assert!(self.token.kind() == TokenKind::Identifier);
        match self.token.kind() {
            TokenKind::Identifier => {
                if self.peek_token().kind() != TokenKind::SemiColon {
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

    fn parse_integer_expression(&mut self) -> Option<IntegerExpression> {
        assert!(self.token.kind() == TokenKind::Integer);
        match self.token.kind() {
            TokenKind::Integer => {
                if self.peek_token().kind() != TokenKind::SemiColon {
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

    fn parse_string_literal_expression(&mut self) -> Option<StringLiteralExpression> {
        assert!(self.token.kind() == TokenKind::String);
        match self.token.kind() {
            TokenKind::String => {
                if self.peek_token().kind() != TokenKind::SemiColon {
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
            TokenKind::Let => {
                if let Some(stmt) = self.parse_let_statement() {
                    return Some(Statement::LetStatement(stmt));
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing let-statement {:?}",
                        self.token
                    ))
                }
                None
            }
            TokenKind::EOF => None,
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
mod parser_test {
    use super::*;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
    }

    #[test]
    fn parser_tests() {
        let test_cases = vec![
            TestCase {
                input: "let x = a;",
            },
            TestCase {
                input: "let x = 5;",
            },
            TestCase {
                input: r#"let x = "Hello";"#,
            },
        ];

        for test_case in test_cases.iter() {
            let parser = Parser::new(test_case.input);
            let ast = parser.ast();
            assert!(ast.errors.is_empty());
        }
    }
}

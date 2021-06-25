use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

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
    Expression(ExpressionStatement),
    Let(LetStatement),
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    pub statements: Vec<Statement>,
    errors: Vec<String>,
}

impl AbstractSyntaxTree {
    pub fn errors(&self) -> &Vec<String> {
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
                    return Some(ExpressionStatement::Identifier(identifier));
                }
                None
            }
            TokenKind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(ExpressionStatement::Integer(integer));
                }
                None
            }
            TokenKind::FloatingPoint => {
                if let Some(floating_point) = self.parse_floating_point_expression() {
                    return Some(ExpressionStatement::FloatingPoint(floating_point));
                }
                None
            }
            TokenKind::String => {
                if let Some(string) = self.parse_string_literal_expression() {
                    return Some(ExpressionStatement::StringLiteral(string));
                }
                None
            }
            TokenKind::Plus => {
                if let Some(expr) = self.parse_unary_plus_expression() {
                    return Some(ExpressionStatement::UnaryPlus(expr));
                }
                None
            }
            TokenKind::Minus => {
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

    fn parse_unary_plus_expression(&mut self) -> Option<UnaryPlusExpression> {
        assert!(self.token.kind() == TokenKind::Plus);
        self.read_token(); // consume `+`
        match self.token.kind() {
            TokenKind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(UnaryPlusExpression::Integer(integer));
                }
                None
            }
            TokenKind::FloatingPoint => {
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
        assert!(self.token.kind() == TokenKind::Minus);
        self.read_token(); // consume `-`
        match self.token.kind() {
            TokenKind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(UnaryMinusExpression::Integer(integer));
                }
                None
            }
            TokenKind::FloatingPoint => {
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

    fn parse_floating_point_expression(&mut self) -> Option<FloatingPointExpression> {
        assert!(self.token.kind() == TokenKind::FloatingPoint);
        match self.token.kind() {
            TokenKind::FloatingPoint => {
                if self.peek_token().kind() != TokenKind::SemiColon {
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
                    return Some(Statement::Let(stmt));
                } else {
                    self.errors.push(format!(
                        "Parse error when parsing let-statement {:?}",
                        self.token
                    ))
                }
                None
            }
            TokenKind::EndOfFile => None,
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

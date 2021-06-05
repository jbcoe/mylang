use crate::lexer::*;

pub struct StringLiteralExpression {
    pub value: String,
}
pub struct IntegerExpression {
    pub value: String,
}
pub struct IdentifierExpression {
    pub name: String,
}

pub enum ExpressionStatement {
    StringLiteralExpression(StringLiteralExpression),
    IntegerExpression(IntegerExpression),
    IdentifierExpression(IdentifierExpression),
}
pub struct LetStatement {
    pub mutable: bool,
    pub identifier: String,
    pub expression: Box<ExpressionStatement>,
}
pub enum Statement {
    ExpressionStatement(ExpressionStatement),
    LetStatement(LetStatement),
}

pub struct AbstractSyntaxTree {
    pub statements: Vec<Statement>,
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
        let tokens = Lexer::new(input).tokens();
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
            statements: statements,
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

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let start = self.position;
        self.read_token(); // consume let

        let mut mutable = true;
        if self.peek_token().kind() == TokenKind::Mut {
            mutable = true;
            self.read_token(); // consume mut
        }

        if self.peek_token().kind() != TokenKind::Identifier {
            self.reset(start);
            return None;
        }
        let identifier = self.token.text();
        self.read_token(); // consume identifier

        if self.peek_token().kind() != TokenKind::EqualSign {
            self.reset(start);
            return None;
        }
        self.read_token(); // consume equals sign

        let maybe_expression = self.parse_expression();
        match maybe_expression {
            None => {
                self.reset(start);
                return None;
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
                return None;
            }
            TokenKind::Integer => {
                if let Some(integer) = self.parse_integer_expression() {
                    return Some(ExpressionStatement::IntegerExpression(integer));
                }
                return None;
            }
            TokenKind::String => {
                if let Some(string) = self.parse_string_literal_expression() {
                    return Some(ExpressionStatement::StringLiteralExpression(string));
                }
                return None;
            }
            _ => None,
        }
    }

    fn parse_identifier_expression(&mut self) -> Option<IdentifierExpression> {
        None
    }

    fn parse_integer_expression(&mut self) -> Option<IntegerExpression> {
        None
    }

    fn parse_string_literal_expression(&mut self) -> Option<StringLiteralExpression> {
        None
    }

    fn parse_next(&mut self) -> Option<Statement> {
        self.read_token();
        match self.token.kind() {
            TokenKind::Let => {
                if let Some(stmt) = self.parse_let_statement() {
                    return Some(Statement::LetStatement(stmt));
                } else {
                    self.errors.push(format!(
                        "Parse error: when parsing let-statement {:?}",
                        self.token
                    ))
                }
                None
            }
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

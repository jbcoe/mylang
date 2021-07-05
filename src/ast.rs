#[derive(Debug)]
pub struct Call {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub enum OpName {
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub operation: OpName,
    pub target: Box<Expression>,
}

#[derive(Debug)]
pub struct Function {
    pub arguments: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Expression {
    Boolean(bool),
    Float(f64),
    Function(Function),
    Call(Call),
    Identifier(String),
    Integer(i32),
    StringLiteral(String),
    UnaryOp(UnaryOp),
}
#[derive(Debug)]
pub struct Let {
    pub mutable: bool,
    pub identifier: String,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Let(Let),
    Return(Box<Expression>),
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    statements: Vec<Statement>,
    errors: Vec<String>,
}

impl AbstractSyntaxTree {
    pub fn new(statements: Vec<Statement>, errors: Vec<String>) -> Self {
        Self { statements, errors }
    }
    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }
    pub const fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

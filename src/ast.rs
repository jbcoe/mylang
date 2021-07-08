#[derive(Debug)]
pub struct Call {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Expression>,
}

#[derive(Debug)]
pub enum OpName {
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub(crate) operation: OpName,
    pub(crate) target: Box<Expression>,
}

#[derive(Debug)]
pub struct Function {
    pub(crate) arguments: Vec<String>,
    pub(crate) body: Vec<Statement>,
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
    pub(crate) mutable: bool,
    pub(crate) identifier: String,
    pub(crate) expression: Box<Expression>,
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
    pub(crate) fn new(statements: Vec<Statement>, errors: Vec<String>) -> Self {
        Self { statements, errors }
    }
    pub(crate) const fn errors(&self) -> &Vec<String> {
        &self.errors
    }
    pub(crate) const fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

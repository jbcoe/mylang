#[derive(Debug)]
pub(crate) struct Call {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Expression>,
}

#[derive(Debug)]
pub(crate) enum OpName {
    Plus,
    Minus,
}

#[derive(Debug)]
pub(crate) struct UnaryOp {
    pub(crate) operation: OpName,
    pub(crate) target: Box<Expression>,
}

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) arguments: Vec<String>,
    pub(crate) body: Vec<Statement>,
}

#[derive(Debug)]
pub(crate) enum Expression {
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
pub(crate) struct Let {
    pub(crate) mutable: bool,
    pub(crate) identifier: String,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Let(Let),
    Return(Box<Expression>),
}

#[derive(Debug)]
pub(crate) struct AbstractSyntaxTree {
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

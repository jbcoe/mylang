use std::{fmt, rc::Rc};

#[derive(Debug, PartialEq)]
pub struct Call {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Expression>,
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.arguments
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum OpName {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl fmt::Display for OpName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpName::Plus => write!(f, "+"),
            OpName::Minus => write!(f, "-"),
            OpName::Multiply => write!(f, "*"),
            OpName::Divide => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub(crate) left: Box<Expression>,
    pub(crate) operation: OpName,
    pub(crate) right: Box<Expression>,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operation, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryOp {
    pub(crate) operation: OpName,
    pub(crate) target: Box<Expression>,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.operation, self.target)
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub(crate) arguments: Vec<String>,
    pub(crate) body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Boolean(bool),
    Float(f64),
    Function(Rc<Function>),
    Call(Call),
    Identifier(String),
    Integer(i32),
    StringLiteral(Rc<String>),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

impl fmt::Display for Expression {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Boolean(b) => write!(formatter, "{}", b),
            Expression::Float(f) => write!(formatter, "{}", f),
            Expression::Identifier(i) => write!(formatter, "{}", i),
            Expression::Integer(i) => write!(formatter, "{}", i),
            Expression::StringLiteral(s) => write!(formatter, "{}", s),
            Expression::Call(c) => write!(formatter, "{}", c),
            Expression::UnaryOp(op) => write!(formatter, "{}", op),
            Expression::BinaryOp(op) => write!(formatter, "{}", op),
            Expression::Function(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub(crate) mutable: bool,
    pub(crate) identifier: String,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug, PartialEq)]
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

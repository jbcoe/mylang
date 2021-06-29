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
pub struct FunctionCallExpression {
    pub name: String,
    pub arguments: Vec<Expression>,
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
    pub arguments: Vec<String>,
    pub body: Vec<Statement>,
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
    FunctionCall(FunctionCallExpression),
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

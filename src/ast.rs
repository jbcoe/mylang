use std::{
    cmp::Ordering,
    fmt::{self},
    rc::Rc,
};

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

impl PartialOrd for OpName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            OpName::Plus | OpName::Minus => match other {
                OpName::Plus | OpName::Minus => Some(Ordering::Equal),
                OpName::Multiply | OpName::Divide => Some(Ordering::Less),
            },
            OpName::Multiply | OpName::Divide => match other {
                OpName::Plus | OpName::Minus => Some(Ordering::Greater),
                OpName::Multiply | OpName::Divide => Some(Ordering::Equal),
            },
        }
    }
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "func ({}) {{\n{}\n}}",
            self.arguments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.body
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
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
            Expression::Function(f) => write!(formatter, "{}", f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub(crate) mutable: bool,
    pub(crate) identifier: String,
    pub(crate) expression: Box<Expression>,
}
impl fmt::Display for Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mutable = if self.mutable { "mut " } else { "" };
        write!(
            f,
            "let {}{} = {}",
            mutable, self.identifier, self.expression
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(Let),
    Return(Box<Expression>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{};", expression),
            Statement::Let(let_statement) => write!(f, "{};", let_statement),
            Statement::Return(expression) => write!(f, "return {};", expression),
        }
    }
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    statements: Vec<Statement>,
    errors: Vec<String>,
}

impl fmt::Display for AbstractSyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|statement| format!("{}", statement))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
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

#[cfg(test)]
mod tests {

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    macro_rules! ast_display_test {
        (name: $test_name:ident ,input: $input:expr, expected: $expected:expr) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);

                assert_eq!(format!("{}", ast), $expected)
            }
        };
    }

    ast_display_test! {
        name: display_return_statement,
        input: "return 7;",
        expected: "return 7;"
    }

    ast_display_test! {
        name: display_let_statement,
        input: "let x = 7;",
        expected: "let x = 7;"
    }

    ast_display_test! {
        name: display_mutable_let_statement,
        input: r#"let mut x = "hello";"#,
        expected: r#"let mut x = "hello";"#
    }

    ast_display_test! {
        name: display_unary_operator,
        input: r#"let mut x = -2.3;"#,
        expected: r#"let mut x = -2.3;"#
    }

    ast_display_test! {
        name: display_binary_operator,
        input: r#"let x = 2 + 3;"#,
        expected: r#"let x = (2 + 3);"#
    }

    ast_display_test! {
        name: display_function,
        input: r#"let f = func (a, b) {
            let z = 0;
            let y = b;
            return -42;
        };"#,
        expected: r#"let f = func (a, b) {
let z = 0;
let y = b;
return -42;
};"#
    }

    ast_display_test! {
        name: display_function_call,
        input: "let x = f(1, 2);",
        expected: "let x = f(1, 2);"
    }
}

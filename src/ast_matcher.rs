use crate::ast::{Expression, OpName, Statement};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
}

pub trait StatementMatcher {
    fn matches(&self, statement: &Statement) -> bool;
}
pub struct AnyExpressionMatcher {}

impl ExpressionMatcher for AnyExpressionMatcher {
    fn matches(&self, _: &Expression) -> bool {
        true
    }
}
pub struct AnyIdentifierMatcher {}

impl ExpressionMatcher for AnyIdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Identifier(_))
    }
}

pub struct FloatMatcher {
    pub(crate) value: f64,
}

impl ExpressionMatcher for FloatMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Float(f) => f.to_bits() == self.value.to_bits(),
            _ => false,
        }
    }
}

pub struct IntegerMatcher {
    pub(crate) value: i32,
}

impl ExpressionMatcher for IntegerMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Integer(i) => *i == self.value,
            _ => false,
        }
    }
}

pub struct BooleanMatcher {
    pub(crate) value: bool,
}

impl ExpressionMatcher for BooleanMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Boolean(b) => *b == self.value,
            _ => false,
        }
    }
}
pub struct StringMatcher {
    pub(crate) value: String,
}

impl ExpressionMatcher for StringMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::StringLiteral(s) => **s == self.value,
            _ => false,
        }
    }
}

pub struct IdentifierMatcher {
    pub(crate) identifier: String,
}

impl ExpressionMatcher for IdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Identifier(i) => *i == self.identifier,
            _ => false,
        }
    }
}

pub struct AnyFunctionMatcher {}

impl ExpressionMatcher for AnyFunctionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Function(_))
    }
}

pub struct CallMatcher {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Box<dyn ExpressionMatcher>>,
}

impl ExpressionMatcher for CallMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Call(call) => {
                if call.name != self.name {
                    return false;
                }
                for (argument, matcher) in call.arguments.iter().zip(&self.arguments) {
                    if !matcher.matches(argument) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

pub struct AnyBinaryOperatorExpressionMatcher {}

impl ExpressionMatcher for AnyBinaryOperatorExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::BinaryOp(_))
    }
}
pub struct BinaryOperatorExpressionMatcher {
    pub(crate) left: Box<dyn ExpressionMatcher>,
    pub(crate) right: Box<dyn ExpressionMatcher>,
    pub(crate) operator: OpName,
}

impl ExpressionMatcher for BinaryOperatorExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::BinaryOp(binary_op) => {
                if binary_op.operation != self.operator {
                    return false;
                }
                if !self.left.matches(&binary_op.left) {
                    return false;
                }

                if !self.right.matches(&binary_op.right) {
                    return false;
                }
                true
            }
            _ => false,
        }
    }
}

pub struct ReturnStatementMatcher {
    pub(crate) expression: Box<dyn ExpressionMatcher>,
}

impl StatementMatcher for ReturnStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match statement {
            Statement::Return(expression) => self.expression.matches(&expression),
            _ => false,
        }
    }
}
pub struct LetStatementMatcher {
    pub(crate) identifier: String,
    pub(crate) mutable: bool,
    pub(crate) expression: Box<dyn ExpressionMatcher>,
}

impl StatementMatcher for LetStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match &statement {
            Statement::Let(let_statement) => {
                self.expression.matches(&let_statement.expression)
                    && self.identifier == let_statement.identifier
                    && self.mutable == let_statement.mutable
            }
            _ => false,
        }
    }
}

pub struct AnyLetStatementMatcher {
    pub(crate) expression: Box<dyn ExpressionMatcher>,
}

impl StatementMatcher for AnyLetStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match &statement {
            Statement::Let(let_statement) => self.expression.matches(&let_statement.expression),
            _ => false,
        }
    }
}

use crate::ast::{Expression, OpName};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
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

use crate::ast::{Expression, OpName};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
}
pub(crate) struct AnyExpressionMatcher {}

impl ExpressionMatcher for AnyExpressionMatcher {
    fn matches(&self, _: &Expression) -> bool {
        true
    }
}
pub(crate) struct AnyIdentifierMatcher {}

impl ExpressionMatcher for AnyIdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Identifier(_) => true,
            _ => false,
        }
    }
}
pub(crate) struct IdentifierMatcher {
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

pub(crate) struct AnyBinaryOperatorExpressionMatcher {}

impl ExpressionMatcher for AnyBinaryOperatorExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::BinaryOp(_) => true,
            _ => false,
        }
    }
}
pub(crate) struct BinaryOperatorExpressionMatcher {
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
                return true;
            }
            _ => {
                return false;
            }
        }
    }
}

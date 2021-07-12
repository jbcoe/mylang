// allow "Matcher" on the end of trait names even though
// this module is called matcher

#![allow(clippy::module_name_repetitions)]
#![allow(unused_macros)]

use crate::ast::{Expression, OpName, Statement};

pub trait ExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool;
}

pub trait StatementMatcher {
    fn matches(&self, statement: &Statement) -> bool;
}

pub struct FloatMatcher {
    pub(crate) value: f64,
}

pub struct AnyFloatMatcher {}

macro_rules! match_float {
    ($value:expr) => {
        Box::new(FloatMatcher { value: $value })
    };
    () => {
        Box::new(AnyFloatMatcher {})
    };
}

impl ExpressionMatcher for FloatMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Float(f) => f.to_bits() == self.value.to_bits(),
            _ => false,
        }
    }
}

impl ExpressionMatcher for AnyFloatMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Float(_))
    }
}

pub struct IntegerMatcher {
    pub(crate) value: i32,
}

pub struct AnyIntegerMatcher {}

macro_rules! match_integer {
    ($value:expr) => {
        Box::new(IntegerMatcher { value: $value })
    };
    () => {
        Box::new(AnyIntegerMatcher {})
    };
}

impl ExpressionMatcher for IntegerMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Integer(i) => *i == self.value,
            _ => false,
        }
    }
}

impl ExpressionMatcher for AnyIntegerMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Integer(_))
    }
}

pub struct BooleanMatcher {
    pub(crate) value: bool,
}

pub struct AnyBooleanMatcher {}
macro_rules! match_boolean {
    ($value:expr) => {
        Box::new(BooleanMatcher { value: $value })
    };
    () => {
        Box::new(AnyBooleanMatcher {})
    };
}

impl ExpressionMatcher for BooleanMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Boolean(b) => *b == self.value,
            _ => false,
        }
    }
}

impl ExpressionMatcher for AnyBooleanMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Boolean(_))
    }
}

pub struct StringMatcher {
    pub(crate) value: String,
}

pub struct AnyStringMatcher {}

macro_rules! match_string {
    ($value:expr) => {
        Box::new(StringMatcher { value: $value })
    };
    () => {
        Box::new(AnyStringMatcher {})
    };
}

impl ExpressionMatcher for StringMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::StringLiteral(s) => **s == self.value,
            _ => false,
        }
    }
}

impl ExpressionMatcher for AnyStringMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::StringLiteral(_))
    }
}
pub struct IdentifierMatcher {
    pub(crate) identifier: String,
}

pub struct AnyIdentifierMatcher {}

macro_rules! match_identifier {
    ($identifier:expr) => {
        Box::new(IdentifierMatcher {
            identifier: $identifier,
        })
    };
    () => {
        Box::new(AnyIdentifierMatcher {})
    };
}

impl ExpressionMatcher for IdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Identifier(i) => *i == self.identifier,
            _ => false,
        }
    }
}

impl ExpressionMatcher for AnyIdentifierMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Identifier(_))
    }
}

pub struct AnyFunctionMatcher {}

macro_rules! match_function {
    () => {
        Box::new(AnyFunctionMatcher {})
    };
}

impl ExpressionMatcher for AnyFunctionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::Function(_))
    }
}

pub struct CallMatcher {
    pub(crate) name: String,
    pub(crate) matchers: Vec<Box<dyn ExpressionMatcher>>,
}
macro_rules! match_call {
    ($name:expr, $matchers:expr, $operator:expr) => {
        Box::new(CallMatcher {
            name: $name,
            matchers: $matchers,
        })
    };
}

impl ExpressionMatcher for CallMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Call(call) => {
                call.name == self.name
                    && self.matchers.len() == call.arguments.len()
                    && call
                        .arguments
                        .iter()
                        .zip(&self.matchers)
                        .all(|(argument, matcher)| matcher.matches(argument))
            }
            _ => false,
        }
    }
}

pub struct AnyBinaryOperatorExpressionMatcher {}

pub struct BinaryOperatorExpressionMatcher {
    pub(crate) left: Box<dyn ExpressionMatcher>,
    pub(crate) right: Box<dyn ExpressionMatcher>,
    pub(crate) operator: OpName,
}

macro_rules! match_binary_op {
    ($left:expr, $right:expr, $operator:expr) => {
        Box::new(BinaryOperatorExpressionMatcher {
            left: $left,
            right: $right,
            operator: $operator,
        })
    };
    () => {
        Box::new(AnyBinaryOperatorExpressionMatcher {})
    };
}

impl ExpressionMatcher for AnyBinaryOperatorExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        matches!(expression, Expression::BinaryOp(_))
    }
}

impl ExpressionMatcher for BinaryOperatorExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::BinaryOp(binary_op) => {
                binary_op.operation == self.operator
                    && self.left.matches(&binary_op.left)
                    && self.right.matches(&binary_op.right)
            }
            _ => false,
        }
    }
}

pub struct ReturnStatementMatcher {
    pub(crate) matcher: Box<dyn ExpressionMatcher>,
}

macro_rules! match_return_stmt {
    ($matcher:expr) => {
        Box::new(ReturnStatementMatcher { matcher: $matcher })
    };
}

impl StatementMatcher for ReturnStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match statement {
            Statement::Return(expression) => self.matcher.matches(expression),
            _ => false,
        }
    }
}

pub struct LetStatementMatcher {
    pub(crate) identifier: String,
    pub(crate) mutable: bool,
    pub(crate) matcher: Box<dyn ExpressionMatcher>,
}

pub struct AnyLetStatementMatcher {}

macro_rules! match_let_stmt {
    ($identifier:expr, $mutable:expr, $matcher:expr) => {
        Box::new(LetStatementMatcher {
            identifier: $identifier,
            mutable: $mutable,
            matcher: $matcher,
        })
    };
    () => {
        Box::new(AnyLetStatementMatcher)
    };
}

impl StatementMatcher for LetStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match &statement {
            Statement::Let(let_statement) => {
                self.matcher.matches(&let_statement.expression)
                    && self.identifier == let_statement.identifier
                    && self.mutable == let_statement.mutable
            }
            _ => false,
        }
    }
}

impl StatementMatcher for AnyLetStatementMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        matches!(statement, Statement::Let(_))
    }
}

pub struct PartialFunctionBodyMatcher {
    pub(crate) matcher: Box<dyn StatementMatcher>,
}

macro_rules! match_function {
    ($matcher:expr) => {
        Box::new(PartialFunctionBodyMatcher { matcher: $matcher })
    };
}

impl ExpressionMatcher for PartialFunctionBodyMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Function(function) => function
                .body
                .iter()
                .any(|statement| self.matcher.matches(&statement)),
            _ => false,
        }
    }
}

pub struct FullFunctionBodyMatcher {
    pub(crate) matchers: Vec<Box<dyn StatementMatcher>>,
}

macro_rules! match_full_function {
    ($matcher:expr) => {
        Box::new(FullFunctionBodyMatcher { matcher: $matcher })
    };
}

impl ExpressionMatcher for FullFunctionBodyMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Function(function) => self
                .matchers
                .iter()
                .zip(&function.body)
                .all(|(matcher, statement)| matcher.matches(&statement)),
            _ => false,
        }
    }
}

pub struct DescendingExpressionMatcher {
    pub(crate) matcher: Box<dyn ExpressionMatcher>,
}

macro_rules! match_descend {
    ($matcher:expr) => {
        Box::new(DescendingExpressionMatcher { matcher: $matcher })
    };
}

impl StatementMatcher for DescendingExpressionMatcher {
    fn matches(&self, statement: &Statement) -> bool {
        match &statement {
            Statement::Expression(expression) => ExpressionMatcher::matches(self, expression),
            Statement::Let(let_expression) => {
                ExpressionMatcher::matches(self, &let_expression.expression)
            }
            Statement::Return(expression) => ExpressionMatcher::matches(self, expression),
        }
    }
}

impl ExpressionMatcher for DescendingExpressionMatcher {
    fn matches(&self, expression: &Expression) -> bool {
        if self.matcher.matches(expression) {
            true
        } else {
            match &expression {
                Expression::Boolean(_)
                | Expression::Float(_)
                | Expression::StringLiteral(_)
                | Expression::Identifier(_)
                | Expression::Integer(_) => false,
                Expression::Call(call) => call
                    .arguments
                    .iter()
                    .any(|argument| self.matcher.matches(&argument)),
                Expression::Function(function) => function
                    .body
                    .iter()
                    .any(|statement| StatementMatcher::matches(self, &statement)),
                Expression::UnaryOp(op) => self.matcher.matches(&op.target),
                Expression::BinaryOp(op) => {
                    self.matcher.matches(&op.left) || self.matcher.matches(&op.right)
                }
            }
        }
    }
}

struct CallbackExpressionMatcher<T: Fn(&Expression), F: Fn(&Expression)> {
    matcher: Box<dyn ExpressionMatcher>,
    on_true: T,
    on_false: F,
}

impl<T: Fn(&Expression), F: Fn(&Expression)> ExpressionMatcher for CallbackExpressionMatcher<T, F> {
    fn matches(&self, expression: &Expression) -> bool {
        if self.matcher.matches(expression) {
            (self.on_true)(&expression);
            true
        } else {
            (self.on_false)(&expression);
            false
        }
    }
}

macro_rules! match_debug {
    ($matcher:expr) => {
        Box::new(CallbackExpressionMatcher {
            matcher: $matcher,
            on_true: |expression: &Expression| {
                println!("Matched: {:?}", &expression);
            },
            on_false: |expression: &Expression| {
                println!("Did not match: {:?}", &expression);
            },
        })
    };
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    macro_rules! matcher_test_case {
        (name: $name:ident, input: $input:expr, matcher: $matcher:expr,) => {
            #[test]
            fn $name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                assert_eq!(ast.statements().len(), 1);
                assert!(
                    StatementMatcher::matches(&*$matcher, &ast.statements()[0]),
                    "Failed to match {}",
                    $input
                );
            }
        };
    }

    matcher_test_case! {
        name: descending_matcher_into_function_definition,
        input: "func() { let x = 5; func (x, y) { let x = 5; return x; }; };",
        matcher: match_descend!(match_debug!(match_identifier!())),
    }

    matcher_test_case! {
        name: string_matcher,
        input: r#"return "Hello";"#,
        matcher: match_descend!(match_string!(r#""Hello""#.to_string())),
    }

    matcher_test_case! {
        name: any_string_matcher,
        input: r#"return "Hello";"#,
        matcher: match_descend!(match_string!()),
    }

    matcher_test_case! {
        name: float_matcher,
        input: r#"return 2.71;"#,
        matcher: match_descend!(match_float!(2.71)),
    }

    matcher_test_case! {
        name: any_float_matcher,
        input: r#"return 2.71;"#,
        matcher: match_descend!(match_float!()),
    }

    matcher_test_case! {
        name: integer_matcher,
        input: r#"return 5;"#,
        matcher: match_descend!(match_debug!(match_integer!(5))),
    }

    matcher_test_case! {
        name: any_integer_matcher,
        input: r#"return 5;"#,
        matcher: match_descend!(match_debug!(match_integer!())),
    }

    matcher_test_case! {
        name: bool_matcher,
        input: r#"return True;"#,
        matcher: match_descend!(match_boolean!(true)),
    }

    matcher_test_case! {
        name: any_bool_matcher,
        input: r#"return True;"#,
        matcher: match_descend!(match_boolean!()),
    }

    matcher_test_case! {
        name: any_binary_op_matcher,
        input: r#"return a + b;"#,
        matcher: match_descend!(match_binary_op!()),
    }
}

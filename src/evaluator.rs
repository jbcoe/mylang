use crate::ast::AbstractSyntaxTree;
use crate::frame::{Frame, Value};
use thiserror::Error;

pub struct Evaluator {
    global: Frame,
}

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Illegal non-i32 return type `{0}` of value `{1}` at global scope")]
    NonPermitted(String, String),
}

impl<'a> Evaluator {
    pub(crate) fn new() -> Self {
        Self {
            global: Frame::new(),
        }
    }

    pub(crate) fn evaluate(&mut self, ast: &AbstractSyntaxTree) -> Result<i32, Error> {
        // Evaluate statements at global scope until one of them returns.
        if let Some(rc) = self.global.evaluate_body(ast.statements()) {
            match &*rc {
                Value::Integer(i) => Ok(*i),
                Value::Boolean(b) => {
                    Err(Error::NonPermitted("Boolean".to_string(), format!("{}", b)))
                }
                Value::Float(f) => Err(Error::NonPermitted("Float".to_string(), format!("{}", f))),
                Value::Function(_) => Err(Error::NonPermitted(
                    "Function".to_string(),
                    "unknown".to_string(),
                )),
                Value::String(s) => Err(Error::NonPermitted("String".to_string(), s.clone())),
            }
        } else {
            Ok(0)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::{Error, Evaluator},
        lexer::Lexer,
        parser::Parser,
    };

    macro_rules! positive_evaluator_test_case {
        (name: $test_name:ident, input: $input:expr, return_value: $return_value:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                let mut evaluator = Evaluator::new();
                assert_eq!(evaluator.evaluate(&ast).unwrap(), $return_value);
            }
        };
    }

    positive_evaluator_test_case! {
        name: return_integer,
        input: "return 42;",
        return_value: 42,
    }

    positive_evaluator_test_case! {
        name: return_unary_plus_integer,
        input: "return +42;",
        return_value: 42,
    }

    positive_evaluator_test_case! {
        name: let_string_no_return,
        input: r#"let a = "Hello";"#,
        return_value: 0,
    }

    positive_evaluator_test_case! {
        name: let_float_no_return,
        input: "let a = 3.14159;",
        return_value: 0,
    }

    positive_evaluator_test_case! {
        name: let_unary_minus_float_no_return,
        input: "let a = -3.14159;",
        return_value: 0,
    }

    positive_evaluator_test_case! {
        name: let_integer_no_return,
        input: "let a = 42;",
        return_value: 0,
    }

    positive_evaluator_test_case! {
        name: let_integer_return_identifier,
        input: r#"
            let a = 42;
            return a;"#,
        return_value: 42,
    }

    positive_evaluator_test_case! {
        name: let_integer_return_unary_minus_identifier,
        input: r#"
            let a = 42;
            return -a;"#,
        return_value: -42,
    }

    positive_evaluator_test_case! {
        name: let_function_no_return,
        input: r#"
            let a = func ( x, y ) {
                return x;
            };"#,
        return_value: 0,
    }

    positive_evaluator_test_case! {
        name: let_function_return_invocation_with_identifiers,
        input: r#"
            # Define a function.
            let first = func (a, b) {
                return a; # indenting is non-syntactic.
            };
            let x = 1;
            let y = 2;
            let z = first(x, y);
            return z;"#,
        return_value: 1,
    }

    positive_evaluator_test_case! {
        name: let_function_return_invocation_with_literals,
        input: r#"
            let first = func (a, b) {
                return a;
            };
            return first(101, 2);"#,
        return_value: 101,
    }

    positive_evaluator_test_case! {
        name: assorted_let_with_return_integer,
        input: r#"
            let a = 42;
            let b = "Hello";
            let c = 3.14159;
            return 7;"#,
        return_value: 7,
    }

    positive_evaluator_test_case! {
        name: assorted_let_with_return_identifier,
        input: r#"
            let a = 42;
            a;"#,
        return_value: 0, // not 42
    }

    macro_rules! negative_evaluator_test_case {
        (name: $test_name:ident, input: $input:expr, return_value: $return_value:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                let mut evaluator = Evaluator::new();
                assert_eq!(evaluator.evaluate(&ast).unwrap_err(), $return_value);
            }
        };
    }

    negative_evaluator_test_case! {
        name: return_float,
        input: "return 3.14;",
        return_value: Error::NonPermitted("Float".to_string(), "3.14".to_string()),
    }
}

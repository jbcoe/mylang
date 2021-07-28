use crate::ast::AbstractSyntaxTree;
use crate::frame::{EvaluationError, Frame, Value};
use thiserror::Error;
pub struct Evaluator {
    global: Frame,
}

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("Illegal non-i32 return type {0} at global scope")]
    NonPermitted(String),
    #[error("{source:?}")]
    Evaluation {
        #[from]
        source: EvaluationError,
    },
}

impl<'a> Evaluator {
    pub(crate) fn new() -> Self {
        Self {
            global: Frame::new(),
        }
    }

    /// Evaluates the ast's statements at global scope until one of them returns
    pub(crate) fn evaluate(&mut self, ast: &AbstractSyntaxTree) -> Result<i32, Error> {
        (self.global.evaluate_body(ast.statements())?).map_or(Ok(0), |value| match &*value {
            Value::Integer(i) => Ok(*i),
            Value::Boolean(_) | Value::Float(_) | Value::Function(_) | Value::String(_) => {
                Err(Error::NonPermitted((*value).to_string()))
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::{Error, Evaluator},
        frame::EvaluationError,
        lexer::Lexer,
        parser::Parser,
    };

    macro_rules! evaluator_test_case {
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

    evaluator_test_case! {
        name: return_integer,
        input: "return 42;",
        return_value: 42,
    }

    evaluator_test_case! {
        name: return_unary_plus_integer,
        input: "return +42;",
        return_value: 42,
    }

    evaluator_test_case! {
        name: let_string_no_return,
        input: r#"let a = "Hello";"#,
        return_value: 0,
    }

    evaluator_test_case! {
        name: let_float_no_return,
        input: "let a = 3.14159;",
        return_value: 0,
    }

    evaluator_test_case! {
        name: let_unary_minus_float_no_return,
        input: "let a = -3.14159;",
        return_value: 0,
    }

    evaluator_test_case! {
        name: let_integer_no_return,
        input: "let a = 42;",
        return_value: 0,
    }

    evaluator_test_case! {
        name: let_integer_return_identifier,
        input: r#"
            let a = 42;
            return a;"#,
        return_value: 42,
    }

    evaluator_test_case! {
        name: let_integer_return_unary_minus_identifier,
        input: r#"
            let a = 42;
            return -a;"#,
        return_value: -42,
    }

    evaluator_test_case! {
        name: let_function_no_return,
        input: r#"
            let a = func ( x, y ) {
                return x;
            };"#,
        return_value: 0,
    }

    evaluator_test_case! {
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

    evaluator_test_case! {
        name: let_function_return_invocation_with_literals,
        input: r#"
            let first = func (a, b) {
                return a;
            };
            return first(101, 2);"#,
        return_value: 101,
    }

    evaluator_test_case! {
        name: assorted_let_with_return_integer,
        input: r#"
            let a = 42;
            let b = "Hello";
            let c = 3.14159;
            return 7;"#,
        return_value: 7,
    }

    evaluator_test_case! {
        name: assorted_let_with_return_identifier,
        input: r#"
            let a = 42;
            a;"#,
        return_value: 0, // not 42
    }

    evaluator_test_case! {
        name: return_deeply_nested_function_call,
        input: r#"
            let f = func () {
                let f = func(){
                    let f = func(){
                        let f = func(){
                            return 42;
                        };
                        return f();
                    };
                    return f();
                };
                return f();
            };
            return f();"#,
        return_value: 42,
    }

    evaluator_test_case! {
        name: return_binary_op_plus_integers,
        input: "return 2 + 2;",
        return_value: 4,
    }

    evaluator_test_case! {
        name: return_binary_op_minus_integers,
        input: "return 2 - 2;",
        return_value: 0,
    }

    evaluator_test_case! {
        name: return_binary_op_multiply_integers,
        input: "return 2 * 2;",
        return_value: 4,
    }

    evaluator_test_case! {
        name: return_binary_op_divide_integers,
        input: "return 2 / 2;",
        return_value: 1,
    }

    macro_rules! evaluator_error_test_case {
        (name: $test_name:ident, input: $input:expr, err_value: $err_value:expr,) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                let mut evaluator = Evaluator::new();
                assert_eq!(evaluator.evaluate(&ast).unwrap_err(), $err_value);
            }
        };
    }

    evaluator_error_test_case! {
        name: return_boolean_err,
        input: "return True;",
        err_value: Error::NonPermitted("Boolean(true)".to_string()),
    }

    evaluator_error_test_case! {
        name: return_float_err,
        input: "return 3.14;",
        err_value: Error::NonPermitted("Float(3.14)".to_string()),
    }

    evaluator_error_test_case! {
        name: return_function_err,
        input: r#"
            return func (a, b) {
                return a;
            };"#,
        err_value: Error::NonPermitted("Function()".to_string()),
    }

    evaluator_error_test_case! {
        name: return_string_err,
        input: r#"return "meow";"#,
        err_value: Error::NonPermitted(r#"String("meow")"#.to_string()),
    }

    evaluator_error_test_case! {
        name: unary_op_on_string,
        input: r#"let x = -"Hello";"#,
        err_value: Error::Evaluation{
            source: EvaluationError::IllegalUnaryOperation{
                opname: "-".to_string(),
                value:"String(\"Hello\")".to_string()
            }
        },
    }

    evaluator_error_test_case! {
         name: unary_op_on_bool,
         input: r#"let x = -True;"#,
         err_value: Error::Evaluation{
             source: EvaluationError::IllegalUnaryOperation{
                 opname: "-".to_string(),
                 value:"Boolean(true)".to_string()
             }
         },
    }

    evaluator_error_test_case! {
        name: call_int_err,
        input: r#"let x = 5; return x();"#,
        err_value: Error::Evaluation{
            source: EvaluationError::NonCallableType("Integer(5)".to_string())
        },
    }

    evaluator_error_test_case! {
        name: call_float_err,
        input: r#"let x = 3.14159; return x();"#,
        err_value: Error::Evaluation{
            source: EvaluationError::NonCallableType("Float(3.14159)".to_string())
        },
    }

    evaluator_error_test_case! {
        name: call_string_err,
        input: r#"let x = "hello"; return x();"#,
        err_value: Error::Evaluation{
            source: EvaluationError::NonCallableType("String(\"hello\")".to_string())
        },
    }

    evaluator_error_test_case! {
        name: call_bool_err,
        input: r#"let x = True; return x();"#,
        err_value: Error::Evaluation{
            source: EvaluationError::NonCallableType("Boolean(true)".to_string())
        },
    }

    evaluator_error_test_case! {
        name: return_binary_op_plus_mixed_inputs,
        input: "return 2 + 2.0;",
        err_value: Error::Evaluation{
            source: EvaluationError::IllegalBinaryOperation {
                opname: "+".to_string(),
                left: "Integer(2)".to_string(),
                right: "Float(2)".to_string()
            }
        },
    }
}

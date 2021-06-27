use std::collections::HashMap;

use anyhow::Result;

use crate::parser::{AbstractSyntaxTree, Expression, Statement};

struct Environment {
    values: HashMap<String, Value>,
}
enum Value {
    Float(f64),
    Integer(i64),
    String(String),
}
pub struct Evaluator {
    globals: Environment,
    errors: Vec<String>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            globals: Environment {
                values: HashMap::new(),
            },
            errors: vec![],
        }
    }

    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn evaluate(&mut self, ast: &AbstractSyntaxTree) -> Result<i64> {
        for statement in ast.statements() {
            match self.evaluate_statement(statement) {
                None => (),
                Some(rc) => return Ok(rc),
            }
        }
        Ok(0)
    }

    fn evaluate_statement(&mut self, statement: &Statement) -> Option<i64> {
        match &statement {
            Statement::Let(let_statement) => match &*let_statement.expression {
                Expression::StringLiteral(string_literal) => {
                    self.globals.values.insert(
                        let_statement.identifier.clone(),
                        Value::String(string_literal.value.clone(),
                        ),
                    );
                }
                Expression::FloatingPoint(float) => {
                    self.globals.values.insert(
                        let_statement.identifier.clone(),
                        Value::Float(float.value),
                    );
                }
                Expression::Integer(integer) => {
                    self.globals.values.insert(
                        let_statement.identifier.clone(),
                        Value::Integer(integer.value),
                    );
                }
                _ => {
                    panic!(
                        "Unhandled expression kind in top-level let statement {:?}",
                        &*let_statement.expression
                    );
                }
            },
            Statement::Return(_return_statement) => match &*_return_statement.expression {
                Expression::StringLiteral(_)
                | Expression::Function(_)
                | Expression::FloatingPoint(_) => {
                    panic!(
                        "Bad expression kind in top-level return statement {:?}",
                        &*_return_statement.expression
                    );
                }
                Expression::Integer(integer) => return Some(integer.value),
                _ => {
                    panic!(
                        "Unhandled expression kind in top-level return statement {:?}",
                        &*_return_statement.expression
                    );
                }
            },
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

    struct EvaluatorTestCase {
        input: &'static str,
        return_value: i64,
    }

    #[test]
    fn evaluate() {
        let test_cases = vec![
            EvaluatorTestCase {
                input: "return 42;",
                return_value: 42,
            },
            EvaluatorTestCase {
                input: r#"let a = "Hello";"#,
                return_value: 0,
            },
            EvaluatorTestCase {
                input: "let a = 3.14159;",
                return_value: 0,
            },
            EvaluatorTestCase {
                input: "let a = 42;",
                return_value: 0,
            },
            EvaluatorTestCase {
                input: r#"let a = 42; let b = "Hello"; let c = 3.14159; return 7;"#,
                return_value: 7,
            },
        ];

        for test_case in test_cases {
            let tokens = Lexer::new(test_case.input).tokens();
            let parser = Parser::new(tokens);
            let ast = parser.ast();
            let errors = ast.errors();

            assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
            let mut evaluator = Evaluator::new();
            let return_value = evaluator.evaluate(&ast);

            match return_value {
                Ok(rc) => assert_eq!(rc, test_case.return_value),
                _ => panic!("Code failed to evaluate: {:?}", test_case.input),
            }
        }
    }
}

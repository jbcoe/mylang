use crate::parser::{Expression, Statement};
use std::collections::HashMap;

#[derive(Clone)]
pub enum Value {
    Float(f64),
    Integer(i32),
    String(String),
}
pub struct Frame {
    values: HashMap<String, Value>,
}

impl Frame {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn evaluate_body(&mut self, body: &[Statement]) -> Option<Value> {
        for statement in body {
            if let Some(v) = self.evaluate_statement(statement) {
                return Some(v);
            }
        }
        None
    }

    fn evaluate_statement(&mut self, statement: &Statement) -> Option<Value> {
        match &statement {
            Statement::Let(let_statement) => {
                match &*let_statement.expression {
                    Expression::StringLiteral(string_literal) => {
                        self.values.insert(
                            let_statement.identifier.clone(),
                            Value::String(string_literal.value.clone()),
                        );
                    }
                    Expression::FloatingPoint(float) => {
                        self.values
                            .insert(let_statement.identifier.clone(), Value::Float(float.value));
                    }
                    Expression::Integer(integer) => {
                        self.values.insert(
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
                }
                None
            }
            Statement::Return(return_statement) => match &*return_statement.expression {
                Expression::StringLiteral(s) => Some(Value::String(s.value.clone())),
                Expression::FloatingPoint(f) => Some(Value::Float(f.value)),
                Expression::Integer(i) => Some(Value::Integer(i.value)),
                Expression::Identifier(identifier) => {
                    if let Some(id) = self.values.get(&identifier.name) {
                        Some(id.clone())
                    } else {
                        panic!("Unknown identifier {:?}", *return_statement.expression)
                    }
                }
                _ => panic!(
                    "Unhandled expression kind in top-level return statement {:?}",
                    *return_statement.expression
                ),
            },
        }
    }
}

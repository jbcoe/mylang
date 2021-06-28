use crate::parser::{Expression, FunctionExpression, Statement};
use std::{collections::HashMap, rc::Rc};

pub(crate) enum Value<'a> {
    Float(f64),
    Integer(i32),
    String(String),
    Function(&'a FunctionExpression),
}
pub(crate) struct Frame<'a> {
    values: HashMap<String, Rc<Value<'a>>>,
}

impl<'a> Frame<'a> {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn evaluate_body(&mut self, body: &'a [Statement]) -> Option<Rc<Value<'a>>> {
        for statement in body {
            if let Some(v) = self.evaluate_statement(statement) {
                return Some(v);
            }
        }
        None
    }

    fn evaluate_statement(&mut self, statement: &'a Statement) -> Option<Rc<Value<'a>>> {
        match &statement {
            Statement::Let(let_statement) => {
                match &*let_statement.expression {
                    Expression::StringLiteral(string_literal) => {
                        self.values.insert(
                            let_statement.identifier.clone(),
                            Rc::new(Value::String(string_literal.value.clone())),
                        );
                    }
                    Expression::FloatingPoint(float) => {
                        self.values.insert(
                            let_statement.identifier.clone(),
                            Rc::new(Value::Float(float.value)),
                        );
                    }
                    Expression::Integer(integer) => {
                        self.values.insert(
                            let_statement.identifier.clone(),
                            Rc::new(Value::Integer(integer.value)),
                        );
                    }
                    Expression::Function(function) => {
                        self.values.insert(
                            let_statement.identifier.clone(),
                            Rc::new(Value::Function(function)),
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
                Expression::StringLiteral(s) => Some(Rc::new(Value::String(s.value.clone()))),
                Expression::FloatingPoint(f) => Some(Rc::new(Value::Float(f.value))),
                Expression::Integer(i) => Some(Rc::new(Value::Integer(i.value))),
                Expression::Identifier(identifier) => {
                    if let Some(value) = self.values.get(&identifier.name) {
                        Some(Rc::clone(value))
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

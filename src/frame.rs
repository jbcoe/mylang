use crate::ast::{Expression, FunctionExpression, Statement};
use std::{collections::HashMap, rc::Rc};

pub enum Value<'a> {
    Float(f64),
    Integer(i32),
    String(String),
    Function(&'a FunctionExpression),
}
pub struct Frame<'a> {
    values: HashMap<String, Rc<Value<'a>>>,
}

impl<'a> Frame<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn evaluate_body(&mut self, body: &'a [Statement]) -> Option<Rc<Value<'a>>> {
        for statement in body {
            if let Some(v) = self.evaluate_statement(statement) {
                return Some(v);
            }
        }
        None
    }

    fn evaluate_expression(&mut self, expression: &'a Expression) -> Rc<Value<'a>> {
        match expression {
            Expression::StringLiteral(s) => Rc::new(Value::String(s.value.clone())),
            Expression::FloatingPoint(f) => Rc::new(Value::Float(f.value)),
            Expression::Integer(i) => Rc::new(Value::Integer(i.value)),
            Expression::Function(function) => Rc::new(Value::Function(function)),
            Expression::Identifier(identifier) => {
                if let Some(value) = self.values.get(&identifier.name) {
                    Rc::clone(value)
                } else {
                    panic!("Unknown identifier {:?}", expression)
                }
            }
            Expression::FunctionCall(call) => {
                if let Some(value) = self.values.get(&call.name) {
                    match **value {
                        Value::Float(_) => panic!("Cannot call a float"),
                        Value::Integer(_) => panic!("Cannot call an int"),
                        Value::String(_) => panic!("Cannot call a string"),
                        Value::Function(function) => {
                            if function.arguments.len() != call.arguments.len() {
                                panic!("Mismatch in argument count for function {}", call.name);
                            }
                            let mut function_frame = Frame::new();
                            for (arg_name, arg_expression) in
                                function.arguments.iter().zip(call.arguments.iter())
                            {
                                let argument_value = self.evaluate_expression(arg_expression);
                                function_frame
                                    .values
                                    .insert(arg_name.clone(), argument_value);
                            }
                            if let Some(return_value) = function_frame.evaluate_body(&function.body)
                            {
                                Rc::clone(&return_value)
                            } else {
                                panic!("Function did not return a value")
                            }
                        }
                    }
                } else {
                    panic!("Unknown identifier {:?}", expression)
                }
            }
            Expression::UnaryPlus(_) | Expression::UnaryMinus(_) => todo!(),
        }
    }

    fn evaluate_statement(&mut self, statement: &'a Statement) -> Option<Rc<Value<'a>>> {
        match &statement {
            Statement::Let(let_statement) => {
                let value = self.evaluate_expression(&let_statement.expression);
                self.values.insert(let_statement.identifier.clone(), value);
                None
            }
            Statement::Return(return_statement) => {
                let value = self.evaluate_expression(&return_statement.expression);
                Some(value)
            }
        }
    }
}

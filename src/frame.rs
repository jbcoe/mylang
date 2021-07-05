use crate::ast::{Expression, Function, Statement};
use std::{collections::HashMap, rc::Rc};

pub enum Value<'a> {
    Boolean(bool),
    Float(f64),
    Function(&'a Function),
    Integer(i32),
    String(String),
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
            Expression::Boolean(b) => Rc::new(Value::Boolean(*b)),
            Expression::FloatingPoint(f) => Rc::new(Value::Float(*f)),
            Expression::Function(function) => Rc::new(Value::Function(function)),
            Expression::Integer(i) => Rc::new(Value::Integer(*i)),
            Expression::StringLiteral(s) => Rc::new(Value::String(s.clone())),
            Expression::Identifier(identifier) => {
                if let Some(value) = self.values.get(identifier) {
                    Rc::clone(value)
                } else {
                    panic!("Unknown identifier {:?}", expression)
                }
            }
            Expression::Call(call) => {
                if let Some(value) = self.values.get(&call.name) {
                    match **value {
                        Value::Boolean(_) => panic!("Cannot call a boolean"),
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
            Statement::Return(return_statement) => Some(self.evaluate_expression(return_statement)),
            Statement::Expression(expression) => {
                let _value = self.evaluate_expression(expression);
                // TODO: Print _value to the console if we're in a REPL.
                None
            }
        }
    }
}

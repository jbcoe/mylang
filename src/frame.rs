use crate::ast::{Expression, Function, OpName, Statement, UnaryOp};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub enum Value {
    Boolean(bool),
    Float(f64),
    Function(Rc<Function>),
    Integer(i32),
    String(String),
}

pub struct Frame {
    values: HashMap<String, Rc<Value>>,
}

impl Frame {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn evaluate_body(&mut self, body: &[Statement]) -> Option<Rc<Value>> {
        for statement in body {
            if let Some(v) = self.evaluate_statement(statement) {
                return Some(v);
            }
        }
        None
    }

    fn evaluate_expression(&self, expression: &Expression) -> Rc<Value> {
        match expression {
            Expression::Boolean(b) => Rc::new(Value::Boolean(*b)),
            Expression::Float(f) => Rc::new(Value::Float(*f)),
            Expression::Function(function) => Rc::new(Value::Function(function.clone())),
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
                    match &**value {
                        Value::Boolean(_) => panic!("Cannot call a boolean"),
                        Value::Float(_) => panic!("Cannot call a float"),
                        Value::Integer(_) => panic!("Cannot call an integer"),
                        Value::String(_) => panic!("Cannot call a string"),
                        Value::Function(function) => {
                            if function.arguments.len() != call.arguments.len() {
                                panic!("Mismatch in argument count for function {}. Expected {} arguments but got {}", call.name, function.arguments.len(), call.arguments.len());
                            }
                            let mut function_frame = Self::new();
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
            Expression::UnaryOp(op) => self.evaluate_unary_op(op),
            Expression::BinaryOp(op) => todo!("Binary op is currently unsupported {:?}", op),
        }
    }

    fn evaluate_statement(&mut self, statement: &Statement) -> Option<Rc<Value>> {
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

    fn evaluate_unary_op(&self, op: &UnaryOp) -> Rc<Value> {
        let target = self.evaluate_expression(&op.target);
        match *target {
            Value::Boolean(_) => panic!("Cannot apply a unary operation to a Boolean"),
            Value::Float(f) => match op.operation {
                OpName::Plus => Rc::new(Value::Float(f)),
                OpName::Minus => Rc::new(Value::Float(-f)),
                OpName::Multiply | OpName::Divide => panic!(
                    "Cannot apply the unary operation {:#?} to the value {:#?} of type Float",
                    op.operation, f
                ),
            },
            Value::Function(_) => panic!("Cannot apply a unary operation to a function definition"),
            Value::Integer(i) => match op.operation {
                OpName::Plus => Rc::new(Value::Integer(i)),
                OpName::Minus => Rc::new(Value::Integer(-i)),
                OpName::Multiply | OpName::Divide => panic!(
                    "Cannot apply the unary operation {:#?} to the value {:#?} of type Integer",
                    op.operation, i
                ),
            },
            Value::String(_) => panic!("Cannot apply a unary operation to a String"),
        }
    }
}

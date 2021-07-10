use crate::ast::{Expression, Function, OpName, Statement, UnaryOp};
use std::{collections::HashMap, fmt, rc::Rc};
use thiserror::Error;

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Float(f64),
    Function(Rc<Function>),
    Integer(i32),
    String(Rc<String>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Float(float) => write!(f, "Float({})", float),
            Value::Function(_) => write!(f, "Function()"),
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::String(s) => write!(f, "String({})", s),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum EvaluationError {
    #[error("Evaluation requested on unknown identifier `{0}`")]
    UnknownIdentifier(String),
    #[error("Called on {0}, which is not a callable type")]
    NonCallableType(String),
    #[error("Mismatch in argument count for Function {name:?}. Expected {expected:?} arguments but got {got:?}")]
    ArgumentCountMismatch {
        name: String,
        expected: usize,
        got: usize,
    },
    #[error("Function {0} didn't return a value")]
    NoReturnValue(String),
    #[error("Can't apply a unary operation {opname:?} to {value:?}")]
    IllegalUnaryOperation { opname: String, value: String },
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

    pub(crate) fn evaluate_body(
        &mut self,
        body: &[Statement],
    ) -> Result<Option<Rc<Value>>, EvaluationError> {
        for statement in body {
            if let Some(v) = self.evaluate_statement(statement)? {
                return Ok(Some(v));
            }
        }
        Ok(None)
    }

    fn evaluate_expression(&self, expression: &Expression) -> Result<Rc<Value>, EvaluationError> {
        match expression {
            Expression::Boolean(b) => Ok(Rc::new(Value::Boolean(*b))),
            Expression::Float(f) => Ok(Rc::new(Value::Float(*f))),
            Expression::Function(function) => Ok(Rc::new(Value::Function(function.clone()))),
            Expression::Integer(i) => Ok(Rc::new(Value::Integer(*i))),
            Expression::StringLiteral(s) => Ok(Rc::new(Value::String(s.clone()))),
            Expression::Identifier(identifier) => self.values.get(identifier).map_or_else(
                || Err(EvaluationError::UnknownIdentifier(identifier.to_string())),
                |value| Ok(Rc::clone(value)),
            ),
            Expression::Call(call) => {
                if let Some(value) = self.values.get(&call.name) {
                    match &**value {
                        Value::Boolean(_)
                        | Value::Float(_)
                        | Value::Integer(_)
                        | Value::String(_) => {
                            Err(EvaluationError::NonCallableType((**value).to_string()))
                        }
                        Value::Function(function) => {
                            if function.arguments.len() == call.arguments.len() {
                                let mut function_frame = Self::new();
                                for (arg_name, arg_expression) in
                                    function.arguments.iter().zip(call.arguments.iter())
                                {
                                    let argument_value =
                                        self.evaluate_expression(arg_expression)?;
                                    function_frame
                                        .values
                                        .insert(arg_name.clone(), argument_value);
                                }
                                (function_frame.evaluate_body(&function.body)?).map_or_else(
                                    || Err(EvaluationError::NoReturnValue(call.name.clone())),
                                    |return_value| Ok(Rc::clone(&return_value)),
                                )
                            } else {
                                Err(EvaluationError::ArgumentCountMismatch {
                                    name: call.name.clone(),
                                    expected: function.arguments.len(),
                                    got: call.arguments.len(),
                                })
                            }
                        }
                    }
                } else {
                    Err(EvaluationError::UnknownIdentifier(call.name.clone()))
                }
            }
            Expression::UnaryOp(op) => self.evaluate_unary_op(op),
            Expression::BinaryOp(op) => todo!("Binary op is currently unsupported {:?}", op),
        }
    }

    fn evaluate_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<Option<Rc<Value>>, EvaluationError> {
        match &statement {
            Statement::Let(let_statement) => {
                let value = self.evaluate_expression(&let_statement.expression)?;
                self.values.insert(let_statement.identifier.clone(), value);
                Ok(None)
            }
            Statement::Return(return_statement) => {
                Ok(Some(self.evaluate_expression(return_statement)?))
            }
            Statement::Expression(expression) => {
                let _value = self.evaluate_expression(expression);
                // TODO: Print _value to the console if we're in a REPL.
                Ok(None)
            }
        }
    }

    fn evaluate_unary_op(&self, op: &UnaryOp) -> Result<Rc<Value>, EvaluationError> {
        let target = self.evaluate_expression(&op.target)?;
        match (&*target, &op.operation) {
            (Value::Float(_) | Value::Integer(_), OpName::Plus) => Ok(target.clone()),
            (Value::Float(f), OpName::Minus) => Ok(Rc::new(Value::Float(-f))),
            (Value::Integer(i), OpName::Minus) => Ok(Rc::new(Value::Integer(-i))),
            (
                Value::Boolean(_)
                | Value::Function(_)
                | Value::String(_)
                | Value::Float(_)
                | Value::Integer(_),
                _,
            ) => Err(EvaluationError::IllegalUnaryOperation {
                opname: op.operation.to_string(),
                value: (*target).to_string(),
            }),
        }
    }
}

use crate::ast::AbstractSyntaxTree;
use crate::frame::{Frame, Value};

pub struct Evaluator<'a> {
    global: Frame<'a>,
    errors: Vec<String>,
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Evaluator {
            global: Frame::new(),
            errors: vec![],
        }
    }

    pub const fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn evaluate(&mut self, ast: &'a AbstractSyntaxTree) -> i32 {
        // Evaluate statements at global scope until one of them returns.
        if let Some(rc) = self.global.evaluate_body(ast.statements()) {
            match *rc {
                Value::Integer(i) => i,
                _ => panic!("Non-integer return type at global scope"),
            }
        } else {
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

    struct EvaluatorTestCase {
        input: &'static str,
        return_value: i32,
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
                input: "let a = 42; return a;",
                return_value: 42,
            },
            EvaluatorTestCase {
                input: "let a = func ( x, y ) { return x; };",
                return_value: 0,
            },
            EvaluatorTestCase {
                input: r#"
let first = func (a, b) { 
    return a; 
};
let x = 1;
let y = 2;
let z = first(x, y);
return z;"#,
                return_value: 1,
            },
            EvaluatorTestCase {
                input: r#"
let first = func (a, b) { 
    return a; 
};
return first(101, 2);"#,
                return_value: 101,
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
            assert_eq!(evaluator.evaluate(&ast), test_case.return_value);
        }
    }
}

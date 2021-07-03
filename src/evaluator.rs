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

    macro_rules! evaluator_test_case {
        ($test_name:ident, $test_case:expr) => {
            #[test]
            fn $test_name() {
                let tokens = Lexer::new($test_case.input).tokens();
                let parser = Parser::new(tokens);
                let ast = parser.ast();
                let errors = ast.errors();

                assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
                let mut evaluator = Evaluator::new();
                assert_eq!(evaluator.evaluate(&ast), $test_case.return_value);
            }
        };
    }

    evaluator_test_case![
        return_integer,
        EvaluatorTestCase {
            input: "return 42;",
            return_value: 42,
        }
    ];

    evaluator_test_case![
        let_string_no_return,
        EvaluatorTestCase {
            input: r#"let a = "Hello";"#,
            return_value: 0,
        }
    ];

    evaluator_test_case![
        let_float_no_return,
        EvaluatorTestCase {
            input: "let a = 3.14159;",
            return_value: 0,
        }
    ];

    evaluator_test_case![
        let_integer_no_return,
        EvaluatorTestCase {
            input: "let a = 42;",
            return_value: 0,
        }
    ];

    evaluator_test_case![
        let_integer_return_identifier,
        EvaluatorTestCase {
            input: "let a = 42; return a;",
            return_value: 42,
        }
    ];

    evaluator_test_case![
        let_function_no_return,
        EvaluatorTestCase {
            input: "let a = func ( x, y ) { return x; };",
            return_value: 0,
        }
    ];

    evaluator_test_case![
        let_function_return_invocation_with_identifiers,
        EvaluatorTestCase {
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
    ];

    evaluator_test_case![
        let_function_return_invocation_with_literals,
        EvaluatorTestCase {
            input: r#"
let first = func (a, b) { 
    return a; 
};
return first(101, 2);"#,
            return_value: 101,
        }
    ];

    evaluator_test_case![
        assorted_let_with_return_integer,
        EvaluatorTestCase {
            input: r#"let a = 42; let b = "Hello"; let c = 3.14159; return 7;"#,
            return_value: 7,
        }
    ];

    evaluator_test_case![
        assorted_let_with_return_identifier,
        EvaluatorTestCase {
            input: r#"let a = 42; a;"#,
            return_value: 0, // not 42
        }
    ];
}

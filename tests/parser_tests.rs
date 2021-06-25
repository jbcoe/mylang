mod parser_test {
    use mylang::Parser;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
    }

    #[test]
    fn parser_tests() {
        let test_cases = vec![
            TestCase {
                input: "let x = a;",
            },
            TestCase {
                input: "let x = 5;",
            },
            TestCase {
                input: "let x = 3.14159;",
            },
            TestCase {
                input: r#"let x = "Hello";"#,
            },
            TestCase {
                input: "let x = +1;",
            },
            TestCase {
                input: "let x = -1;",
            },
            TestCase {
                input: "let x = +3.14159;",
            },
            TestCase {
                input: "let x = -3.14159;",
            },
        ];

        for test_case in test_cases.iter() {
            let parser = Parser::new(test_case.input);
            let ast = parser.ast();
            assert!(ast.errors().is_empty());
        }
    }
}

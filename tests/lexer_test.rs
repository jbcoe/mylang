mod lexer_test {
    use mylang::Kind;
    use mylang::Lexer;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
        skip_whitespace: bool,
        expected_tokens: Vec<(&'static str, Kind)>,
    }

    #[test]
    fn lexer_tests() {
        let test_cases = vec![
            TestCase {
                input: "let myPet  =  10dog01 ;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    ("  ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    ("  ", Kind::Whitespace),
                    ("10dog01", Kind::Unknown),
                    (" ", Kind::Whitespace),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let myPet = "Timmy the dog";"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    (r#""Timmy the dog""#, Kind::String),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let mut myPet = "Timmy the dog just goes on forever"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("mut", Kind::Mut),
                    (" ", Kind::Whitespace),
                    ("myPet", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    // Note the unbalanced quotes in the string below.
                    (r#""Timmy the dog just goes on forever"#, Kind::Unknown),
                ],
            },
            TestCase {
                input: "let myNumber = 1001;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myNumber", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("1001", Kind::Integer),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let myValue = anotherValue;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("myValue", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("anotherValue", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let value = value0;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    (" ", Kind::Whitespace),
                    ("value", Kind::Identifier),
                    (" ", Kind::Whitespace),
                    ("=", Kind::EqualSign),
                    (" ", Kind::Whitespace),
                    ("value0", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let my_value = 10;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    ("my_value", Kind::Identifier),
                    ("=", Kind::EqualSign),
                    ("10", Kind::Integer),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "let add = func (lhs, rhs) { return lhs + rhs; };",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", Kind::Let),
                    ("add", Kind::Identifier),
                    ("=", Kind::EqualSign),
                    ("func", Kind::Function),
                    ("(", Kind::LeftParen),
                    ("lhs", Kind::Identifier),
                    (",", Kind::Comma),
                    ("rhs", Kind::Identifier),
                    (")", Kind::RightParen),
                    ("{", Kind::LeftBrace),
                    ("return", Kind::Return),
                    ("lhs", Kind::Identifier),
                    ("+", Kind::Plus),
                    ("rhs", Kind::Identifier),
                    (";", Kind::SemiColon),
                    ("}", Kind::RightBrace),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a > b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    (">", Kind::Greater),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a < b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("<", Kind::Less),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a >= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    (">=", Kind::GreaterOrEqual),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a <= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("<=", Kind::LessOrEqual),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a == b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("==", Kind::DoubleEquals),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "a != b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", Kind::Identifier),
                    ("!=", Kind::NotEquals),
                    ("b", Kind::Identifier),
                    (";", Kind::SemiColon),
                ],
            },
            TestCase {
                input: "3",
                skip_whitespace: false,
                expected_tokens: vec![("3", Kind::Integer)],
            },
            TestCase {
                input: "314",
                skip_whitespace: false,
                expected_tokens: vec![("314", Kind::Integer)],
            },
            TestCase {
                input: "3.14",
                skip_whitespace: false,
                expected_tokens: vec![("3.14", Kind::FloatingPoint)],
            },
            TestCase {
                input: "3.",
                skip_whitespace: false,
                expected_tokens: vec![("3.", Kind::FloatingPoint)],
            },
            TestCase {
                input: ".14",
                skip_whitespace: false,
                expected_tokens: vec![(".14", Kind::FloatingPoint)],
            },
            TestCase {
                input: "3e8",
                skip_whitespace: false,
                expected_tokens: vec![("3e8", Kind::FloatingPoint)],
            },
            TestCase {
                input: "0.314e1",
                skip_whitespace: false,
                expected_tokens: vec![("0.314e1", Kind::FloatingPoint)],
            },
            TestCase {
                input: "9.1e-31",
                skip_whitespace: false,
                expected_tokens: vec![("9.1e-31", Kind::FloatingPoint)],
            },
            TestCase {
                input: "6.02e+23",
                skip_whitespace: false,
                expected_tokens: vec![("6.02e+23", Kind::FloatingPoint)],
            },
            TestCase {
                input: "2e",
                skip_whitespace: false,
                expected_tokens: vec![("2e", Kind::Unknown)],
            },
            TestCase {
                input: "2.e",
                skip_whitespace: false,
                expected_tokens: vec![("2.e", Kind::Unknown)],
            },
            TestCase {
                input: "2.4f",
                skip_whitespace: false,
                expected_tokens: vec![("2.4f", Kind::Unknown)],
            },
            TestCase {
                input: "2.4e3a",
                skip_whitespace: false,
                expected_tokens: vec![("2.4e3a", Kind::Unknown)],
            },
            TestCase {
                input: "123f+4.2e-3",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("123f", Kind::Unknown),
                    ("+", Kind::Plus),
                    ("4.2e-3", Kind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4.56",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4.56", Kind::Unknown)],
            },
            TestCase {
                input: "1.23e-4+3.2e-5",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("1.23e-4", Kind::FloatingPoint),
                    ("+", Kind::Plus),
                    ("3.2e-5", Kind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4e-3.2",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4e-3.2", Kind::Unknown)],
            },
        ];

        for test_case in test_cases.iter() {
            let mut lexer = Lexer::new(test_case.input);
            for expected_token in test_case.expected_tokens.iter() {
                let mut t = lexer.next_token();
                while t.kind() == Kind::Whitespace && test_case.skip_whitespace {
                    t = lexer.next_token();
                }
                assert_eq!(t.text(), expected_token.0);
                assert_eq!(t.kind(), expected_token.1);
            }
            assert_eq!(lexer.next_token().kind(), Kind::EndOfFile);
        }
    }
}

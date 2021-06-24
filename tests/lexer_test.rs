mod lexer_test {
    use mylang::Lexer;
    use mylang::TokenKind;

    #[derive(Debug)]
    struct TestCase {
        input: &'static str,
        skip_whitespace: bool,
        expected_tokens: Vec<(&'static str, TokenKind)>,
    }

    #[test]
    fn lexer_tests() {
        let test_cases = vec![
            TestCase {
                input: "let myPet  =  10dog01 ;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    ("  ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    ("  ", TokenKind::Whitespace),
                    ("10dog01", TokenKind::Unknown),
                    (" ", TokenKind::Whitespace),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let myPet = "Timmy the dog";"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    (r#""Timmy the dog""#, TokenKind::String),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: r#"let mut myPet = "Timmy the dog just goes on forever"#,
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("mut", TokenKind::Mut),
                    (" ", TokenKind::Whitespace),
                    ("myPet", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    // Note the unbalanced quotes in the string below.
                    (r#""Timmy the dog just goes on forever"#, TokenKind::Unknown),
                ],
            },
            TestCase {
                input: "let myNumber = 1001;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myNumber", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("1001", TokenKind::Integer),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let myValue = anotherValue;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("myValue", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("anotherValue", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let value = value0;",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    (" ", TokenKind::Whitespace),
                    ("value", TokenKind::Identifier),
                    (" ", TokenKind::Whitespace),
                    ("=", TokenKind::EqualSign),
                    (" ", TokenKind::Whitespace),
                    ("value0", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let my_value = 10;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    ("my_value", TokenKind::Identifier),
                    ("=", TokenKind::EqualSign),
                    ("10", TokenKind::Integer),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "let add = (lhs, rhs) { return lhs + rhs; };",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("let", TokenKind::Let),
                    ("add", TokenKind::Identifier),
                    ("=", TokenKind::EqualSign),
                    ("(", TokenKind::LeftParen),
                    ("lhs", TokenKind::Identifier),
                    (",", TokenKind::Comma),
                    ("rhs", TokenKind::Identifier),
                    (")", TokenKind::RightParen),
                    ("{", TokenKind::LeftBrace),
                    ("return", TokenKind::Return),
                    ("lhs", TokenKind::Identifier),
                    ("+", TokenKind::Plus),
                    ("rhs", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                    ("}", TokenKind::RightBrace),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a > b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    (">", TokenKind::Greater),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a < b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("<", TokenKind::Less),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a >= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    (">=", TokenKind::GreaterOrEqual),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a <= b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("<=", TokenKind::LessOrEqual),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a == b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("==", TokenKind::DoubleEquals),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "a != b;",
                skip_whitespace: true,
                expected_tokens: vec![
                    ("a", TokenKind::Identifier),
                    ("!=", TokenKind::NotEquals),
                    ("b", TokenKind::Identifier),
                    (";", TokenKind::SemiColon),
                ],
            },
            TestCase {
                input: "3",
                skip_whitespace: false,
                expected_tokens: vec![("3", TokenKind::Integer)],
            },
            TestCase {
                input: "314",
                skip_whitespace: false,
                expected_tokens: vec![("314", TokenKind::Integer)],
            },
            TestCase {
                input: "3.14",
                skip_whitespace: false,
                expected_tokens: vec![("3.14", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "3.",
                skip_whitespace: false,
                expected_tokens: vec![("3.", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: ".14",
                skip_whitespace: false,
                expected_tokens: vec![(".14", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "3e8",
                skip_whitespace: false,
                expected_tokens: vec![("3e8", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "0.314e1",
                skip_whitespace: false,
                expected_tokens: vec![("0.314e1", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "9.1e-31",
                skip_whitespace: false,
                expected_tokens: vec![("9.1e-31", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "6.02e+23",
                skip_whitespace: false,
                expected_tokens: vec![("6.02e+23", TokenKind::FloatingPoint)],
            },
            TestCase {
                input: "2e",
                skip_whitespace: false,
                expected_tokens: vec![("2e", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.e",
                skip_whitespace: false,
                expected_tokens: vec![("2.e", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.4f",
                skip_whitespace: false,
                expected_tokens: vec![("2.4f", TokenKind::Unknown)],
            },
            TestCase {
                input: "2.4e3a",
                skip_whitespace: false,
                expected_tokens: vec![("2.4e3a", TokenKind::Unknown)],
            },
            TestCase {
                input: "123f+4.2e-3",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("123f", TokenKind::Unknown),
                    ("+", TokenKind::Plus),
                    ("4.2e-3", TokenKind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4.56",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4.56", TokenKind::Unknown)],
            },
            TestCase {
                input: "1.23e-4+3.2e-5",
                skip_whitespace: false,
                expected_tokens: vec![
                    ("1.23e-4", TokenKind::FloatingPoint),
                    ("+", TokenKind::Plus),
                    ("3.2e-5", TokenKind::FloatingPoint),
                ],
            },
            TestCase {
                input: "1.23e-4e-3.2",
                skip_whitespace: false,
                expected_tokens: vec![("1.23e-4e-3.2", TokenKind::Unknown)],
            },
        ];

        for test_case in test_cases.iter() {
            let mut lexer = Lexer::new(test_case.input);
            for expected_token in test_case.expected_tokens.iter() {
                let mut t = lexer.next_token();
                while t.kind() == TokenKind::Whitespace && test_case.skip_whitespace {
                    t = lexer.next_token();
                }
                assert_eq!(t.text(), expected_token.0);
                assert_eq!(t.kind(), expected_token.1);
            }
            assert_eq!(lexer.next_token().kind(), TokenKind::Eof);
        }
    }
}

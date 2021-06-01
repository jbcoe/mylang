use mylang::lexer::Lexer;
use mylang::lexer::TokenKind;
use std::env;
use std::fs;

#[derive(Debug)]
struct LexerError(String);

fn main() -> Result<(), LexerError> {
    let filepaths: Vec<String> = env::args().skip(1).collect();
    if filepaths.len() != 1 {
        return Err(LexerError(String::from(
            "A single filepath must be specified as a command line argument.",
        )));
    }
    for filepath in filepaths {
        let text = fs::read_to_string(&filepath)
            .map_err(|err| LexerError(format!("Error reading `{}`: {}", filepath, err)))?;
        let lexer = Lexer::new(&text);
        let tokens = lexer.tokens();
        for token in tokens {
            if token.kind() == TokenKind::Whitespace {
                continue;
            }
            println!(
                "{start}:{end} {kind:?} '{text}',",
                start = token.offset(),
                end = token.offset() + token.len(),
                kind = token.kind(),
                text = token.text(),
            );
        }
    }
    Ok(())
}

mod lexer;
mod token;

use lexer::Lexer;
use std::env;
use std::fs;
use token::TokenKind;

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
            let line = 1 + text[0..token.offset()].matches('\n').count();
            let column = if line > 1 {
                token.offset() - text[0..token.offset()].rfind('\n').unwrap()
            } else {
                token.offset() + 1
            };

            println!(
                "{line}:{column} {kind:?} '{text}',",
                line = line,
                column = column,
                kind = token.kind(),
                text = token.text(),
            );
        }
    }
    Ok(())
}

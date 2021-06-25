mod lexer;
mod token;

use lexer::Lexer;
use std::{env, fs};
use token::Kind;

#[derive(Debug)]
struct LexerError(String);

fn main() -> Result<(), LexerError> {
    let filepaths: Vec<String> = env::args().skip(1).collect();
    if filepaths.is_empty() {
        return Err(LexerError(String::from(
            "At least one filepath must be specified as a command line argument.",
        )));
    }
    for filepath in filepaths {
        let text = fs::read_to_string(&filepath)
            .map_err(|err| LexerError(format!("Error reading `{}`: {}", filepath, err)))?;
        process_text(&text);
    }
    Ok(())
}

/// process the input text, outputting a printable form of each token, one token per line
/// Line numbers and columns are shown
fn process_text(text: &str) {
    let lexer = Lexer::new(text);
    let tokens = lexer.tokens();
    for token in tokens {
        if token.kind() == Kind::Whitespace {
            continue;
        }
        let line = 1 + text[0..token.offset()].matches('\n').count();
        let column = if line > 1 {
            token.offset() - text[0..token.offset()].rfind('\n').unwrap()
        } else {
            token.offset() + 1
        };

        println!(
            "{line}:{column} {token},",
            line = line,
            column = column,
            token = token,
        );
    }
}

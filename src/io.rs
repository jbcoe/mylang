use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Kind;

use anyhow::{Context, Result};
use std::{
    fs::File,
    io::{self, Read, Write},
};

/// process first filepath in the input Vec, or stdin if empty
pub fn process_files(filepaths: Vec<String>) -> Result<i64> {
    let stdout = io::stdout();
    let mut outio = stdout.lock();

    if filepaths.is_empty() {
        process_stream(io::stdin(), &mut outio).with_context(|| "while reading stdin")
    } else {
        let filepath = &filepaths[0];
        let file = File::open(&filepath)
            .with_context(|| format!("Failed to open file at {}", filepath))?;
        process_stream(file, &mut outio).with_context(|| format!("while reading {}", filepath))
    }
}

/// process an input stream from inio, reading its text and writing the
/// processed results to outio
fn process_stream<R, W>(mut inio: R, outio: W) -> Result<i64>
where
    R: Read,
    W: Write,
{
    let mut text = String::new();
    inio.read_to_string(&mut text)?;

    process_text(&text, outio)
}

/// process an input text, writing a printable form of all tokens except
/// whitespace, one token per line onto out. Line numbers and columns are
/// written. Any AST errors are written
fn process_text<W: Write>(text: &str, mut out: W) -> Result<i64> {
    let lexer = Lexer::new(text);
    let tokens = lexer.tokens();
    for token in &tokens {
        if token.kind() == Kind::Whitespace {
            continue;
        }
        let line = 1 + text[0..token.offset()].matches('\n').count();
        let column = if line > 1 {
            token.offset() - text[0..token.offset()].rfind('\n').unwrap()
        } else {
            token.offset() + 1
        };

        writeln!(
            out,
            "{line}:{column} {token},",
            line = line,
            column = column,
            token = token,
        )?;
    }
    let parser = Parser::new(tokens);
    let ast = parser.ast();
    for err in ast.errors() {
        writeln!(out, "{}", err)?;
    }
    let mut evaluator = Evaluator::new();
    evaluator.evaluate(&ast)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    #[should_panic(expected = "stream did not contain valid UTF-8")]
    fn invalid_utf8_input_causes_a_panic() {
        let invalid_utf8: &[u8] = &[0x44, 0xEF];
        process_stream(invalid_utf8, io::sink()).unwrap();
    }

    #[test]
    #[should_panic(expected = "Failed to open file at /dev/null/madeupfile")]
    fn missing_file_causes_a_panic() {
        process_files(vec!["/dev/null/madeupfile".to_string()]).unwrap();
    }

    #[test]
    fn valid_utf8_input_doesnt_panic() {
        let valid_utf8: &[u8] = b"meow";
        process_stream(valid_utf8, io::sink()).unwrap();
    }
}

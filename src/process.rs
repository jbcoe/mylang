use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Kind;

use anyhow::{Context, Result};
use std::{
    fs::File,
    io::{self, Read, Write},
};

/// process each filepath in the input Vec, or stdin if empty
pub fn files(filepaths: Vec<String>) -> Result<()> {
    let stdout = io::stdout();
    let mut outio = stdout.lock();

    if filepaths.is_empty() {
        process_stream(io::stdin(), &mut outio).with_context(|| "while reading stdin")?;
    } else {
        for filepath in filepaths {
            let file = File::open(&filepath)
                .with_context(|| format!("Failed to open file at {}", filepath))?;
            process_stream(file, &mut outio)
                .with_context(|| format!("while reading {}", filepath))?;
        }
    }
    Ok(())
}

/// process an input stream from inio, reading its text and writing the
/// processed results to outio
fn process_stream<R, W>(mut inio: R, outio: W) -> Result<()>
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
fn process_text<W: Write>(text: &str, mut out: W) -> Result<()> {
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
    for err in parser.ast().errors() {
        writeln!(out, "{}", err)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    #[should_panic(expected = "stream did not contain valid UTF-8")]
    fn bad_unicode_input_causes_a_panic() {
        let bad_unicode: &[u8] = &[0x44, 0xEF];
        process_stream(bad_unicode, io::sink()).unwrap();
    }

    #[test]
    #[should_panic(expected = "Failed to open file at")]
    fn missing_file_causes_a_panic() {
        files(vec!["madeupfilename".to_string()]).unwrap();
    }

    #[test]
    fn valid_utf8_input_doesnt_panic() {
        let good_input = "meow".as_bytes();
        process_stream(good_input, io::sink()).unwrap();
    }
}

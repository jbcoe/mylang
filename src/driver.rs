use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Kind;

use anyhow::{Context, Result};
use std::{
    fs::File,
    io::{self, Read, Write},
};

use std::path::PathBuf;
use structopt::StructOpt;

/// A lexer, parser, interpreter, JIT and AOT compiler and
/// runtime written in Rust for a new (simple) language
#[derive(StructOpt, Debug)]
#[structopt(name = "mylang")]
pub struct Opt {
    /// Files to process
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

/// process first filepath in Files, or stdin if empty
///
/// # Errors
///
/// Will return Err if files couldn't be opened, or found
/// or if they could not be lexed, parsed or evaluated
pub fn go(opt: &Opt) -> Result<i32> {
    let stdout = io::stdout();
    let mut outio = stdout.lock();

    if opt.files.is_empty() {
        process_stream(io::stdin(), &mut outio).with_context(|| "while reading stdin")
    } else {
        let filepath = &opt.files[0];
        let file = File::open(&filepath)
            .with_context(|| format!("Failed to open file at {}", filepath.display()))?;
        process_stream(file, &mut outio)
            .with_context(|| format!("while reading {}", filepath.display()))
    }
}

/// process an input stream from inio, reading its text and writing the
/// processed results to outio
fn process_stream<R, W>(mut inio: R, outio: W) -> Result<i32>
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
fn process_text<W: Write>(text: &str, mut out: W) -> Result<i32> {
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
    let result = evaluator.evaluate(&ast);
    for err in evaluator.errors() {
        writeln!(out, "{}", err)?;
    }
    Ok(result)
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
        let opts = Opt {
            files: vec![PathBuf::from(r"/dev/null/madeupfile")],
        };
        go(&opts).unwrap();
    }

    #[test]
    fn valid_utf8_input_doesnt_panic() {
        let valid_utf8: &[u8] = b"let meow; meow";
        process_stream(valid_utf8, io::sink()).unwrap();
    }
}

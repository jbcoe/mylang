use crate::ast::AbstractSyntaxTree;
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
    /// Use alternate parser
    #[structopt(short, long)]
    alternate: bool,

    /// Parse only (skip evaluation)
    #[structopt(short, long)]
    parse_only: bool,

    /// Filename to process. Omit to process standard input
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}

/// process the input filename, or standard input if empty
///
/// # Errors
///
/// Returns the result of running the evaluator on the text
///
/// Returns Err if the input (file) couldn't be opened, or found
/// or if it could not be lexed, parsed or evaluated
pub fn real_main(opt: &Opt) -> Result<i32> {
    let stdout = io::stdout();
    let mut outio = stdout.lock();

    match &opt.file {
        Some(filepath) => {
            let file = File::open(&filepath)
                .with_context(|| format!("Failed to open file at {}", filepath.display()))?;
            process_stream(file, &mut outio, opt)
                .with_context(|| format!("while reading {}", filepath.display()))
        }
        None => process_stream(io::stdin(), &mut outio, opt).with_context(|| "while reading stdin"),
    }
}

/// process an input stream from inio, reading its text and writing the
/// processed results to outio
fn process_stream<R, W>(mut inio: R, outio: W, opt: &Opt) -> Result<i32>
where
    R: Read,
    W: Write,
{
    let mut text = String::new();
    inio.read_to_string(&mut text)?;

    process_text(outio, &text, opt)
}

/// process an input text, writing a printable form of all tokens except
/// whitespace, one token per line onto out. Line numbers and columns are
/// written. Any AST errors are written
fn process_text<W: Write>(mut out: W, text: &str, options: &Opt) -> Result<i32> {
    let ast = if options.alternate {
        alternate_lex_parse(&mut out, text)
    } else {
        lex_and_parse(&mut out, text)?
    };
    let result = if options.parse_only {
        0
    } else {
        evaluate(out, &ast)?
    };
    Ok(result)
}

fn evaluate<W: Write>(mut out: W, ast: &AbstractSyntaxTree) -> Result<i32, anyhow::Error> {
    let mut evaluator = Evaluator::new();
    let result = evaluator.evaluate(ast);
    for err in evaluator.errors() {
        writeln!(out, "{}", err)?;
    }
    Ok(result)
}

fn alternate_lex_parse<W: Write>(mut _out: W, _text: &str) -> AbstractSyntaxTree {
    AbstractSyntaxTree::new(vec![], vec![])
}

fn lex_and_parse<W: Write>(mut out: W, text: &str) -> Result<AbstractSyntaxTree, anyhow::Error> {
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
    Ok(ast)
}

#[cfg(test)]
mod tests {

    use structopt::lazy_static::lazy_static;

    use super::*;

    lazy_static! {
        static ref OPTS: Opt = Opt::from_iter(vec!["mylang"]);
    }

    #[test]
    #[should_panic(expected = "stream did not contain valid UTF-8")]
    fn invalid_utf8_input_causes_a_panic() {
        let invalid_utf8: &[u8] = &[0x44, 0xEF];
        process_stream(invalid_utf8, io::sink(), &OPTS).unwrap();
    }

    #[test]
    #[should_panic(expected = "Failed to open file at /dev/null/madeupfile")]
    fn unopenable_file_causes_a_panic() {
        let opts: Opt = Opt::from_iter(vec!["mylang", "/dev/null/madeupfile"]);
        real_main(&opts).unwrap();
    }

    #[test]
    fn valid_utf8_input_doesnt_panic() {
        let valid_utf8: &[u8] = b"let meow; meow";
        process_stream(valid_utf8, io::sink(), &OPTS).unwrap();
    }
}

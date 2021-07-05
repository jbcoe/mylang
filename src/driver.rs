use crate::ast::AbstractSyntaxTree;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::{Kind, Token};

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
    /// Use alternate parser (not yet implemented)
    #[structopt(short, long)]
    alternate: bool,

    /// Parse only (skip evaluation)
    #[structopt(short, long)]
    parse_only: bool,

    /// Write out tokens
    #[structopt(short, long)]
    tokens: bool,

    /// Filename to process. Omit to process standard input
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}

pub struct Driver<'a> {
    opt: &'a Opt,
}

/// A Driver for all phases from tokenisation through to evaluation
impl<'a> Driver<'a> {
    #[must_use]
    pub const fn new(opt: &'a Opt) -> Self {
        Driver { opt }
    }

    /// process the input filename, or standard input if empty
    ///
    /// # Errors
    ///
    /// Returns the result of running the evaluator on the text
    ///
    /// Returns Err if the input (file) couldn't be opened, or found
    /// or if it could not be lexed, parsed or evaluated
    pub fn main(&self) -> Result<i32> {
        let stdout = io::stdout();
        let mut outio = stdout.lock();

        match &self.opt.file {
            Some(filepath) => {
                let file = File::open(&filepath)
                    .with_context(|| format!("Failed to open file at {}", filepath.display()))?;
                self.process_stream(file, &mut outio)
                    .with_context(|| format!("while reading {}", filepath.display()))
            }
            None => self
                .process_stream(io::stdin(), &mut outio)
                .with_context(|| "while reading stdin"),
        }
    }

    /// process an input stream from inio, reading its text and writing the
    /// processed results to outio
    fn process_stream<R, W>(&self, mut inio: R, outio: W) -> Result<i32>
    where
        R: Read,
        W: Write,
    {
        let mut text = String::new();
        inio.read_to_string(&mut text)?;

        self.process_text(outio, &text)
    }

    /// process an input text, writing a printable form of all tokens except
    /// whitespace, one token per line onto out. Line numbers and columns are
    /// written. Any AST errors are written
    fn process_text<W: Write>(&self, mut out: W, text: &str) -> Result<i32> {
        let ast = if self.opt.alternate {
            alternate_lex_parse(&mut out, text)
        } else {
            self.lex_and_parse(&mut out, text)?
        };
        let result = if self.opt.parse_only {
            0
        } else {
            evaluate(out, &ast)?
        };
        Ok(result)
    }

    fn lex_and_parse<W: Write>(
        &self,
        mut out: W,
        text: &str,
    ) -> Result<AbstractSyntaxTree, anyhow::Error> {
        let lexer = Lexer::new(text);
        let tokens = lexer.tokens();
        if self.opt.tokens {
            write_tokens(&mut out, &tokens, text)?;
        }
        let parser = Parser::new(tokens);
        let ast = parser.ast();
        for err in ast.errors() {
            writeln!(out, "{}", err)?;
        }
        Ok(ast)
    }
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

fn write_tokens<W: Write>(mut out: W, tokens: &[Token], text: &str) -> Result<(), anyhow::Error> {
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

        writeln!(
            out,
            "{line}:{column} {token},",
            line = line,
            column = column,
            token = token,
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {

    use structopt::lazy_static::lazy_static;

    use super::*;

    lazy_static! {
        static ref OPTS: Opt = Opt::from_iter(vec!["mylang"]);
        static ref DRIVER: Driver<'static> = Driver::new(&OPTS);
    }

    #[test]
    #[should_panic(expected = "stream did not contain valid UTF-8")]
    fn invalid_utf8_input_causes_a_panic() {
        let invalid_utf8: &[u8] = &[0x44, 0xEF];
        DRIVER.process_stream(invalid_utf8, io::sink()).unwrap();
    }

    #[test]
    #[should_panic(expected = "Failed to open file at /dev/null/madeupfile")]
    fn unopenable_file_causes_a_panic() {
        let opts: Opt = Opt::from_iter(vec!["mylang", "/dev/null/madeupfile"]);
        let driver = Driver::new(&opts);
        driver.main().unwrap();
    }

    #[test]
    fn valid_utf8_input_doesnt_panic() {
        let valid_utf8: &[u8] = b"let meow; meow";
        DRIVER.process_stream(valid_utf8, io::sink()).unwrap();
    }
}

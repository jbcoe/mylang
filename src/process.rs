use crate::lexer::Lexer;
use crate::token::Kind;

use anyhow::{bail, Context, Result};
use std::{fs, io, io::Write};

/// process each filepath in the input Vec
pub fn files(filepaths: Vec<String>) -> Result<()> {
    if filepaths.is_empty() {
        bail!("At least one filepath must be specified as a command line argument.");
    }
    
    for filepath in filepaths {
        let text = fs::read_to_string(&filepath)
            .with_context(|| format!("Failed to read from the file at {}", filepath))?;
        process_text(&text, &mut io::stdout().lock())?;
    }
    Ok(())
}

/// process the input text, writing a printable form of each token,
/// one token per line onto out. Line numbers and columns are written
fn process_text<W: Write>(text: &str, mut out: W) -> Result<()> {
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

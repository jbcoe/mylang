mod lexer;
mod parser;
mod process;
mod token;

use anyhow::Result;
use process::files;
use std::env;

fn main() -> Result<()> {
    files(env::args().skip(1).collect())
}

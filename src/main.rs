mod evaluator;
mod io;
mod lexer;
mod parser;
mod token;

use anyhow::Result;
use io::process_files;
use std::env;

fn main() -> Result<()> {
    process_files(env::args().skip(1).collect())
}

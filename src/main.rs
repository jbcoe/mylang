mod evaluator;
mod io;
mod lexer;
mod parser;
mod token;

use anyhow::Result;
use io::process_files;
use std::{env, process};

fn main() -> Result<()> {
    process::exit(process_files(env::args().skip(1).collect())? as i32)
}

use anyhow::Result;
use mylang::io::process_files;
use std::{env, process};

fn main() -> Result<()> {
    process::exit(process_files(env::args().skip(1).collect())?)
}

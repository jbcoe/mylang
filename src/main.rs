use anyhow::Result;
use std::{env, process};
use mylang::io::process_files;

fn main() -> Result<()> {
    process::exit(process_files(env::args().skip(1).collect())? as i32)
}

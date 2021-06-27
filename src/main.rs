use anyhow::Result;
use mylang::io::{process_files, Opt};
use std::process;
use structopt::StructOpt;

fn main() -> Result<()> {
    let opt = Opt::from_args();
    #[allow(clippy::cast_possible_truncation)]
    process::exit(process_files(&opt)? as i32)
}

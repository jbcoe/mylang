use anyhow::Result;
use mylang::io::{process_files, Opt};
use std::process;
use structopt::StructOpt;

fn main() -> Result<()> {
    let opt = Opt::from_args();
    process::exit(process_files(&opt)?)
}

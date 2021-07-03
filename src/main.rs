use anyhow::Result;
use mylang::driver::{real_main, Opt};
use std::process;
use structopt::StructOpt;

fn main() -> Result<()> {
    process::exit(real_main(&Opt::from_args())?)
}

use anyhow::Result;
use mylang::driver::{go, Opt};
use std::process;
use structopt::StructOpt;

fn main() -> Result<()> {
    process::exit(go(&Opt::from_args())?)
}

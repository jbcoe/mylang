use anyhow::Result;
use mylang::driver::{Driver, Opt};
use std::process;
use structopt::StructOpt;

fn main() -> Result<()> {
    let opts = Opt::from_args();
    let driver = Driver::new(&opts);
    process::exit(driver.main()?)
}

use eyre::Result;
use clap::{Parser, Subcommand};

/// https://docs.rs/clap/latest/clap/_tutorial/chapter_2/index.html#subcommands
/// https://docs.rs/clap/latest/clap/_derive/index.html#command-attributes
#[derive(Parser)]
struct Cli {
  #[command(subcommand)]
  command: Command
}

#[derive(Subcommand)]
enum Command {
  Run,
}

fn main() -> Result<()> {
  let _cli = Cli::parse();

  Ok(())
}

use clap::{Parser, Subcommand};
use eyre::Result;

/// https://docs.rs/clap/latest/clap/_tutorial/chapter_2/index.html#subcommands
/// https://docs.rs/clap/latest/clap/_derive/index.html#command-attributes
#[derive(Parser)]
struct Cli {
  #[command(subcommand)]
  command: Command,
}

#[derive(Subcommand)]
enum Command {
  Run,
}

fn main() -> Result<()> {
  tracing_subscriber::fmt().init();

  let cli = Cli::parse();

  match cli.command {
    Command::Run => {
      println!("Running!");
    }
  }

  Ok(())
}

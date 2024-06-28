use clap::{Parser, Subcommand};
use color_eyre::eyre::Result;

#[allow(dead_code)]
mod parser;

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
  Decode,
  Send,
  Encode { string: String },
}

fn main() -> Result<()> {
  color_eyre::install()?;
  tracing_subscriber::fmt().init();

  let cli = Cli::parse();

  match cli.command {
    Command::Run => {
      println!("Running!");
    }
    Command::Decode => todo!(),
    Command::Send => todo!(),
    Command::Encode { string: s } => {
      use parser::Encode;

      let x = parser::Language::String(parser::Str(s));

      println!("Encoded: {}", x.encode())
    }
  }

  Ok(())
}

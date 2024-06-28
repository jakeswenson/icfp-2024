use clap::{Parser, Subcommand};
use color_eyre::eyre::Result;
use crate::parser::{ICFPExpr, Encode, Decode};

#[allow(dead_code)]
mod parser;
mod communicator;

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
  Decode { input: String },
  Send,
  Encode { string: String },
}

#[tokio::main]
async fn main() -> Result<()> {
  color_eyre::install()?;
  tracing_subscriber::fmt().init();

  let cli = Cli::parse();

  match cli.command {
    Command::Run => {
      communicator::send_program("S'%4}).$%8".to_string()).await;  
      println!("Running!");
    }
    Command::Send => todo!(),
    Command::Encode { string: s } => {
      use parser::Encode;

      let x = ICFPExpr::String(parser::Str(s));

      println!("Encoded: {}", x.encode())
    }
    Command::Decode{input} => {
      let expr = ICFPExpr::decode(&input)?;
      println!();
      println!();
      println!("Decoded: {expr:?}")
    },
  }


  Ok(())
}

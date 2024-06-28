use clap::{Parser, Subcommand};
use color_eyre::eyre::{anyhow, Result};
use dotenvy::dotenv;
use futures::future::err;
use crate::parser::{ICFPExpr, Encode, Decode, Str};
use tracing::{error, info, warn};
use crate::communicator::send_program;

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
  Get {page: String},
  Echo {text: String}
}

#[tokio::main]
async fn main() -> Result<()> {
  color_eyre::install()?;
  tracing_subscriber::fmt().init();
  dotenv().ok();

  let cli = Cli::parse();

  match cli.command {
    Command::Run => {
      let input = "S'%4}).$%8";
      info!(expr = ?ICFPExpr::decode(input)?, "Request");

      let response = communicator::send_program(input.to_string()).await?;

      info!(response = ?ICFPExpr::decode(&response)?, "Response");
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
      info!(?expr, "Decoded")
    },
    Command::Get {page} => {
      let request = format!("get {page}");

      let prog = ICFPExpr::String(Str(request));

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::decode(&response)?;

      let ICFPExpr::String(Str(page_text)) = result else {
        error!(expr = ?result, "Expected string result of page text, got");
        return Err(anyhow!("Unexpected response"))
      };

      println!("Response: {page_text}");
    }
    Command::Echo {text} => {
      let request = format!("echo {text}");

      let prog = ICFPExpr::String(Str(request));

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::decode(&response)?;

      let ICFPExpr::String(Str(page_text)) = result else {
        error!(expr = ?result, "Expected string result of echo text, got");
        return Err(anyhow!("Unexpected response"))
      };

      println!("Response: {page_text}");
    }
  }


  Ok(())
}

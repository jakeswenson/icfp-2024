use crate::communicator::send_program;
use crate::parser::{Encode, ICFPExpr, Parsable};
use clap::{Parser, Subcommand};
use color_eyre::eyre::{anyhow, Result};
use dotenvy::dotenv;
use tracing::{error, info};

mod communicator;
mod evaluator;
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
  Decode { input: String },
  Send { command: String, args: Vec<String> },
  Encode { string: String },
  Get { page: String },
  Echo { text: String },
  Test,
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
      info!(expr = ?ICFPExpr::parse(input)?, "Request");

      let response = communicator::send_program(input.to_string()).await?;

      info!(response = ?ICFPExpr::parse(&response)?, "Response");
    }
    Command::Send { command, args } => {
      let args = args.join(" ");

      let request = format!("{command} {args}");

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response)?;

      println!("Response: {result:?}");
    }
    Command::Encode { string: s } => {
      use parser::Encode;

      let x = ICFPExpr::String(s);

      println!("Encoded: {}", x.encode())
    }
    Command::Decode { input } => {
      let expr = ICFPExpr::parse(&input)?;
      println!();
      println!();
      info!(?expr, "Decoded")
    }
    Command::Get { page } => {
      let request = format!("get {page}");

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response)?;

      let ICFPExpr::String(page_text) = result else {
        error!(expr = ?result, "Expected string result of page text, got");
        return Err(anyhow!("Unexpected response"));
      };

      println!("Response: {page_text}");
    }

    Command::Test => {
      let request = "get language_test".to_string();

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response)?;

      println!("Response: {result:#?}");

      println!("Result: {:?}", evaluator::eval(result))
    }
    Command::Echo { text } => {
      let request = format!("echo {text}");

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response)?;

      let ICFPExpr::String(response_text) = result else {
        error!(expr = ?result, "Expected string result of echo text, got");
        return Err(anyhow!("Unexpected response"));
      };

      println!("Response: {response_text}");
    }
  }

  Ok(())
}

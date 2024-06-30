use crate::communicator::send_program;
use crate::parser::{Encode, ICFPExpr, Parsable};
use clap::{Parser, Subcommand};
use dotenvy::dotenv;
use miette::miette;
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
  Decode {
    input: String,
  },
  Send {
    command: String,
    args: Vec<String>,
  },
  Encode {
    string: String,
  },
  Get {
    page: String,
  },
  Echo {
    text: String,
  },
  Test,
  Spaceship {
    problem: usize,
  },
  Lambda {
    problem: usize,
  },
  DL {
    name: String,
    id: usize,
  },
  #[clap(name = "3d")]
  Spacetime {
    problem: usize,
    #[command(subcommand)]
    command: SpaceCommand,
  },
}

#[derive(Subcommand)]
enum SpaceCommand {
  Solve,
  Test {
    args: Vec<String>,
  },
  #[clap(name = "sim")]
  Simulate {
    args: Vec<String>,
    #[arg(short, default_value_t = 1_000_000)]
    iterations: usize,
  },
}

#[tokio::main]
async fn main() -> miette::Result<()> {
  miette::set_panic_hook();
  tracing_subscriber::fmt()
    .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
    .init();
  dotenv().ok();

  let cli = Cli::parse();

  match cli.command {
    Command::Run => {
      let input = "S'%4}).$%8";
      info!(expr = ?ICFPExpr::parse(input)
        .map_err(|e| miette!("Error Parsing: {}", e))?, "Request");

      let response = communicator::send_program(input.to_string()).await?;

      info!(response = ?ICFPExpr::parse(&response)
        .map_err(|e| miette!("Error Parsing: {}", e))?, "Response");
    }
    Command::Send { command, args } => {
      let args = args.join(" ");

      let request = format!("{command} {args}");

      let prog = ICFPExpr::str(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      println!("Response: {result:?}");
    }
    Command::Encode { string: s } => {
      use parser::Encode;

      let x = ICFPExpr::str(s);

      println!("Encoded: {}", x.encode())
    }
    Command::Decode { input } => {
      let expr = ICFPExpr::parse(&input).map_err(|e| miette!("Error Parsing: {}", e))?;
      println!();
      println!();
      info!(?expr, "Decoded")
    }
    Command::Get { page } => {
      let request = format!("get {page}");

      let prog = ICFPExpr::str(request);

      let response = send_program(prog.encode()).await?;

      let result =
        ICFPExpr::parse(&response).map_err(|e| miette!(help = response, "Error Parsing: {}", e))?;

      if let ICFPExpr::String(page_text) = result {
        println!("\n");
        termimad::print_inline(&page_text.decode()?)
      } else {
        println!("Expr: {result:?}")
      };
    }
    Command::Echo { text } => {
      let request = format!("echo {text}");

      let prog = ICFPExpr::str(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      let ICFPExpr::String(response_text) = result else {
        error!(expr = ?result, "Expected string result of echo text, got");
        return Err(miette!("Unexpected response"));
      };

      let decoded_response = response_text.decode()?;

      println!("Response: {decoded_response}");
    }
    Command::Test => {
      let request = "get language_test".to_string();

      let prog = ICFPExpr::str(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      println!("Response: {result:#?}");

      println!("Result: {:?}", evaluator::eval(result))
    }
    Command::DL { name, id } => problems::download(name, id).await?,
    Command::Spaceship {
      problem: problem_id,
    } => problems::spaceship::run(problem_id).await?,
    Command::Lambda {
      problem: problem_id,
    } => {
      const PROBLEM_NAME: &'static str = "lambdaman";
      let input = problems::load_input(PROBLEM_NAME, problem_id)?;
      let solution = problems::lambdaman::solve(problem_id, input)?;
      problems::submit(PROBLEM_NAME, problem_id, solution).await?
    }
    Command::Spacetime {
      problem: problem_id,
      command,
    } => {
      const PROBLEM_NAME: &'static str = "3d";
      let solution = problems::load_solution(PROBLEM_NAME, problem_id)?;

      match command {
        SpaceCommand::Solve => {
          let _solution = problems::spacetime::solve(problem_id, solution)?;
          unimplemented!();
          // problems::submit(PROBLEM_NAME, problem_id, solution).await?
        }
        SpaceCommand::Test { args } => {
          problems::test_solution(PROBLEM_NAME, args.join(" "), solution).await?;
        }
        SpaceCommand::Simulate { args, iterations } => {
          let _solution = problems::spacetime::simulate(iterations, solution, args)?;
        }
      }
    }
  }

  Ok(())
}

mod problems;

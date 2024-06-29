use crate::communicator::send_program;
use crate::evaluator::eval;
use crate::parser::{Encode, ICFPExpr, Parsable};
use clap::{Parser, Subcommand};
use dotenvy::dotenv;
use miette::miette;
use std::path::PathBuf;
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
  Spaceship { problem: usize },
  DL { name: String, id: usize },
}

#[tokio::main]
async fn main() -> miette::Result<()> {
  miette::set_panic_hook();
  tracing_subscriber::fmt().init();
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

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      println!("Response: {result:?}");
    }
    Command::Encode { string: s } => {
      use parser::Encode;

      let x = ICFPExpr::String(s);

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

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      if let ICFPExpr::String(page_text) = result {
        println!("\n");
        termimad::print_inline(&page_text)
      } else {
        println!("Expr: {result:?}")
      };
    }
    Command::Echo { text } => {
      let request = format!("echo {text}");

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      let ICFPExpr::String(response_text) = result else {
        error!(expr = ?result, "Expected string result of echo text, got");
        return Err(miette!("Unexpected response"));
      };

      println!("Response: {response_text}");
    }
    Command::Test => {
      let request = "get language_test".to_string();

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      println!("Response: {result:#?}");

      println!("Result: {:?}", evaluator::eval(result))
    }
    Command::DL { name, id } => {
      let dir = env!("CARGO_MANIFEST_DIR");
      let problems_dir = PathBuf::from(format!("{dir}/../problems/{name}"));

      let problem_path = problems_dir.join(format!("{name}{id}"));
      let problem_error_path = problems_dir.join(format!("{name}{id}.eval-error.txt"));

      let request = format!("get {name}{id}");

      let prog = ICFPExpr::String(request);

      let response = send_program(prog.encode()).await?;

      let parse_result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

      if let ICFPExpr::String(page_text) = parse_result {
        std::fs::write(problem_path, page_text)
          .map_err(|e| miette!("Failed to write file: {}", e))?;
        println!("Done!");
      } else {
        println!("Expr: {parse_result:?}");
        let eval_result = eval(parse_result.clone());

        let Ok(result) = eval_result else {
          std::fs::write(problem_error_path, format!("{:#?}", parse_result))
            .map_err(|e| miette!("Failed to write file: {}", e))?;
          eval_result?;
          return Ok(());
        };

        let ICFPExpr::String(page_text) = result else {
          println!("did not eval to a string!");
          return Ok(());
        };
        std::fs::write(problem_path, page_text)
          .map_err(|e| miette!("Failed to write file: {}", e))?;
      };
    }
    Command::Spaceship { problem } => {
      let dir = env!("CARGO_MANIFEST_DIR");
      let problems_dir = PathBuf::from(format!("{dir}/../problems/spaceship"));

      let problem_path = problems_dir.join(format!("spaceship{problem}"));

      let problem = std::fs::read_to_string(dbg!(&problem_path))
        .map_err(|e| miette!("Failed to read file: {}", e))?;

      #[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
      struct Point {
        x: i32,
        y: i32,
      }

      impl Point {
        fn at(
          x: i32,
          y: i32,
        ) -> Self {
          Self { x, y }
        }
      }

      let mut all_points: Vec<Point> = problem
        .lines()
        .map(|line| {
          let vec = line
            .splitn(2, " ")
            .map(str::parse::<i32>)
            .map(|r| r.unwrap())
            .collect::<Vec<_>>();
          Point {
            x: vec[0],
            y: vec[1],
          }
        })
        .collect();

      dbg!(&all_points);

      struct Spaceship {
        vx: i32,
        vy: i32,
      }

      fn gcd(
        mut a: i32,
        mut b: i32,
      ) -> i32 {
        while b != 0 {
          let temp = b;
          b = a % b;
          a = temp;
        }
        a.abs() // Ensure the GCD is positive
      }

      fn normalize_to_lcm_point(point: Point) -> Point {
        // Calculate the GCD of x and y
        let x = point.x;
        let y = point.y;
        let gcd_value = gcd(x, y);

        // Normalize the point by dividing each coordinate by the GCD
        let nx = x / gcd_value;
        let ny = y / gcd_value;

        Point { x: nx, y: ny }
      }

      fn extended_taxicab_distance(
        p1: Point,
        p2: Point,
      ) -> i32 {
        let dx = (p1.x - p2.x).abs();
        let dy = (p1.y - p2.y).abs();

        // The Chebyshev distance is the maximum of these differences
        std::cmp::max(dx, dy)
      }

      let starting_point = Point::default();

      let best_option = all_points
        .iter()
        .map(|p| normalize_to_lcm_point(*p))
        .min_by(|p1, p2| {
          extended_taxicab_distance(starting_point, *p1)
            .cmp(&extended_taxicab_distance(starting_point, *p2))
        })
        .unwrap();

      dbg!(best_option);
      all_points.retain(|p| *p != best_option);

      let starting_point = Point::at(1, -1);

      let best_option = all_points
        .iter()
        .map(|p| normalize_to_lcm_point(*p))
        .min_by(|p1, p2| {
          extended_taxicab_distance(starting_point, *p1)
            .cmp(&extended_taxicab_distance(starting_point, *p2))
        })
        .unwrap();

      dbg!(best_option);
    }
  }

  Ok(())
}

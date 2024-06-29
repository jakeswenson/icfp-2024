use crate::communicator::send_program;
use crate::evaluator::eval;
use crate::parser::{Encode, ICFPExpr, Parsable};
use miette::{miette, Diagnostic};
use std::path::PathBuf;
use thiserror::Error;
use tracing::info;

#[allow(dead_code)]
pub mod spaceship;

#[allow(dead_code)]
pub mod lambdaman;

pub mod spacetime;

#[derive(Error, Diagnostic, Debug)]
pub enum ProblemError {
  #[error("Bad Solution: {reason}")]
  BadSolution { reason: &'static str },
}

pub(crate) async fn download(
  name: String,
  id: usize,
) -> miette::Result<()> {
  let dir = env!("CARGO_MANIFEST_DIR");
  let problems_dir = PathBuf::from(format!("{dir}/../problems/{name}"));

  let problem_path = problems_dir.join(format!("{name}{id}"));
  let problem_error_path = problems_dir.join(format!("{name}{id}.eval-error.txt"));
  let problem_raw_path = problems_dir.join(format!("{name}{id}.raw"));

  let request = format!("get {name}{id}");

  let prog = ICFPExpr::String(request);

  let response = send_program(prog.encode()).await?;

  std::fs::write(problem_raw_path, &response).map_err(|e| miette!("Failed to write raw file: {}", e))?;

  let parse_result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

  if let ICFPExpr::String(page_text) = parse_result {
    std::fs::write(problem_path, page_text).map_err(|e| miette!("Failed to write file: {}", e))?;
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
    std::fs::write(problem_path, page_text).map_err(|e| miette!("Failed to write file: {}", e))?;
  };

  Ok(())
}
pub(crate) fn load_input(
  problem: &str,
  id: usize,
) -> miette::Result<String> {
  let dir = env!("CARGO_MANIFEST_DIR");
  let problems_dir = PathBuf::from(format!("{dir}/../problems/{problem}"));

  let problem_path = problems_dir.join(format!("{problem}{id}"));

  let problem = std::fs::read_to_string(dbg!(&problem_path))
    .map_err(|e| miette!("Failed to read file: {}", e))?;

  Ok(problem)
}

pub(crate) fn load_solution(
  problem: &str,
  id: usize,
) -> miette::Result<String> {
  let dir = env!("CARGO_MANIFEST_DIR");
  let problems_dir = PathBuf::from(format!("{dir}/../problems/{problem}"));

  let problem_path = problems_dir.join(format!("{problem}{id}.solve"));

  let problem = std::fs::read_to_string(dbg!(&problem_path))
    .map_err(|e| miette!("Failed to read file: {}", e))?;

  Ok(problem)
}

pub(crate) async fn submit(
  problem: &str,
  id: usize,
  solution: String,
) -> miette::Result<()> {
  let request = format!("solve {problem}{id} {solution}");

  info!(request, "Submitting solution");
  let prog = ICFPExpr::String(request);

  let response = send_program(prog.encode()).await?;

  let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

  println!("Response: {result:?}");

  Ok(())
}


pub(crate) async fn test_solution(
  problem: &str,
  args: String,
  solution: String,
) -> miette::Result<()> {
  let request = format!("test {problem} {args}\n{solution}");

  info!(request, "Submitting solution");
  let prog = ICFPExpr::String(request);

  let response = send_program(prog.encode()).await?;

  println!("{response}");
  println!();

  let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

  println!("Response: {result:?}");

  Ok(())
}

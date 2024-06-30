use crate::communicator::send_program;
use crate::evaluator::eval;
use crate::parser::{Encode, ICFPExpr, Parsable};
use miette::{miette, Diagnostic};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Add;
use std::path::PathBuf;
use thiserror::Error;
use tracing::{debug, info};

#[allow(dead_code)]
pub mod spaceship;

#[allow(dead_code)]
pub mod lambdaman;

pub mod spacetime;

#[derive(Default, Copy, Clone, PartialEq, Eq, Hash)]
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

impl Debug for Point {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}

impl Add<(i32, i32)> for Point {
  type Output = Point;

  fn add(
    self,
    rhs: (i32, i32),
  ) -> Self::Output {
    Point::at(self.x + rhs.0, self.y + rhs.1)
  }
}

impl Add<Direction> for Point {
  type Output = Point;

  fn add(
    self,
    rhs: Direction,
  ) -> Self::Output {
    self + rhs.adjustment()
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
enum Direction {
  Up,
  Down,
  Left,
  Right,
}

impl Direction {
  fn adjustment(&self) -> (i32, i32) {
    match self {
      Direction::Up => (-1, 0),
      Direction::Down => (1, 0),
      Direction::Left => (0, -1),
      Direction::Right => (0, 1),
    }
  }
}

// Define directions with corresponding names
const DIRS: [((isize, isize), Direction); 4] = [
  ((-1, 0), Direction::Up),   // up
  ((1, 0), Direction::Down),  // down
  ((0, -1), Direction::Left), // left
  ((0, 1), Direction::Right), // right
];

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

  let prog = ICFPExpr::str(request);

  let response = send_program(prog.encode()).await?;

  std::fs::write(problem_raw_path, &response)
    .map_err(|e| miette!("Failed to write raw file: {}", e))?;
  let parse_result =
    ICFPExpr::parse(&response).map_err(|e| miette!(help = response, "Error Parsing: {}", e))?;

  if let ICFPExpr::String(page_text) = parse_result {
    std::fs::write(problem_path, page_text.decode()?)
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
    std::fs::write(problem_path, page_text.decode()?)
      .map_err(|e| miette!("Failed to write file: {}", e))?;
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
  let prog = ICFPExpr::str(request);

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

  info!(request, "Submitting TEST solution");
  let prog = ICFPExpr::str(request);

  let response = send_program(prog.encode()).await?;

  debug!(raw = response, "Response");
  println!();

  let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

  info!(expr = ?result, "Parsed");

  let evald = eval(result)?;

  info!(expr = ?evald, "Eval'd");

  Ok(())
}

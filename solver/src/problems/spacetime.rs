use crate::problems::ProblemError;

// Shared types
#[derive(Copy, Clone)]
enum Operator {
  ShiftLeft,
  ShiftRight,
  ShiftUp,
  ShiftDown,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Warp,
  Eq,
  NotEq,
}

#[derive(Copy, Clone)]
enum OperatorArity {
  Unary,
  Binary,
  Quad,
}

#[derive(Copy, Clone)]
enum Parameter {
  A,
  B
}

#[derive(Copy, Clone)]
enum CellValues {
  Op(Operator),
  Param(Parameter),
  Val(i32),
  EndState,
  Empty,
}

#[derive(Copy, Clone)]
struct Cell {
  x: i32,
  y: i32,
  value: CellValues,
}
// End

// Helpers
fn OperatorArity(op : Operator) -> OperatorArity {
  match op {
    Operator::ShiftLeft => OperatorArity::Unary,
    Operator::ShiftRight => OperatorArity::Unary,
    Operator::ShiftUp => OperatorArity::Unary,
    Operator::ShiftDown => OperatorArity::Unary,
    Operator::Add => OperatorArity::Binary,
    Operator::Sub => OperatorArity::Binary,
    Operator::Mul => OperatorArity::Binary,
    Operator::Div => OperatorArity::Binary,
    Operator::Mod => OperatorArity::Binary,
    Operator::Warp => OperatorArity::Quad,
    Operator::Eq => OperatorArity::Binary,
    Operator::NotEq => OperatorArity::Binary,
  }
}
// End

// Read from file
// End

// Compiler
// End

// Evaluator
fn evaluate(grid: Vec<Vec<Cell>>) {
  let operators : Vec<_> = grid.iter().flatten().copied().filter(|cell| {
    if let CellValues::Op(_) = cell.value {
      true
    } else {
      false
    }
  }).collect();
  for t in 1 .. 1000000 {
  }
}
// End

// Visualizer
// End

pub fn solve(
  _id: usize,
  input: String,
) -> miette::Result<String, ProblemError> {
  Err(ProblemError::BadSolution {
    reason: "TO DO"
  })
}

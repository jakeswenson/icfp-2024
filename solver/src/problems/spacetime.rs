use crate::problems::ProblemError;

// Shared types
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

enum Parameter {
  A,
  B
}

enum CellValues {
  Op(Operator),
  Param(Parameter),
  Val(i32),
  EndState,
  Empty,
}
struct Cell {
  x: i32,
  y: i32,
  value: CellValues,
}
// End

// Read from file
// End

// Compiler
// End

// Evaluator
fn evaluate(grid: Vec<Vec<Cell>>) {

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

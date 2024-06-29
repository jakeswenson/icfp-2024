use crate::problems::ProblemError;

pub fn solve(
  _id: usize,
  input: String,
) -> miette::Result<String, ProblemError> {
  let _x = input.is_empty();

  Err(ProblemError::BadSolution {
    reason: "Not Solved",
  })
}

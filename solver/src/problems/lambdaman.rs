use crate::problems::lambdaman::Direction::Down;
use crate::problems::ProblemError;
use std::collections::{HashSet, VecDeque};
use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Direction {
  Up,
  Down,
  Left,
  Right,
}

impl Display for Direction {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Up => "U",
        Down => "D",
        Left => "L",
        Right => "R",
      }
    )
  }
}

impl Direction {
  fn backtrack(self) -> Self {
    match self {
      Up => Down,
      Down => Up,
      Left => Right,
      Right => Left,
    }
  }
}

use crate::problems::lambdaman::GridState::{DontComeBack, Visited};
use Direction::*;

// Define directions with corresponding names
const DIRS: [((isize, isize), Direction); 4] = [
  ((-1, 0), Up),   // up
  ((1, 0), Down),  // down
  ((0, -1), Left), // left
  ((0, 1), Right), // right
];

struct State {
  pos: (usize, usize),          // current position (row, col)
  direction: Option<Direction>, // direction taken to reach current position
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum GridState {
  Candy,
  Visited,
  DontComeBack,
  Wall,
}

// DFS function to find the path with direction tracking without recursion
fn dfs_without_recursion(
  grid: &[Vec<char>],
  start: (usize, usize),
) -> Option<Vec<((usize, usize), Direction)>> {
  let rows = grid.len();
  let cols = grid[0].len();

  let mut state_grid = vec![vec![GridState::Candy; cols]; rows];
  let mut path = Vec::new();
  let mut to_visit_stack = VecDeque::new();

  let mut actual_walked_path = Vec::new();

  // Initialize the start point
  to_visit_stack.push_back(State {
    pos: start,
    direction: None,
  });

  state_grid[start.0][start.1] = Visited;

  // Function to check if a cell is within bounds and not a wall
  fn is_valid(
    grid: &[Vec<char>],
    row: usize,
    col: usize,
    grid_state: &[Vec<GridState>],
  ) -> bool {
    row < grid.len()
      && col < grid[0].len()
      && grid[row][col] != '#'
      && grid_state[row][col] != DontComeBack
  }

  let mut iter = 0;

  while let Some(State { pos, direction }) = to_visit_stack.pop_back() {
    let (row, col) = pos;

    // Push the current position and direction to the path
    if let Some(dir) = direction {
      path.push((pos, dir));
      actual_walked_path.push(dir);
      println!("Moved: {}", dir);
    }

    // Explore all directions
    let mut found_next = false;
    for &((dr, dc), dir_name) in &DIRS {
      if Some(dir_name.backtrack()) == direction {
        println!("Skipping dir: {}", dir_name);
        continue;
      }

      let new_row = (row as isize + dr) as isize;
      let new_col = (col as isize + dc) as isize;

      if new_row >= 0
        && new_col >= 0
        && is_valid(grid, new_row as usize, new_col as usize, &state_grid)
      {
        let new_pos = (new_row as usize, new_col as usize);

        // Move to the new position
        to_visit_stack.push_back(State {
          pos: new_pos,
          direction: Some(dir_name),
        });
        state_grid[new_pos.0][new_pos.1] = Visited;
        found_next = true;
      } else {
        println!("Not valid: {:?}", (new_row, new_col))
      }
    }

    // If no valid move found, backtrack by popping from path
    if !found_next {
      state_grid[pos.0][pos.1] = DontComeBack;
      let (back_track, dir) = path.pop().unwrap();
      println!("No move from {:?}, backtracking: {:?}", pos, back_track);
      to_visit_stack.push_back(State {
        pos: back_track,
        direction: Some(dir.backtrack()),
      });
    }

    if iter > 80 {
      break;
    }
    iter += 1;
  }

  Some(path)
}

pub fn solve(
  _id: usize,
  input: String,
) -> miette::Result<String, ProblemError> {
  let _x = input.is_empty();

  let lines = input.lines().collect::<Vec<_>>();

  lines.iter().for_each(|line| println!("|{}|", line));

  let grid = lines
    .iter()
    .map(|row| row.chars().collect::<Vec<_>>())
    .collect::<Vec<_>>();
  let start = (0, 0);

  match dfs_without_recursion(&grid, start) {
    Some(path) => {
      let solution = path
        .iter()
        .map(|(p, dir)| dir.to_string())
        .collect::<String>();
      println!("Path: {}", solution);

      Ok(solution)
    }
    None => Err(ProblemError::BadSolution {
      reason: "Not Solved",
    }),
  }
}

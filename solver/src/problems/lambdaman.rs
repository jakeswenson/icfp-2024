use crate::problems::ProblemError;
use std::collections::{HashSet, VecDeque};

// Define directions with corresponding names
const DIRS: [((isize, isize), &str); 4] = [
  ((-1, 0), "U"), // up
  ((1, 0), "D"),  // down
  ((0, -1), "L"), // left
  ((0, 1), "R"),  // right
];

// Function to check if a cell is within bounds and not a wall
fn is_valid(
  grid: &[Vec<char>],
  row: isize,
  col: isize,
) -> bool {
  row >= 0
    && row < grid.len() as isize
    && col >= 0
    && col < grid[0].len() as isize
    && grid[row as usize][col as usize] != '#'
}

struct State {
  pos: (usize, usize),             // current position (row, col)
  direction: Option<&'static str>, // direction taken to reach current position
}

// DFS function to find the path with direction tracking without recursion
fn dfs_without_recursion(
  grid: &[Vec<char>],
  start: (usize, usize),
) -> Option<Vec<((usize, usize), &str)>> {
  let rows = grid.len();
  let cols = grid[0].len();

  let mut visited = HashSet::new();
  let mut path = Vec::new();
  let mut stack = VecDeque::new();

  // Initialize the start point
  stack.push_back(State {
    pos: start,
    direction: None,
  });
  visited.insert(start);

  while let Some(State { pos, direction }) = stack.pop_back() {
    let (row, col) = pos;

    // Push the current position and direction to the path
    if let Some(dir) = direction {
      path.push((pos, dir));
    }

    // Explore all directions
    let mut found_next = false;
    for &((dr, dc), dir_name) in &DIRS {
      let new_row = row as isize + dr;
      let new_col = col as isize + dc;

      if is_valid(grid, new_row, new_col) {
        let new_pos = (new_row as usize, new_col as usize);

        // Move to the new position
        if !visited.contains(&new_pos) {
          stack.push_back(State {
            pos: new_pos,
            direction: Some(dir_name),
          });
          visited.insert(new_pos);
          found_next = true;
        }
      }
    }

    // If no valid move found, backtrack by popping from path
    if !found_next && path.last().map_or(false, |&(p, _)| p == pos) {
      path.pop();
    }
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
      let solution = path.iter().map(|(p, dir)| *dir).collect::<String>();
      println!("Path: {}", solution);

      Ok(solution)
    }
    None => Err(ProblemError::BadSolution {
      reason: "Not Solved",
    }),
  }
}

use crate::problems::lambdaman::Direction::Down;
use crate::problems::ProblemError;
use std::collections::VecDeque;
use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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

  fn move_pos(
    self,
    (row, col): (usize, usize),
  ) -> (usize, usize) {
    match self {
      Up => (row - 1, col),
      Down => (row + 1, col),
      Left => (row, col - 1),
      Right => (row, col + 1),
    }
  }
}

use crate::problems::lambdaman::GridState::{Candy, Visited};
use Direction::*;

// Define directions with corresponding names
const DIRS: [((isize, isize), Direction); 4] = [
  ((-1, 0), Up),   // up
  ((1, 0), Down),  // down
  ((0, -1), Left), // left
  ((0, 1), Right), // right
];

#[derive(Debug)]
struct State {
  pos: (usize, usize),          // current position (row, col)
  direction: Option<Direction>, // direction taken to reach current position
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum GridState {
  Candy,
  Visited,
}

// DFS function to find the path with direction tracking without recursion
fn dfs_without_recursion(
  grid: Vec<Vec<char>>,
  start: (usize, usize),
) -> Option<Vec<Direction>> {
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
  let mut map = grid.clone();
  map[start.0][start.1] = 'S';

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
      && grid_state[row][col] != Visited
  }

  let mut is_backtracking = false;

  while let Some(State { pos, direction }) = to_visit_stack.pop_back() {
    let (row, col) = pos;

    if map[row][col] == '.' {
      map[row][col] = 'a';
    } else {
      map[row][col] = (map[row][col] as u8 + 1) as char;
    }

    // Push the current position and direction to the path
    if let Some(dir) = direction {
      actual_walked_path.push(dir);
      state_grid[pos.0][pos.1] = Visited;
      if !is_backtracking {
        path.push((pos, dir));
      }
    }

    // println!("path: {:?}", path.last());
    //
    // print_grid(&map);
    // let debug_path = actual_walked_path
    //   .iter()
    //   .map(|dir| dir.to_string())
    //   .collect::<String>();
    // println!("{}", debug_path);
    // Explore all directions
    let mut found_next = false;
    for &((dr, dc), dir_name) in &DIRS {
      if Some(dir_name.backtrack()) == direction {
        continue;
      }

      let new_row = (row as isize + dr) as isize;
      let new_col = (col as isize + dc) as isize;

      if new_row >= 0
        && new_col >= 0
        && is_valid(&grid, new_row as usize, new_col as usize, &state_grid)
      {
        let new_pos = (new_row as usize, new_col as usize);

        // Move to the new position
        to_visit_stack.push_back(State {
          pos: new_pos,
          direction: Some(dir_name),
        });

        found_next = true;
      }
    }

    // If no valid move found, backtrack by popping from path
    if !found_next {
      map[pos.0][pos.1] = '!';
      if has_candy(&state_grid) && !path.is_empty() {
        let (back_track, dir): ((usize, usize), Direction) = path.pop().unwrap();

        let backwards = dir.backtrack();
        // actual_walked_path.push(backwards);

        let state = State {
          pos: backwards.move_pos(back_track),
          direction: Some(backwards),
        };

        // println!("Backtrack: {:?}", state);

        to_visit_stack.push_back(state);
      }

      is_backtracking = true;
    } else {
      is_backtracking = false;
    }

    // if iter > 80 {
    //   break;
    // }
    // iter += 1;
  }

  Some(actual_walked_path)
}

fn has_candy(grid: &[Vec<GridState>]) -> bool {
  grid.iter().any(|row| row.iter().any(|c| *c == Candy))
}

fn print_grid(grid: &[Vec<char>]) {
  println!("============");
  grid
    .iter()
    .for_each(|line| println!("|{}|", line.iter().collect::<String>()));
  println!("============");
}

pub fn solve(
  _id: usize,
  input: String,
) -> miette::Result<String, ProblemError> {
  let _x = input.is_empty();

  let grid = input
    .lines()
    .map(|row| row.chars().collect::<Vec<_>>())
    .collect::<Vec<_>>();

  let (start, _): ((usize, usize), &char) = grid
    .iter()
    .enumerate()
    .flat_map(|(idx, row): (usize, _)| {
      row
        .iter()
        .enumerate()
        .map(move |(colidx, col)| ((idx, colidx).clone(), col))
    })
    .find(|(_, char)| **char == 'L')
    .unwrap();

  // print_grid(&grid);

  match dfs_without_recursion(grid, start) {
    Some(path) => {
      let solution = path.iter().map(|dir| dir.to_string()).collect::<String>();
      println!("Path: {}", solution);

      Ok(solution)
    }
    None => Err(ProblemError::BadSolution {
      reason: "Not Solved",
    }),
  }
}

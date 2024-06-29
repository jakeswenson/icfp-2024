use crate::communicator::send_program;
use crate::parser::{Encode, ICFPExpr, Parsable};
use miette::miette;
use std::fmt::{Debug, Formatter};
use std::ops::Not;
use std::path::PathBuf;

#[derive(Default, Copy, Clone, PartialEq, Eq)]
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

mod tims {
  pub fn number_of_moves_left(
    d: i32,
    t: i32,
    v: i32,
  ) -> i32 {
    return (t - d / v).abs();
  }

  pub fn velocity_compute(
    d: i32,
    t: i32,
    v: i32,
  ) -> i32 {
    let n = if d + v < t {
      1
    } else if d + v > t {
      -1
    } else {
      0
    };
    return n;
  }

  pub fn compute_move(
    nx: i32,
    ny: i32,
  ) -> i32 {
    if nx == 1 {
      if ny == -1 {
        return 3;
      } else if ny == 1 {
        return 9;
      } else {
        return 6;
      }
    } else if nx == -1 {
      if ny == -1 {
        return 1;
      } else if ny == 1 {
        return 7;
      } else {
        return 4;
      }
    } else {
      if ny == -1 {
        return 2;
      } else if ny == 1 {
        return 8;
      } else {
        return 5;
      }
    }
  }
}

pub async fn run(problem_id: usize) -> miette::Result<()> {
  {
    let dir = env!("CARGO_MANIFEST_DIR");
    let problems_dir = PathBuf::from(format!("{dir}/../problems/spaceship"));

    let problem_path = problems_dir.join(format!("spaceship{problem_id}"));

    let problem = std::fs::read_to_string(dbg!(&problem_path))
      .map_err(|e| miette!("Failed to read file: {}", e))?;

    let all_points: Vec<Point> = problem
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

    // TODO: Weird dedup OFF helped on 3
    // all_points.dedup();

    // dbg!(&all_points);

    fn compute_moves(mut pts: Vec<Point>) -> Vec<i32> {
      let mut moves = Vec::new();
      let mut curr = Point::default();

      let mut vx = 0;
      let mut vy = 0;

      while pts.is_empty().not() {
        let closest = *pts
          .iter()
          .min_by(|p1, p2| {
            extended_taxicab_distance(curr, **p1).cmp(&extended_taxicab_distance(curr, **p2))
          })
          .expect("How could we not have a point");

        let pos = pts.iter().position(|x| *x == closest).unwrap();
        pts.remove(pos);

        // if we are already at the target point but our velocity is not 0, then
        // we need to move
        loop {
          let nx = tims::velocity_compute(curr.x, closest.x, vx);
          let ny = tims::velocity_compute(curr.y, closest.y, vy);
          let move_number = tims::compute_move(nx, ny);

          vx = vx + nx;
          vy = vy + ny;
          curr.x = curr.x + vx;
          curr.y = curr.y + vy;
          moves.push(move_number);

          let _x = curr.x;
          let _y = curr.y;
          // println!("move: {move_number}, dx: {x}, dy: {y}, vx: {vx}, vy: {vy}");

          if curr.x == closest.x && curr.y == closest.y {
            break;
          }
        }

        let _x = curr.x;
        let _y = curr.y;
        // println!("reached: {x}, {y}")
      }
      return moves;
    }

    let all_moves = compute_moves(all_points.clone());
    println!();
    println!();

    let solution = all_moves.iter().map(|m| format!("{m}")).collect::<String>();

    print!("{}", solution);

    println!();
    println!();
    let len = all_moves.len();
    println!("Total moves: {len}");

    if len > 10_000_000 {
      println!("Can't submit with too many moves");
      return Ok(());
    }

    let request = format!("solve spaceship{problem_id} {solution}");

    let prog = ICFPExpr::str(request);

    let response = send_program(prog.encode()).await?;

    let result = ICFPExpr::parse(&response).map_err(|e| miette!("Error Parsing: {}", e))?;

    println!("Response: {result:?}");

    Ok(())
  }
}

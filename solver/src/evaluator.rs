use super::parser::{
  base94_decode, base94_encode_number, BinOp, Decode, Encode, ICFPExpr, UnOp, Var,
};
use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::Add;

pub fn eval(expr: ICFPExpr) -> ICFPExpr {
  expr.eval(&Environment::default())
}

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct Environment {
  vars: HashMap<Var, LazyExpr>,
}

impl Environment {
  fn bind(
    &self,
    var: Var,
    expr: ICFPExpr,
  ) -> Self {
    let mut next = self.clone();

    next.vars.insert(var, LazyExpr::new(expr, self.clone()));

    next
  }

  fn _merge(
    &self,
    other: &Self,
  ) -> Environment {
    let mut new_env = self.clone();
    new_env.vars.extend(other.vars.clone());

    new_env
  }
}

pub trait Evalable {
  fn eval(
    &self,
    env: &Environment,
  ) -> ICFPExpr;
}

#[derive(Clone)]
struct LazyExpr {
  once_cell: OnceCell<ICFPExpr>,
  func: DeferredEval,
}

impl PartialEq for LazyExpr {
  fn eq(
    &self,
    other: &Self,
  ) -> bool {
    self.func.expr.eq(&other.func.expr)
  }
}

impl Eq for LazyExpr {}

impl Debug for LazyExpr {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "lazy({:?})", self.once_cell.get())
  }
}

trait LazyInitializer {
  fn eval(&self) -> ICFPExpr;
}

#[derive(Clone)]
struct DeferredEval {
  expr: ICFPExpr,
  env: Environment,
}

impl LazyInitializer for DeferredEval {
  fn eval(&self) -> ICFPExpr {
    self.expr.eval(&self.env)
  }
}

impl LazyExpr {
  fn new(
    value: ICFPExpr,
    environment: Environment,
  ) -> Self {
    Self {
      once_cell: OnceCell::new(),
      func: DeferredEval {
        expr: value,
        env: environment,
      },
    }
  }

  fn get(&self) -> ICFPExpr {
    self.once_cell.get_or_init(|| self.func.eval()).clone()
  }
}

impl Evalable for ICFPExpr {
  fn eval(
    &self,
    env: &Environment,
  ) -> ICFPExpr {
    // info!(env = ?env.vars, name = ?self, "Eval");

    match self {
      literal @ ICFPExpr::Boolean(_)
      | literal @ ICFPExpr::Integer(_)
      | literal @ ICFPExpr::String(_) => literal.clone(),
      ICFPExpr::UnaryOp(op, operand) => match op {
        UnOp::Negate => {
          let ICFPExpr::Integer(i) = operand.eval(env) else {
            panic!("Wrong type ")
          };

          ICFPExpr::Integer(i.wrapping_neg())
        }
        UnOp::Not => {
          let ICFPExpr::Boolean(b) = operand.eval(env) else {
            panic!("Wrong type ")
          };

          ICFPExpr::Boolean(!b)
        }
        UnOp::StrToInt => {
          let ICFPExpr::String(s) = operand.eval(env) else {
            panic!("Wrong type ")
          };

          ICFPExpr::Integer(base94_decode(&s.encode()).unwrap())
        }
        UnOp::IntToStr => {
          let ICFPExpr::Integer(i) = operand.eval(env) else {
            panic!("Wrong type ")
          };

          ICFPExpr::String(String::decode(&base94_encode_number(i as usize)).unwrap())
        }
      },
      ICFPExpr::BinaryOp(op, left, right) => match op {
        BinOp::Add => ICFPExpr::Integer(left.eval(env).expect_int() + right.eval(env).expect_int()),
        BinOp::Sub => ICFPExpr::Integer(left.eval(env).expect_int() - right.eval(env).expect_int()),
        BinOp::Mul => ICFPExpr::Integer(left.eval(env).expect_int() * right.eval(env).expect_int()),
        BinOp::Div => ICFPExpr::Integer(left.eval(env).expect_int() / right.eval(env).expect_int()),
        BinOp::Mod => ICFPExpr::Integer(left.eval(env).expect_int() % right.eval(env).expect_int()),
        BinOp::LessThan => {
          ICFPExpr::Boolean(left.eval(env).expect_int() < right.eval(env).expect_int())
        }
        BinOp::GreaterThan => {
          ICFPExpr::Boolean(left.eval(env).expect_int() > right.eval(env).expect_int())
        }
        BinOp::Equals => ICFPExpr::Boolean(left.eval(env) == right.eval(env)),
        BinOp::Or => {
          ICFPExpr::Boolean(left.eval(env).expect_bool() || right.eval(env).expect_bool())
        }
        BinOp::And => {
          ICFPExpr::Boolean(left.eval(env).expect_bool() && right.eval(env).expect_bool())
        }
        BinOp::Concat => ICFPExpr::String(
          left
            .eval(env)
            .expect_string()
            .to_string()
            .add(right.eval(env).expect_string()),
        ),
        BinOp::TakeChars => ICFPExpr::String(
          right.eval(env).expect_string()[..left.eval(env).expect_usize()].to_string(),
        ),
        BinOp::SkipChars => ICFPExpr::String(
          right.eval(env).expect_string()[left.eval(env).expect_usize()..].to_string(),
        ),
        BinOp::ApplyLambda => match left.eval(env) {
          ICFPExpr::Lambda(arg_name, body) => {
            let mut environment = env.bind(arg_name, *right.clone());
            body.eval(&mut environment)
          }
          ICFPExpr::Closure { arg, body, env } => {
            let mut environment = env.bind(arg, *right.clone());
            body.eval(&mut environment)
          }
          e => panic!("Expected lambda, got {e:?}"),
        },
      },
      ICFPExpr::If(cond, if_true, if_false) => {
        if cond.eval(env).expect_bool() {
          if_true.eval(env)
        } else {
          if_false.eval(env)
        }
      }
      ICFPExpr::Lambda(arg, body) => ICFPExpr::Closure {
        arg: *arg,
        body: body.clone(),
        env: env.clone(),
      },
      closure @ ICFPExpr::Closure { .. } => closure.clone(),
      ICFPExpr::VarRef(var) => env
        .vars
        .get(var)
        .expect(&format!(
          "Var {var:?} not found in environment {:?}!",
          env.vars.keys()
        ))
        .get(),
      ICFPExpr::Unknown { indicator, .. } => {
        unimplemented!("Unknown {indicator} not evaluable")
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::evaluator::{eval, Environment, Evalable};
  use crate::parser::{ICFPExpr, Parsable, Var};

  #[test]
  fn simple_function_application() {
    let f = ICFPExpr::lambda(
      Var(0),
      ICFPExpr::if_(ICFPExpr::var(0), ICFPExpr::const_true(), ICFPExpr::int(3)),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(f.clone(), ICFPExpr::const_true()).eval(&env),
      ICFPExpr::const_true()
    );
    assert_eq!(
      ICFPExpr::call(f, ICFPExpr::const_false()).eval(&env),
      ICFPExpr::int(3)
    );
  }

  #[test]
  fn curried_function() {
    let f = ICFPExpr::lambda(
      Var(0),
      ICFPExpr::lambda(
        Var(1),
        ICFPExpr::if_(ICFPExpr::var(0), ICFPExpr::VarRef(Var(1)), ICFPExpr::int(3)),
      ),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(
        ICFPExpr::call(f.clone(), ICFPExpr::const_true()),
        ICFPExpr::int(13)
      )
      .eval(&env),
      ICFPExpr::int(13)
    );
    assert_eq!(
      ICFPExpr::call(
        ICFPExpr::call(f.clone(), ICFPExpr::const_false()),
        ICFPExpr::int(13)
      )
      .eval(&env),
      ICFPExpr::int(3)
    );
  }

  #[test]
  fn modulo() -> color_eyre::Result<()> {
    assert_eq!(eval(ICFPExpr::parse("B% U- I( I#")?), ICFPExpr::int(-1));

    Ok(())
  }

  #[test]
  fn num_to_string() -> color_eyre::Result<()> {
    assert_eq!(
      eval(ICFPExpr::parse("U$ I4%34")?),
      ICFPExpr::String("test".to_string())
    );

    Ok(())
  }

  #[test]
  fn string_to_num() -> color_eyre::Result<()> {
    assert_eq!(eval(ICFPExpr::parse("U# S4%34")?), ICFPExpr::int(15818151));

    Ok(())
  }
}

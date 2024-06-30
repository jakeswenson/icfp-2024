use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Neg};
use std::sync::Arc;

use miette::{Diagnostic, Report};
use thiserror::Error;

use super::parser::{
  base94_decode, base94_encode_number, BinOp, Decode, DeferredDecode, Encode, ICFPExpr, IntType,
  UnOp, Var,
};

type EvalResult<Error = EvalError> = Result<ICFPExpr, Error>;

pub fn eval(expr: ICFPExpr) -> EvalResult<Report> {
  Ok(expr.eval(&Environment::default())?)
}

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct Environment {
  vars: HashMap<Var, Arc<LazyExpr>>,
}

impl Environment {
  fn bind(
    &self,
    var: Var,
    expr: ICFPExpr,
  ) -> Self {
    let mut next = self.clone();
    next
      .vars
      .insert(var, Arc::new(LazyExpr::new(expr, self.clone())));
    next
  }

  fn merge(
    &self,
    other: &Self,
    except: Var,
  ) -> Environment {
    let mut new_env = self.clone();
    new_env.vars.extend(other.vars.clone());
    new_env
      .vars
      .insert(except, self.vars.get(&except).unwrap().clone());
    new_env
  }
}

trait Evaluable {
  fn eval(
    &self,
    env: &Environment,
  ) -> EvalResult;
}

#[derive(Debug, Diagnostic, Error, Clone)]
pub enum EvalError {
  #[error("Unbound Variable")]
  #[diagnostic(code(eval::env::missing_var))]
  VariableNotInEnvironment {
    #[help]
    variable: String,
  },
  #[error("Expected {expected_type}")]
  #[diagnostic(code(eval::expected_type))]
  ExpectedType {
    expected_type: &'static str,
    #[help]
    got: String,
  },
  #[error("{message}")]
  #[diagnostic(code(eval::error))]
  ErrorContext {
    message: &'static str,
    #[help]
    ctx: &'static str,
    #[related]
    related: Vec<EvalError>,
  },
  #[error("{message}")]
  #[diagnostic(code(eval::error))]
  DecodeError {
    message: String,
    #[help]
    ctx: String,
  },
}

impl ICFPExpr {
  pub fn expect_int(&self) -> Result<IntType, EvalError> {
    match self {
      ICFPExpr::Integer(i) => Ok(i.decode().map_err(|e| EvalError::DecodeError {
        message: format!("failed to decode int: {e}"),
        ctx: format!("coded: {i:?}"),
      })?),
      e => Err(EvalError::ExpectedType {
        expected_type: "int",
        got: format!("Got: {e:?}"),
      }),
    }
  }

  pub fn expect_usize(&self) -> Result<usize, EvalError> {
    match self {
      ICFPExpr::Integer(i) => {
        let value = i.decode().map_err(|e| EvalError::DecodeError {
          message: format!("failed to decode int: {e}"),
          ctx: format!("coded: {i:?}"),
        })?;
        Ok(usize::try_from(&value).unwrap())
      }
      e => Err(EvalError::ExpectedType {
        expected_type: "usize",
        got: format!("Got: {e:?}"),
      }),
    }
  }

  pub fn expect_bool(&self) -> Result<bool, EvalError> {
    match self {
      ICFPExpr::Boolean(b) => Ok(*b),
      e => Err(EvalError::ExpectedType {
        expected_type: "bool",
        got: format!("Got: {e:?}"),
      }),
    }
  }

  pub fn expect_string(&self) -> Result<&str, EvalError> {
    match self {
      ICFPExpr::String(s) => {
        let x = s.decode().map_err(|_e| EvalError::DecodeError {
          message: format!("Failed to decode: {:?}", s),
          ctx: "".to_string(),
        })?;
        Ok(x)
      }
      e => Err(EvalError::ExpectedType {
        expected_type: "string",
        got: format!("Got: {e:?}"),
      }),
    }
  }
}

#[derive(Clone)]
struct LazyExpr {
  once_cell: OnceCell<EvalResult>,
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
    write!(f, "lazy({:?}, {:?})", self.once_cell.get(), self.func.expr)
  }
}

trait LazyInitializer {
  fn eval(&self) -> EvalResult;
}

#[derive(Clone)]
struct DeferredEval {
  expr: ICFPExpr,
  env: Environment,
}

impl LazyInitializer for DeferredEval {
  fn eval(&self) -> EvalResult {
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

  fn get(&self) -> EvalResult {
    self.once_cell.get_or_init(|| self.func.eval()).clone()
  }
}

impl Evaluable for ICFPExpr {
  fn eval(
    &self,
    env: &Environment,
  ) -> EvalResult {
    // info!(env = ?env.vars, name = ?self, "Eval");

    Ok(match self {
      literal @ ICFPExpr::Boolean(_)
      | literal @ ICFPExpr::Integer(_)
      | literal @ ICFPExpr::String(_) => literal.clone(),
      ICFPExpr::UnaryOp(op, operand) => match op {
        UnOp::Negate => {
          let ICFPExpr::Integer(i) = operand.eval(env)? else {
            panic!("Wrong type ")
          };

          ICFPExpr::int(
            i.decode()
              .map_err(|e| EvalError::DecodeError {
                message: format!("failed to decode int: {e}"),
                ctx: format!("coded: {i:?}"),
              })?
              .neg(),
          )
        }
        UnOp::Not => {
          let ICFPExpr::Boolean(b) = operand.eval(env)? else {
            panic!("Wrong type ")
          };

          ICFPExpr::Boolean(!b)
        }
        UnOp::StrToInt => {
          let ICFPExpr::String(s) = operand.eval(env)? else {
            panic!("Wrong type ")
          };

          match s {
            DeferredDecode::Deferred { coded, .. } => {
              ICFPExpr::Integer(DeferredDecode::deferred(&coded))
            }
            DeferredDecode::Lit(lit) => {
              ICFPExpr::Integer(DeferredDecode::Lit(base94_decode(&lit.encode()).unwrap()))
            }
          }
        }
        UnOp::IntToStr => {
          let ICFPExpr::Integer(i) = operand.eval(env)? else {
            panic!("Wrong type {operand:?}")
          };

          match i {
            DeferredDecode::Deferred { coded, .. } => {
              ICFPExpr::String(DeferredDecode::deferred(&coded))
            }
            DeferredDecode::Lit(lit) => ICFPExpr::String(DeferredDecode::Lit(
              String::decode(&base94_encode_number(lit)).unwrap(),
            )),
          }
        }
      },
      ICFPExpr::BinaryOp(op, left, right) => match op {
        BinOp::Add => {
          let left = left
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during add",
              ctx: "left",
              related: vec![e],
            })?;
          let right = right
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during add",
              ctx: "right",
              related: vec![e],
            })?;
          ICFPExpr::int(left + right)
        }
        BinOp::Sub => {
          println!("SubEval: {:?} in {env:?}", left);
          let left = left
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during sub",
              ctx: "left",
              related: vec![e],
            })?;
          let right = right
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during sub",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::int(left - right)
        }
        BinOp::Mul => {
          let left = left
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during mul",
              ctx: "left",
              related: vec![e],
            })?;
          let right = right
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during mul",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::int(left * right)
        }
        BinOp::Div => {
          let left = left
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during div",
              ctx: "left",
              related: vec![e],
            })?;
          let right = right
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during div",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::int(left / right)
        }
        BinOp::Mod => {
          let left = left
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during mod",
              ctx: "left",
              related: vec![e],
            })?;
          let right = right
            .eval(env)?
            .expect_int()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during mod",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::int(left % right)
        }
        BinOp::LessThan => {
          let left = left.eval(env)?.expect_int()?;
          let right = right.eval(env)?.expect_int()?;
          ICFPExpr::Boolean(left < right)
        }
        BinOp::GreaterThan => {
          let left = left.eval(env)?.expect_int()?;
          let right = right.eval(env)?.expect_int()?;
          ICFPExpr::Boolean(left > right)
        }
        BinOp::Equals => {
          let left = left.eval(env)?;
          let right = right.eval(env)?;
          println!("{left:?} == {right:?}");
          ICFPExpr::Boolean(left == right)
        }
        BinOp::Or => {
          let left = left.eval(env)?.expect_bool()?;
          let right = right.eval(env)?.expect_bool()?;
          ICFPExpr::Boolean(left || right)
        }
        BinOp::And => {
          let left = left.eval(env)?.expect_bool()?;
          let right = right.eval(env)?.expect_bool()?;
          ICFPExpr::Boolean(left && right)
        }
        BinOp::Concat => {
          let left = left
            .eval(env)?
            .expect_string()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during concat",
              ctx: "left",
              related: vec![e],
            })?
            .to_string();

          let right_exp = right.eval(env)?;
          let right = right_exp
            .expect_string()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during concat",
              ctx: "left",
              related: vec![e],
            })?;
          ICFPExpr::str(left.add(right))
        }
        BinOp::TakeChars => {
          let evaled_right = right.eval(env)?;
          let string = evaled_right
            .expect_string()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during take_chars",
              ctx: "left",
              related: vec![e],
            })?;

          let take_to = left
            .eval(env)?
            .expect_usize()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during take_chars",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::str(&string[..take_to])
        }
        BinOp::SkipChars => {
          let right_evaled = right.eval(env)?;
          let string = right_evaled
            .expect_string()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during skip_chars",
              ctx: "left",
              related: vec![e],
            })?;

          let start_at = left
            .eval(env)?
            .expect_usize()
            .map_err(|e| EvalError::ErrorContext {
              message: "Error during concat",
              ctx: "right",
              related: vec![e],
            })?;

          ICFPExpr::str(&string[start_at..])
        }
        BinOp::ApplyLambda => {
          let arg_value = right.clone();
          match left.eval(env)? {
            ICFPExpr::Lambda(id, arg_name, body) => {
              let mut environment = env.bind(arg_name, *arg_value);
              println!("Eval Lambda {id}");
              body.eval(&mut environment)?
            }
            ICFPExpr::Closure {
              id: _id,
              arg: arg_name,
              body,
              env: clo_env,
            } => {
              println!();
              println!();

              println!("Eval Closure {_id} (set {arg_name:?}: {right:?})");

              println!("Current Env: {:?}", env);
              println!("Closure Env: {:?}", clo_env);

              let environment = env.bind(arg_name, *arg_value);
              let e = environment.merge(&clo_env, arg_name);

              println!("Merged Env: {environment:?}");
              println!("Final Env: {:?}", e);

              body.eval(&e)?
            }
            e => {
              return Err(EvalError::ExpectedType {
                expected_type: "lambda",
                got: format!("Got: {e:?} from {left:?} in {env:?}"),
              })
            }
          }
        }
      },
      ICFPExpr::If(cond, if_true, if_false) => {
        if cond.eval(env)?.expect_bool()? {
          // println!("true: {cond:?} {env:?}");
          if_true.eval(env)?
        } else {
          // println!("false: {cond:?}");
          if_false.eval(env)?
        }
      }
      ICFPExpr::Lambda(id, arg, body) => ICFPExpr::Closure {
        id: *id,
        arg: *arg,
        body: body.clone(),
        env: env.clone(),
      },
      closure @ ICFPExpr::Closure { .. } => closure.clone(),
      ICFPExpr::VarRef(var) => env
        .vars
        .get(var)
        .ok_or(EvalError::VariableNotInEnvironment {
          variable: format!(
            "Var {var:?} not found in environment {:?}!",
            env.vars.keys()
          ),
        })?
        .get()?,
      ICFPExpr::Unknown { indicator, .. } => {
        unimplemented!("Unknown {indicator} not evaluable")
      }
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::evaluator::{eval, Environment, EvalError, Evaluable};
  use crate::parser::ICFPExpr::VarRef;
  use crate::parser::{BinOp, ICFPExpr, Parsable, Var};

  #[test]
  fn simple_function_application() -> Result<(), EvalError> {
    let f = ICFPExpr::lambda(
      0,
      Var(0),
      ICFPExpr::if_(ICFPExpr::var(0), ICFPExpr::const_true(), ICFPExpr::int(3)),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(f.clone(), ICFPExpr::const_true()).eval(&env)?,
      ICFPExpr::const_true()
    );
    assert_eq!(
      ICFPExpr::call(f, ICFPExpr::const_false()).eval(&env)?,
      ICFPExpr::int(3)
    );

    Ok(())
  }

  #[test]
  fn curried_function() -> Result<(), EvalError> {
    let f = ICFPExpr::lambda(
      0,
      Var(0),
      ICFPExpr::lambda(
        1,
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
      .eval(&env)?,
      ICFPExpr::int(13)
    );
    assert_eq!(
      ICFPExpr::call(
        ICFPExpr::call(f.clone(), ICFPExpr::const_false()),
        ICFPExpr::int(13)
      )
      .eval(&env)?,
      ICFPExpr::int(3)
    );

    Ok(())
  }

  #[test]
  fn modulo() -> Result<(), miette::Report> {
    assert_eq!(
      eval(ICFPExpr::parse("B% U- I( I#").map_err(|e| miette::miette!("Error parsing: {}", e))?)?,
      ICFPExpr::int(-1)
    );

    Ok(())
  }

  #[test]
  fn num_to_string() -> Result<(), miette::Report> {
    assert_eq!(
      eval(ICFPExpr::parse("U$ I4%34").map_err(|e| miette::miette!("Error parsing: {}", e))?)?,
      ICFPExpr::str("test")
    );

    Ok(())
  }

  #[test]
  fn string_to_num() -> Result<(), miette::Report> {
    assert_eq!(
      eval(ICFPExpr::parse("U# S4%34").map_err(|e| miette::miette!("Error parsing: {}", e))?)?,
      ICFPExpr::int(15818151)
    );

    Ok(())
  }

  #[test]
  fn recursion() -> Result<(), EvalError> {
    let var0 = Var(0);
    let var1 = Var(1);
    let var2 = Var(2);
    let var3 = Var(3);
    let var4 = Var(4);

    let fixed = ICFPExpr::lambda(
      1,
      var0,
      ICFPExpr::call(
        ICFPExpr::lambda(2, var1, ICFPExpr::call(var0, ICFPExpr::call(var1, var1))),
        ICFPExpr::lambda(3, var2, ICFPExpr::call(var0, ICFPExpr::call(var2, var2))),
      ),
    );

    let dots = ICFPExpr::lambda(
      4,
      var3,
      ICFPExpr::lambda(
        5,
        var4,
        ICFPExpr::if_(
          ICFPExpr::bin_op(var4, BinOp::Equals, 1),
          ".",
          ICFPExpr::bin_op(".", BinOp::Concat, ICFPExpr::call(var3, VarRef(var4) - 1)),
        ),
      ),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(ICFPExpr::call(fixed, dots), ICFPExpr::int(10)).eval(&env)?,
      "..........".into()
    );

    Ok(())
  }

  #[test]
  fn recursion_env_complex() -> Result<(), EvalError> {
    let var0 = Var(0);
    let var1 = Var(1);
    let var2 = Var(2);
    let var3 = Var(3);
    let var4 = Var(4);

    let fixed = ICFPExpr::lambda(
      1,
      var0,
      ICFPExpr::call(
        ICFPExpr::lambda(2, var1, ICFPExpr::call(var0, ICFPExpr::call(var1, var1))),
        ICFPExpr::lambda(3, var2, ICFPExpr::call(var0, ICFPExpr::call(var2, var2))),
      ),
    );

    let char_func = ICFPExpr::lambda(
      10,
      Var(10),
      ICFPExpr::call(
        fixed,
        ICFPExpr::lambda(
          4,
          var3,
          ICFPExpr::lambda(
            5,
            var4,
            ICFPExpr::if_(
              ICFPExpr::bin_op(var4, BinOp::Equals, 1),
              Var(10),
              ICFPExpr::bin_op(
                Var(10),
                BinOp::Concat,
                ICFPExpr::call(var3, VarRef(var4) - 1),
              ),
            ),
          ),
        ),
      ),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(ICFPExpr::call(char_func, "."), ICFPExpr::int(10)).eval(&env)?,
      "..........".into()
    );

    Ok(())
  }

  #[test]
  fn recursion_complex_var_reuse() -> Result<(), EvalError> {
    let var0 = Var(0);
    let var1 = Var(1);
    let var2 = Var(2);
    let var3 = Var(3);
    let var4 = Var(4);

    let fixed = ICFPExpr::lambda(
      1,
      var0,
      ICFPExpr::call(
        ICFPExpr::lambda(2, var1, ICFPExpr::call(var0, ICFPExpr::call(var1, var1))),
        ICFPExpr::lambda(3, var2, ICFPExpr::call(var0, ICFPExpr::call(var2, var2))),
      ),
    );

    let char_func = ICFPExpr::lambda(
      10,
      var0,
      ICFPExpr::call(
        fixed,
        ICFPExpr::lambda(
          4,
          var3,
          ICFPExpr::lambda(
            5,
            var4,
            ICFPExpr::if_(
              ICFPExpr::bin_op(var4, BinOp::Equals, 1),
              var0,
              ICFPExpr::bin_op(var0, BinOp::Concat, ICFPExpr::call(var3, VarRef(var4) - 1)),
            ),
          ),
        ),
      ),
    );

    let env = Environment::default();

    assert_eq!(
      ICFPExpr::call(ICFPExpr::call(char_func, "."), ICFPExpr::int(10)).eval(&env)?,
      "..........".into()
    );

    Ok(())
  }
}

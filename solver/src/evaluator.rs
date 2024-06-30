use std::borrow::Cow;
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Neg};
use std::sync::Arc;

use miette::{Diagnostic, Report};
use serde::__private::de::Borrowed;
use thiserror::Error;
use tracing::{debug, info, trace};

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
pub(crate) struct LazyExpr {
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
    write!(f, "lazy({:?}, {})", self.once_cell.get(), self.func.expr)
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

trait CallByName: Clone {
  fn bind_by_name(
    &self,
    name: Var,
    expr: &ICFPExpr,
    free_vars: &HashSet<Var>,
  ) -> Cow<Self>;
}

impl CallByName for ICFPExpr {
  fn bind_by_name(
    &self,
    name: Var,
    expr: &ICFPExpr,
    free_vars: &HashSet<Var>
  ) -> Cow<Self> {
    use Cow::*;
    match self {
      noop @ ICFPExpr::Boolean(_) | noop @ ICFPExpr::Integer(_) | noop @ ICFPExpr::String(_) => {
        Borrowed(noop)
      }
      ICFPExpr::UnaryOp(op, body) => match body.bind_by_name(name, expr, free_vars) {
        Borrowed(_) => Borrowed(self),
        Owned(body) => Owned(ICFPExpr::UnaryOp(*op, Box::new(body))),
      },
      ICFPExpr::BinaryOp(op, left, right) => {
        match (
          left.bind_by_name(name, expr, free_vars),
          right.bind_by_name(name, expr, free_vars),
        ) {
          (Owned(left), Owned(right)) => {
            Owned(ICFPExpr::BinaryOp(*op, Box::new(left), Box::new(right)))
          }
          (Borrowed(left), Owned(right)) => Owned(ICFPExpr::BinaryOp(
            *op,
            Box::new(left.clone()),
            Box::new(right),
          )),
          (Owned(left), Borrowed(right)) => Owned(ICFPExpr::BinaryOp(
            *op,
            Box::new(left),
            Box::new(right.clone()),
          )),
          (Borrowed(_), Borrowed(_)) => Borrowed(self),
        }
      }
      ICFPExpr::If(cond, left, right) => {
        match (
          cond.bind_by_name(name, expr, free_vars),
          left.bind_by_name(name, expr, free_vars),
          right.bind_by_name(name, expr, free_vars),
        ) {
          (Owned(cond), Owned(left), Owned(right)) => Owned(ICFPExpr::If(
            Box::new(cond),
            Box::new(left),
            Box::new(right),
          )),
          (Owned(cond), Borrowed(left), Borrowed(right)) => Owned(ICFPExpr::If(
            Box::new(cond),
            Box::new(left.clone()),
            Box::new(right.clone()),
          )),
          (Owned(cond), Borrowed(left), Owned(right)) => Owned(ICFPExpr::If(
            Box::new(cond),
            Box::new(left.clone()),
            Box::new(right),
          )),
          (Owned(cond), Owned(left), Borrowed(right)) => Owned(ICFPExpr::If(
            Box::new(cond),
            Box::new(left),
            Box::new(right.clone()),
          )),
          (Borrowed(cond), Borrowed(left), Owned(right)) => Owned(ICFPExpr::If(
            Box::new(cond.clone()),
            Box::new(left.clone()),
            Box::new(right),
          )),
          (Borrowed(cond), Owned(left), Borrowed(right)) => Owned(ICFPExpr::If(
            Box::new(cond.clone()),
            Box::new(left),
            Box::new(right.clone()),
          )),
          (Borrowed(cond), Owned(left), Owned(right)) => Owned(ICFPExpr::If(
            Box::new(cond.clone()),
            Box::new(left),
            Box::new(right),
          )),
          (Borrowed(_), Borrowed(_), Borrowed(_)) => Borrowed(self),
        }
      }
      ICFPExpr::VarRef(var) => {
        if *var == name {
          Owned(expr.clone())
        } else {
          Borrowed(self)
        }
      }
      ICFPExpr::Lambda(id, arg, body) => {
        if *arg == name || free_vars.contains(arg) {
          Borrowed(self)
        } else {
          match body.bind_by_name(name, &expr, free_vars) {
            Borrowed(_) => Borrowed(self),
            Owned(body) => Owned(ICFPExpr::Lambda(*id, *arg, Box::new(body))),
          }
        }
      }
      ICFPExpr::Closure { .. } => Borrowed(self),
      ICFPExpr::Thunk(_) => Borrowed(self),
      ICFPExpr::Unknown { .. } => Borrowed(self),
    }
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
          // trace!(%left, ?env, "eval subtract");
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
          let result = left == right;
          trace!(result, %left, %right, "equals?");
          ICFPExpr::Boolean(result)
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
          match left.eval(env)? {
            ICFPExpr::Lambda(id, arg_name, body) => {
              //let environment = env.bind(arg_name, *arg_value);
              // let expr = LazyExpr::new((*arg_value).clone(), env.clone());
              // let arc = Arc::new(expr);
              // let val: ICFPExpr =

              debug!(value= %right, "binding {arg_name:?}");
              let mut free_vars = HashSet::new();

              right.collect_free_vars(&mut free_vars);

              let right_simp = right.simplify(env);

              let r = right_simp.as_ref().inspect(|v| {
                info!(new=%v, old=%right, "simplified");
              }).unwrap_or_else(|e| e);

              match body.bind_by_name(arg_name,r, &free_vars) {
                Cow::Borrowed(body) => {
                  trace!(id, %body, "lambda, no rebinding");
                  body.eval(&env)?
                }
                Cow::Owned(new_body) => {
                  trace!(id, arg = %arg_name, before = %body,  after = %new_body, "lambda, rebinding");
                  new_body.eval(&env)?
                }
              }
            }
            ICFPExpr::Closure {
              id: _id,
              arg: arg_name,
              body,
              env: clo_env,
            } => {
              debug!(id=_id, arg = %arg_name, value = %right,"Eval Closure");

              trace!(env = ?env, "Current");
              trace!(env = ?clo_env, "Closure");

              let environment = env.bind(arg_name, *right.clone());
              let e = environment.merge(&clo_env, arg_name);

              trace!(env = ?environment, "Merged");
              trace!(env = ?e, "Final");

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
          trace!("true: {cond} {env:?}");
          if_true.eval(env)?
        } else {
          trace!("false: {cond}");
          if_false.eval(env)?
        }
      }

      // ICFPExpr::Lambda(id, arg, body) => ICFPExpr::Closure {
      //   id: *id,
      //   arg: *arg,
      //   body: body.clone(),
      //   env: env.clone(),
      // },
      lambda @ ICFPExpr::Lambda(_, _, _) => lambda.clone(),
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
      ICFPExpr::Thunk(thunk) => thunk.get()?,
      ICFPExpr::Unknown { indicator, .. } => {
        unimplemented!("Unknown {indicator} not evaluable")
      }
    })
  }
}

impl ICFPExpr {
  fn simplify(&self, env: &Environment) -> EvalResult<&ICFPExpr> {
    use ICFPExpr::*;
    use BinOp::*;
    use Cow::*;
    match self {
      expr@Boolean(_)
      | expr@Integer(_)
      | expr@String(_)
      | expr@ Unknown { .. }
      | expr@Thunk(_) => Ok(expr.clone()),
      UnaryOp(op, expr) => {
        let e = expr.simplify(env).map_err(|_| self)?;
        Ok(UnaryOp(*op, Box::new(e)).eval(env).map_err(|_| self)?)
      },
      BinaryOp(op, left, right) => {
        let left = left.simplify(env).map_err(|_| self)?;
        let right = right.simplify(env).map_err(|_| self)?;
        Ok(BinaryOp(*op, Box::new(left), Box::new(right)).eval(env).map_err(|_| self)?)
      }
      If(cond, t, f) => {
        let cond = cond.simplify(env).map_err(|_| self)?;
        let t = t.simplify(env).map_err(|_| self)?;
        let f = f.simplify(env).map_err(|_| self)?;
        Ok(If(Box::new(cond), Box::new(t), Box::new(f)).eval(env).map_err(|_| self)?)
      }
      Lambda(_, _, _)
      | VarRef(_)
      | Closure { .. } => Err(self)
    }
  }

  fn collect_free_vars(&self, set: &mut HashSet<Var>) {
    use ICFPExpr::*;
    match self {
       Boolean(_)
      | Integer(_)
      | String(_)  => {}
      UnaryOp(_, exp) => exp.collect_free_vars(set),
      BinaryOp(_, left, right) => {
        left.collect_free_vars(set);
        right.collect_free_vars(set);
      }
      If(cond, t, f) => {
        cond.collect_free_vars(set);
        t.collect_free_vars(set);
        f.collect_free_vars(set);
      }
      Closure { arg: var, body, .. }
      | Lambda(_, var, body) => {
        let mut body_set = HashSet::new();
        body.collect_free_vars(&mut body_set);
        body_set.remove(var);
        set.extend(body_set);
      }
      VarRef(v) => {
        set.insert(*v);
      },
      Thunk(_) => {}
      Unknown { .. } => {}
    }
  }
}

#[cfg(test)]
mod tests {
  use tracing_test::traced_test;
  use crate::evaluator::{eval, Environment, EvalError, Evaluable};
  use crate::parser::ICFPExpr::VarRef;
  use crate::parser::{BinOp, ICFPExpr, Parsable, Var};

  #[traced_test]
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

  #[traced_test]
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

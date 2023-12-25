use std::rc::Rc;

use saft_ast::{Expr, Module, Statement};
use saft_common::span::Span;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Integer(i64),
    Float(f64),
}

type VRef = Rc<Value>;

#[derive(Debug)]
pub enum Error {
    Exotic {
        message: &'static str,
        span: Option<Span>,
        note: Option<String>,
    },
}

pub struct Env {}

impl Env {
    pub fn new() -> Self {
        Self {}
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Env) -> Result<VRef, Error>;
}

impl Eval for Statement {
    fn eval(&self, env: &mut Env) -> Result<VRef, Error> {
        match self {
            Statement::Expr(se) => se.v.eval(env),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, _env: &mut Env) -> Result<VRef, Error> {
        match self {
            Expr::Var(_) => todo!(),
            Expr::Integer(i) => Ok(Rc::new(Value::Integer(*i))),
            Expr::Float(f) => Ok(Rc::new(Value::Float(*f))),
        }
    }
}

impl Eval for Module {
    fn eval(&self, env: &mut Env) -> Result<VRef, Error> {
        for stmt in self.stmts.iter() {
            stmt.v.eval(env)?;
        }

        Ok(Rc::new(Value::Nil))
    }
}

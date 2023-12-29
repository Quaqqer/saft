use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Item, Module, Statement};
use saft_common::span::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Val {
    Nil,
    Integer(i64),
    Float(f64),
}

impl Val {
    pub fn type_name(&self) -> String {
        match self {
            Val::Nil => "nil".into(),
            Val::Integer(..) => "integer".into(),
            Val::Float(..) => "float".into(),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Exotic {
        message: String,
        span: Option<Span>,
        note: Option<String>,
    },
    UnresolvedVariable {
        message: String,
        span: Span,
    },
    TypeError {
        message: String,
        span: Span,
    },
}

impl Error {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::Exotic {
                message,
                span,
                note,
            } => {
                let mut diag = Diagnostic::error().with_message(message);
                if let Some(s) = span {
                    let mut label = Label::primary(file_id, s.r.clone());
                    if let Some(n) = note {
                        label = label.with_message(n);
                    }
                    diag = diag.with_labels(vec![label]);
                }
                diag
            }
            Error::UnresolvedVariable { message, span } => Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(file_id, span.r.clone())]),
            Error::TypeError { message, span } => Diagnostic::error()
                .with_message(format!("TypeError: {}", message))
                .with_labels(vec![Label::primary(file_id, span.r.clone())]),
        }
    }
}

pub struct Env {
    scopes: Vec<HashMap<Ident, Val>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn scoped<F, T>(&mut self, f: F) -> T
    where
        F: Fn(&mut Env) -> T,
    {
        self.scopes.push(HashMap::new());
        let res = f(self);
        self.scopes.pop();
        res
    }

    fn lookup(&self, sident: &Spanned<Ident>) -> Result<Val, Error> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(&sident.v) {
                return Ok(v.clone());
            }
        }

        Err(Error::UnresolvedVariable {
            message: "Could not look up variable".into(),
            span: sident.s.clone(),
        })
    }

    fn declare(&mut self, sident: &Spanned<Ident>, v: Val) -> Result<(), Error> {
        self.scopes.last_mut().unwrap().insert(sident.v.clone(), v);
        Ok(())
    }

    fn assign(&mut self, sident: &Spanned<Ident>, v: Val) -> Result<(), Error> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(&sident.v) {
                Some(r) => {
                    *r = v.clone();
                    return Ok(());
                }
                None => {}
            }
        }

        Err(Error::UnresolvedVariable {
            message: "Could not resolve variable when assigning".into(),
            span: sident.s.clone(),
        })
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Env) -> Result<Val, Error>;
}

impl Eval for Statement {
    fn eval(&self, env: &mut Env) -> Result<Val, Error> {
        match self {
            Statement::Expr(se) => se.v.eval(env),
            Statement::Declare { ident, expr } => {
                let res = expr.v.eval(env)?;
                env.declare(ident, res)?;
                Ok(Val::Nil)
            }
            Statement::Item(Item::Fn {
                ident,
                params,
                body,
            }) => todo!(),
        }
    }
}

fn binary_num_promote(
    lhs: &Spanned<Val>,
    rhs: &Spanned<Val>,
    op_name: &'static str,
) -> Result<(Spanned<Val>, Spanned<Val>), Error> {
    match (&lhs.v, &rhs.v) {
        (Val::Integer(_), Val::Integer(_)) => Ok((lhs.clone(), rhs.clone())),
        (Val::Float(_), Val::Float(_)) => Ok((lhs.clone(), rhs.clone())),
        (Val::Integer(a), Val::Float(_)) => Ok((lhs.map(|_| Val::Float(*a as f64)), rhs.clone())),
        (Val::Float(_), Val::Integer(b)) => Ok((lhs.clone(), rhs.map(|_| Val::Float(*b as f64)))),
        _ => Err(Error::TypeError {
            message: format!(
                "Could not perform operation '{}' for operands of types {} and {}",
                op_name,
                lhs.v.type_name(),
                rhs.v.type_name()
            ),
            span: lhs.s.join(&rhs.s),
        }),
    }
}

impl Eval for Expr {
    fn eval(&self, env: &mut Env) -> Result<Val, Error> {
        match self {
            Expr::Var(ident) => env.lookup(ident),
            Expr::Integer(i) => Ok(Val::Integer(*i)),
            Expr::Float(f) => Ok(Val::Float(*f)),
            Expr::Nil => Ok(Val::Nil),
            Expr::Assign(lhs, rhs) => {
                if let Expr::Var(ident) = &lhs.v {
                    let res = rhs.v.eval(env)?;
                    env.assign(ident, res.clone())?;
                    Ok(res)
                } else {
                    Err(Error::Exotic {
                        message: "Cannot assign to a non-variable".into(),
                        span: Some(lhs.s.clone()),
                        note: Some(format!("Found {}", lhs.v.describe())),
                    })
                }
            }
            Expr::Add(lhs, rhs) => {
                let lv = lhs.v.eval(env)?;
                let rv = rhs.v.eval(env)?;

                match (lv, rv) {
                    (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a + b)),
                    _ => Err(Error::Exotic {
                        message: "Binary operation error".into(),
                        span: Some(lhs.s.join(&rhs.s)),
                        note: None,
                    }),
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lv = lhs.v.eval(env)?;
                let rv = rhs.v.eval(env)?;

                match (lv, rv) {
                    (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a - b)),
                    _ => Err(Error::Exotic {
                        message: "Binary operation error".into(),
                        span: Some(lhs.s.join(&rhs.s)),
                        note: None,
                    }),
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lv = lhs.v.eval(env)?;
                let rv = rhs.v.eval(env)?;

                match (lv, rv) {
                    (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a * b)),
                    _ => Err(Error::Exotic {
                        message: "Binary operation error".into(),
                        span: Some(lhs.s.join(&rhs.s)),
                        note: None,
                    }),
                }
            }
            Expr::Div(lhs, rhs) => {
                let lv = lhs.v.eval(env)?;
                let rv = rhs.v.eval(env)?;

                match (lv, rv) {
                    (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a / b)),
                    _ => Err(Error::Exotic {
                        message: "Binary operation error".into(),
                        span: Some(lhs.s.join(&rhs.s)),
                        note: None,
                    }),
                }
            }
            Expr::Pow(lhs, rhs) => {
                let lv = lhs.v.eval(env)?;
                let rv = rhs.v.eval(env)?;

                match (lv, rv) {
                    (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a.pow(b as u32))),
                    _ => Err(Error::Exotic {
                        message: "Binary operation error".into(),
                        span: Some(lhs.s.join(&rhs.s)),
                        note: None,
                    }),
                }
            }
            Expr::Grouping(inner) => inner.v.eval(env),
            Expr::Call(_, _) => todo!(),
            Expr::Neg(expr) => {
                let res = expr.v.eval(env)?;
                match res {
                    Val::Integer(i) => Ok(Val::Integer(-i)),
                    Val::Float(f) => Ok(Val::Float(-f)),
                    _ => Err(Error::Exotic {
                        message: "Cannot negate value".into(),
                        span: Some(expr.s.clone()),
                        note: None,
                    }),
                }
            }
        }
    }
}

impl Eval for Module {
    fn eval(&self, env: &mut Env) -> Result<Val, Error> {
        for stmt in self.stmts.iter() {
            stmt.v.eval(env)?;
        }

        Ok(Val::Nil)
    }
}

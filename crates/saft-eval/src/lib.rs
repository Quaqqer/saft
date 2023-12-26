use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Module, Statement};
use saft_common::span::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Val {
    Nil,
    Integer(i64),
    Float(f64),
}

#[derive(Debug)]
pub enum Error {
    Exotic {
        message: &'static str,
        span: Option<Span>,
        note: Option<String>,
    },
    UnresolvedVariable {
        message: &'static str,
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
                let mut diag = Diagnostic::error().with_message(*message);
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
                .with_message(*message)
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
            message: "Could not look up variable",
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
            message: "Could not resolve variable when assigning",
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
                env.declare(ident, res);
                Ok(Val::Nil)
            }
        }
    }
}

impl Eval for Expr {
    fn eval(&self, env: &mut Env) -> Result<Val, Error> {
        match self {
            Expr::Var(ident) => env.lookup(ident),
            Expr::Integer(i) => Ok(Val::Integer(*i)),
            Expr::Float(f) => Ok(Val::Float(*f)),
            Expr::Nil => Ok(Val::Nil),
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

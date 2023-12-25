use std::{collections::HashMap, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Module, Statement};
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
        }
    }
}

pub struct Env {
    globals: HashMap<Ident, VRef>,
    scopes: Vec<HashMap<Ident, VRef>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            scopes: Vec::new(),
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

    //
    // fn lookup(&self, ident: Ident) -> Result<VRef, Error> {}
    //
    // fn declare(&mut self, ident: Ident, v: VRef) -> Result<(), Error> {}
    //
    // fn assign(&mut self, ident: Ident, v: VRef) -> Result<(), Error> {}
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
            Expr::Nil => Ok(Rc::new(Value::Nil)),
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

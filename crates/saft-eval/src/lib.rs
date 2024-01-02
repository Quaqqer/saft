use std::{collections::HashMap, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Item, Module, Statement};
use saft_common::span::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Val {
    Nil,
    Integer(i64),
    Float(f64),
    Function(Function),
}

impl Val {
    pub fn type_name(&self) -> String {
        match self {
            Val::Nil => "nil".into(),
            Val::Integer(..) => "integer".into(),
            Val::Float(..) => "float".into(),
            Val::Function(..) => "function".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    SaftFunction(SaftFunction),
}

#[derive(Debug, Clone)]
pub struct SaftFunction {
    pub params: Vec<Spanned<String>>,
    pub body: Rc<Vec<Spanned<Statement>>>,
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
    Return {
        v: Val,
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
            Error::Return { .. } => panic!("No diagnostic for returns"),
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

pub fn exec_module(env: &mut Env, module: Module) -> Result<(), Error> {
    for stmt in module.stmts {
        exec_statement(env, &stmt)?;
    }
    Ok(())
}

pub fn exec_statement(env: &mut Env, stmt: &Spanned<Statement>) -> Result<(), Error> {
    match &stmt.v {
        Statement::Expr(se) => eval_expr(env, se).map(|_| ()),
        Statement::Declare { ident, expr } => {
            let res = eval_expr(env, expr)?;
            env.declare(ident, res)?;
            Ok(())
        }
        Statement::Item(Item::Fn {
            ident,
            params,
            body,
        }) => {
            let fun = Val::Function(Function::SaftFunction(SaftFunction {
                params: params.clone(),
                body: Rc::new(body.clone()),
            }));
            env.declare(ident, fun)?;
            Ok(())
        }
        Statement::Return(expr) => {
            let v = eval_expr(env, expr)?;
            Err(Error::Return { v })
        }
    }
}

pub fn eval_expr(env: &mut Env, expr: &Spanned<Expr>) -> Result<Val, Error> {
    let s= expr.s.clone();

    match &expr.v {
        Expr::Var(ident) => env.lookup(ident),
        Expr::Integer(i) => Ok(Val::Integer(*i)),
        Expr::Float(f) => Ok(Val::Float(*f)),
        Expr::Nil => Ok(Val::Nil),
        Expr::Assign(lhs, rhs) => {
            if let Expr::Var(ident) = &lhs.v {
                let res = eval_expr(env, rhs.as_ref())?;
                env.assign(ident, res.clone())?;
                Ok(res)
            } else {
                Err(Error::Exotic {
                    message: "Cannot assign to a non-variable".into(),
                    span: Some(s),
                    note: Some(format!("Found {}", lhs.v.describe())),
                })
            }
        }
        Expr::Add(lhs, rhs) => {
            let lv = eval_expr(env, lhs.as_ref())?;
            let rv = eval_expr(env, rhs.as_ref())?;

            match (lv, rv) {
                (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a + b)),
                _ => Err(Error::Exotic {
                    message: "Binary operation error".into(),
                    span: Some(s),
                    note: None,
                }),
            }
        }
        Expr::Sub(lhs, rhs) => {
            let lv = eval_expr(env, lhs.as_ref())?;
            let rv = eval_expr(env, rhs.as_ref())?;

            match (lv, rv) {
                (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a - b)),
                _ => Err(Error::Exotic {
                    message: "Binary operation error".into(),
                    span: Some(s),
                    note: None,
                }),
            }
        }
        Expr::Mul(lhs, rhs) => {
            let lv = eval_expr(env, lhs.as_ref())?;
            let rv = eval_expr(env, rhs.as_ref())?;

            match (lv, rv) {
                (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a * b)),
                _ => Err(Error::Exotic {
                    message: "Binary operation error".into(),
                    span: Some(s),
                    note: None,
                }),
            }
        }
        Expr::Div(lhs, rhs) => {
            let lv = eval_expr(env, lhs.as_ref())?;
            let rv = eval_expr(env, rhs.as_ref())?;

            match (lv, rv) {
                (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a / b)),
                _ => Err(Error::Exotic {
                    message: "Binary operation error".into(),
                    span: Some(s),
                    note: None,
                }),
            }
        }
        Expr::Pow(lhs, rhs) => {
            let lv = eval_expr(env, lhs.as_ref())?;
            let rv = eval_expr(env, rhs.as_ref())?;

            match (lv, rv) {
                (Val::Integer(a), Val::Integer(b)) => Ok(Val::Integer(a.pow(b as u32))),
                _ => Err(Error::Exotic {
                    message: "Binary operation error".into(),
                    span: Some(s),
                    note: None,
                }),
            }
        }
        Expr::Grouping(inner) => eval_expr(env, inner.as_ref()),
        Expr::Call(f, args) => {
            let fun = eval_expr(env, f.as_ref())?;
            let mut arg_vals = Vec::new();

            for arg in args {
                arg_vals.push(eval_expr(env, arg)?);
            }

            match fun {
                Val::Function(Function::SaftFunction(SaftFunction { params, body })) => {
                    let res: Result<Val, Error> = env.scoped(|env| {
                        for (arg_name, arg) in params.iter().zip(arg_vals.iter()) {
                            env.declare(arg_name, arg.clone())?;
                        }

                        for statement in body.iter() {
                            exec_statement(env, statement)?;
                        }

                        Ok(Val::Nil)
                    });

                    match res {
                        Ok(v) => Ok(v),
                        Err(Error::Return { v }) => Ok(v),
                        Err(e) => Err(e),
                    }
                }
                _ => Err(Error::Exotic {
                    message: "Cannot call non-function".into(),
                    span: Some(s),
                    note: Some(format!("Got type {}", fun.type_name())),
                }),
            }
        }
        Expr::Neg(expr) => {
            let res = eval_expr(env, expr.as_ref())?;
            match res {
                Val::Integer(i) => Ok(Val::Integer(-i)),
                Val::Float(f) => Ok(Val::Float(-f)),
                _ => Err(Error::Exotic {
                    message: "Cannot negate value".into(),
                    span: Some(s),
                    note: None,
                }),
            }
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

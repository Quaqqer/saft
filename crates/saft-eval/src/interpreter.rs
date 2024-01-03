use crate::natives::add_natives;
use crate::value::{Cast, Function, NativeFuncData, Num, SaftFunction, Value};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Item, Module, Statement};
use saft_common::span::{Span, Spanned};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interpreter<IO: InterpreterIO> {
    env: Env,
    io: IO,
}

pub trait InterpreterIO {
    fn print(s: &str) {
        println!("{}", s);
    }
}

pub struct StandardIO {}

impl StandardIO {
    pub fn new() -> Self {
        Self {}
    }
}

impl InterpreterIO for StandardIO {}

impl<IO: InterpreterIO> Interpreter<IO> {
    pub fn new(io: IO) -> Self {
        Self {
            env: Env::new(),
            io,
        }
    }

    fn scoped<F, T>(&mut self, f: F) -> T
    where
        F: Fn(&mut Self) -> T,
    {
        self.env.scopes.push(HashMap::new());
        let res = f(self);
        self.env.scopes.pop();
        res
    }

    pub fn exec_module(&mut self, module: Module) -> Result<(), Error> {
        for stmt in module.stmts.iter() {
            self.exec_statement(stmt)?;
        }

        Ok(())
    }

    pub fn exec_statement(&mut self, stmt: &Spanned<Statement>) -> Result<(), Error> {
        match &stmt.v {
            Statement::Expr(se) => self.eval_expr(se).map(|_| ()),
            Statement::Declare { ident, expr } => {
                let res = self.eval_expr(expr)?;
                self.env.declare(ident, res);
                Ok(())
            }
            Statement::Item(Item::Fn {
                ident,
                params,
                body,
            }) => {
                let fun = Value::Function(Function::SaftFunction(SaftFunction {
                    params: params.clone(),
                    body: Rc::new(body.clone()),
                }));
                self.env.declare(ident, fun);
                Ok(())
            }
            Statement::Return(expr) => {
                let v = self.eval_expr(expr)?;
                Err(Error::Return(v))
            }
        }
    }

    pub fn eval_expr(&mut self, expr: impl Borrow<Spanned<Expr>>) -> Result<Value, Error> {
        let expr = expr.borrow();

        let s = expr.s.clone();

        match &expr.v {
            Expr::Var(ident) => self.env.lookup(ident),
            Expr::Integer(i) => Ok((*i).into()),
            Expr::Float(f) => Ok((*f).into()),
            Expr::Nil => Ok(Value::Nil),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Assign(lhs, rhs) => {
                if let Expr::Var(ident) = &lhs.v {
                    let res = self.eval_expr(rhs.as_ref())?;
                    self.env.assign(ident, res.clone())?;
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
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Ok(Value::Num(lhs.add(rhs)))
            }
            Expr::Sub(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Ok(Value::Num(lhs.sub(rhs)))
            }
            Expr::Mul(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Ok(Value::Num(lhs.mul(rhs)))
            }
            Expr::Div(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Ok(Value::Num(lhs.div(rhs)))
            }
            Expr::Pow(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Ok(Value::Num(lhs.pow(rhs)))
            }
            Expr::Grouping(inner) => self.eval_expr(inner.as_ref()),
            Expr::Call(f, args) => {
                let fun = self.eval_expr(f.as_ref())?;
                let mut arg_vals = Vec::new();

                for arg in args {
                    arg_vals.push(self.eval_expr(arg)?);
                }

                match fun {
                    Value::Function(Function::SaftFunction(SaftFunction { params, body })) => {
                        let res: Result<Value, Error> = self.scoped(|interpreter| {
                            for (arg_name, arg) in params.iter().zip(arg_vals.iter()) {
                                interpreter.env.declare(arg_name, arg.clone());
                            }

                            for statement in body.iter() {
                                interpreter.exec_statement(statement)?;
                            }

                            Ok(Value::Nil)
                        });

                        match res {
                            Ok(v) => Ok(v),
                            Err(Error::Return(v)) => Ok(v),
                            Err(e) => Err(e),
                        }
                    }
                    Value::Function(Function::NativeFunction(NativeFuncData { f, .. })) => {
                        f(arg_vals)
                    }
                    _ => Err(Error::Exotic {
                        message: "Cannot call non-function".into(),
                        span: Some(s),
                        note: Some(format!("Got type {}", fun.type_name())),
                    }),
                }
            }
            Expr::Neg(expr) => Ok(Value::Num(
                Cast::<Num>::cast(&self.eval_expr(expr.as_ref())?)?.neg(),
            )),
        }
    }
}

pub struct Env {
    scopes: Vec<HashMap<Ident, Value>>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Self {
            scopes: vec![HashMap::new()],
        };
        add_natives(&mut env);
        env
    }

    fn lookup(&self, sident: &Spanned<Ident>) -> Result<Value, Error> {
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

    pub fn declare(&mut self, sident: &Spanned<Ident>, v: Value) {
        self.scopes.last_mut().unwrap().insert(sident.v.clone(), v);
    }

    pub fn declare_unspanned(&mut self, ident: &Ident, v: Value) {
        self.scopes.last_mut().unwrap().insert(ident.clone(), v);
    }

    pub fn assign(&mut self, sident: &Spanned<Ident>, v: Value) -> Result<(), Error> {
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
    Return(Value),
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

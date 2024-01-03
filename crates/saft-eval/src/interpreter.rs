use crate::natives::add_natives;
use crate::value::{Cast, Function, NativeFuncData, Num, SaftFunction, Value};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Expr, Ident, Item, Module, Statement};
use saft_common::span::{Span, Spanned};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

#[macro_export]
macro_rules! exotic {
    ($msg:expr) => {
        Exception::Exotic {
            message: $msg.into(),
            span: None,
            note: None,
        }
        .into()
    };

    ($msg:expr, $span:expr) => {
        Exception::Exotic {
            message: $msg.into(),
            span: Some($span),
            note: None,
        }
        .into()
    };

    ($msg:expr, $span:expr, $note:expr) => {
        Exception::Exotic {
            message: $msg.into(),
            span: Some($span),
            note: Some($note),
        }
        .into()
    };
}

#[macro_export]
macro_rules! cast_error {
    ($got:expr, $expected:expr) => {
        Exception::Exotic {
            message: "Cast error".into(),
            span: Some($got.s.clone()),
            note: Some(format!(
                "Cannot cast {} into {}",
                $got.v.ty().name(),
                $expected
            )),
        }
        .into()
    };
}

#[macro_export]
macro_rules! type_error {
    ($got:expr, $expected:expr) => {
        Exception::TypeError {
            span: $got.s,
            note: format!("expected {} but got {}", $expected, $got.v.type_desc()),
        }
    };
}

#[macro_export]
macro_rules! unresolved_error {
    ($var:expr, $span:expr) => {
        Exception::UnresolvedVariable {
            span: $span.clone(),
            note: format!(
                "Variable '{}' could not be found in the current scope",
                $var
            ),
        }
        .into()
    };
}

pub struct Interpreter<IO: InterpreterIO> {
    env: Env,
    pub io: IO,
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

    pub fn exec_module(&mut self, module: Module) -> Result<(), Exception> {
        for stmt in module.stmts.iter() {
            self.exec_outer_statement(stmt)?;
        }

        Ok(())
    }

    pub fn exec_outer_statement(&mut self, stmt: &Spanned<Statement>) -> Result<(), Exception> {
        self.exec_statement(stmt).map_err(|e| match e {
            ControlFlow::Return(_) => exotic!("Cannot return from outer scope"),
            ControlFlow::Exception(ex) => ex,
        })
    }

    fn exec_statement(&mut self, stmt: &Spanned<Statement>) -> Result<(), ControlFlow> {
        match &stmt.v {
            Statement::Expr(se) => self.eval_expr(se).map(|_| ()),
            Statement::Declare { ident, expr } => {
                let res = self.eval_expr(expr)?;
                self.env.declare(ident, res.v);
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
                Err(ControlFlow::Return(v.v))
            }
        }
    }

    pub fn eval_outer_expr(
        &mut self,
        expr: impl Borrow<Spanned<Expr>>,
    ) -> Result<Spanned<Value>, Exception> {
        self.eval_expr(expr).map_err(|e| match e {
            ControlFlow::Return(_) => exotic!("Cannot return from outer scope"),
            ControlFlow::Exception(ex) => ex,
        })
    }

    pub fn eval_expr(
        &mut self,
        expr: impl Borrow<Spanned<Expr>>,
    ) -> Result<Spanned<Value>, ControlFlow> {
        let expr = expr.borrow();

        let s = expr.s.clone();

        Ok(s.spanned(match &expr.v {
            Expr::Var(ident) => self.env.lookup(ident)?,
            Expr::Integer(i) => (*i).into(),
            Expr::Float(f) => (*f).into(),
            Expr::Nil => Value::Nil,
            Expr::String(s) => Value::String(s.clone()),
            Expr::Assign(lhs, rhs) => {
                if let Expr::Var(ident) = &lhs.v {
                    let res = self.eval_expr(rhs.as_ref())?;
                    self.env.assign(ident, res.v.clone())?;
                    res.v
                } else {
                    return Err(exotic!(
                        "Cannot assign to a non-variable",
                        s,
                        format!("Found {}", lhs.v.describe())
                    ));
                }
            }
            Expr::Add(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Value::Num(lhs.add(rhs))
            }
            Expr::Sub(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Value::Num(lhs.sub(rhs))
            }
            Expr::Mul(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Value::Num(lhs.mul(rhs))
            }
            Expr::Div(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Value::Num(lhs.div(rhs))
            }
            Expr::Pow(lhs, rhs) => {
                let lhs: Num = self.eval_expr(lhs.as_ref())?.cast()?;
                let rhs: Num = self.eval_expr(rhs.as_ref())?.cast()?;

                Value::Num(lhs.pow(rhs))
            }
            Expr::Grouping(inner) => self.eval_expr(inner.as_ref())?.v,
            Expr::Call(f, args) => {
                let fun = self.eval_expr(f.as_ref())?;
                let mut arg_vals = Vec::new();

                for arg in args {
                    arg_vals.push(self.eval_expr(arg)?);
                }

                match fun.v {
                    Value::Function(Function::SaftFunction(SaftFunction { params, body })) => {
                        match self.scoped(|interpreter| {
                            if arg_vals.len() != params.len() {
                                return Err(Exception::ArgMismatch {
                                    span: s.clone(),
                                    expected: params.len(),
                                    got: args.len(),
                                }
                                .into());
                            }

                            for (arg_name, arg) in params.iter().zip(arg_vals.iter()) {
                                interpreter.env.declare(arg_name, arg.v.clone());
                            }

                            for statement in body.iter() {
                                interpreter.exec_statement(statement)?;
                            }

                            Ok(Value::Nil)
                        }) {
                            Ok(v) => v,
                            Err(ControlFlow::Return(v)) => v,
                            Err(e) => return Err(e),
                        }
                    }
                    Value::Function(Function::NativeFunction(NativeFuncData { f, .. })) => {
                        f(&s, arg_vals)?
                    }
                    _ => {
                        return Err(exotic!(
                            "Cannot call non-function",
                            s,
                            format!("Got type {}", fun.v.ty().name())
                        ))
                    }
                }
            }
            Expr::Neg(expr) => Value::Num(Cast::<Num>::cast(self.eval_expr(expr.as_ref())?)?.neg()),
            Expr::Index(expr, index) => {
                let expr = self.eval_expr(expr.as_ref())?;
                let index = self.eval_expr(index.as_ref())?;

                match expr.v {
                    Value::String(str) => match index.v {
                        Value::Num(Num::Int(i)) => match str.get(i as usize..i as usize + 1) {
                            Some(c) => Value::String(c.to_string()),
                            None => {
                                return Err(exotic!(
                                    "Indexed out of bounds",
                                    s,
                                    format!(
                                        "Got index {} and the size of the string is {}",
                                        i,
                                        str.len()
                                    )
                                ))
                            }
                        },
                        v => {
                            return Err(Exception::TypeError {
                                span: s,
                                note: format!("Cannot index into a string with {}", v.ty().name()),
                            }
                            .into())
                        }
                    },

                    _ => {
                        return Err(Exception::TypeError {
                            span: s,
                            note: format!("Cannot index into {}", expr.v.ty().name()),
                        }
                        .into())
                    }
                }
            }
        }))
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

    fn lookup(&self, sident: &Spanned<Ident>) -> Result<Value, ControlFlow> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(&sident.v) {
                return Ok(v.clone());
            }
        }

        Err(unresolved_error!(sident.v, sident.s))
    }

    pub fn declare(&mut self, sident: &Spanned<Ident>, v: Value) {
        self.scopes.last_mut().unwrap().insert(sident.v.clone(), v);
    }

    pub fn declare_unspanned(&mut self, ident: &Ident, v: Value) {
        self.scopes.last_mut().unwrap().insert(ident.clone(), v);
    }

    pub fn assign(&mut self, sident: &Spanned<Ident>, v: Value) -> Result<(), ControlFlow> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.get_mut(&sident.v) {
                Some(r) => {
                    *r = v.clone();
                    return Ok(());
                }
                None => {}
            }
        }

        Err(unresolved_error!(sident.v, sident.s))
    }
}

pub enum ControlFlow {
    Return(Value),
    Exception(Exception),
}

pub enum Exception {
    Exotic {
        message: String,
        span: Option<Span>,
        note: Option<String>,
    },
    UnresolvedVariable {
        span: Span,
        note: String,
    },
    TypeError {
        span: Span,
        note: String,
    },
    ArgMismatch {
        span: Span,
        expected: usize,
        got: usize,
    },
}

impl From<Exception> for ControlFlow {
    fn from(value: Exception) -> Self {
        ControlFlow::Exception(value)
    }
}

impl Exception {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Exception::Exotic {
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
            Exception::UnresolvedVariable { span, note } => Diagnostic::error()
                .with_message("Unresolved variable")
                .with_labels(vec![
                    Label::primary(file_id, span.r.clone()).with_message(note)
                ]),
            Exception::TypeError { span, note } => Diagnostic::error()
                .with_message("Type error")
                .with_labels(vec![
                    Label::primary(file_id, span.r.clone()).with_message(note)
                ]),
            Exception::ArgMismatch {
                span,
                expected,
                got,
            } => Diagnostic::error()
                .with_message("Argument mismatch")
                .with_labels(vec![Label::primary(file_id, span.r.clone()).with_message(
                    format!("Expected {} arguments, but got {}", expected, got),
                )]),
        }
    }
}

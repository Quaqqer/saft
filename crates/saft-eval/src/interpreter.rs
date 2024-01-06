use crate::natives::add_natives;
use crate::value::{Cast, Function, NativeFuncData, Num, SaftFunction, Value, ValueType};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast::{Block, Expr, Ident, Item, Module, Statement};
use saft_common::span::{Span, Spanned};
use std::borrow::Borrow;
use std::collections::HashMap;

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
    ($msg:expr, $s:expr) => {
        Exception::TypeError {
            span: $s.clone(),
            note: $msg,
        }
        .into()
    };
}

#[macro_export]
macro_rules! type_expected_error {
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

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
}

#[derive(Default)]
pub struct StandardIO {}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Env::default(),
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
            ControlFlow::Break(_) => exotic!("Cannot break from outside of loops"),
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
                    body: body.clone(),
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
            ControlFlow::Break(_) => exotic!("Cannot break from outer scope"),
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
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => Value::Num(a.add(b)),
                    (Value::String(a), Value::String(b)) => Value::String(a.to_owned() + b),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot add values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => Value::Num(a.sub(b)),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot subtract values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => Value::Num(a.mul(b)),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot multiply values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => Value::Num(a.div(b)),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot divide values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::IDiv(lhs, rhs) => {
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b))
                        if let Some(a) = a.cast_int()
                            && let Some(b) = b.cast_int() =>
                    {
                        (a / b).into()
                    }
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot integer divide values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Pow(lhs, rhs) => {
                let lhs = self.eval_expr(lhs.as_ref())?;
                let rhs = self.eval_expr(rhs.as_ref())?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => Value::Num(a.pow(b)),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot divide values of types {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Grouping(inner) => self.eval_expr(inner.as_ref())?.v,
            Expr::Call(box f, args) => {
                let fun = self.eval_expr(f)?;
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

                            interpreter.eval_block(&body.v)
                        }) {
                            Ok(v) => v,
                            Err(ControlFlow::Return(v)) => v,
                            Err(e) => return Err(e),
                        }
                    }
                    Value::Function(Function::NativeFunction(NativeFuncData { f, .. })) => {
                        f(self, &s, arg_vals)?
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
            Expr::Neg(expr) => {
                let v = self.eval_expr(expr.as_ref())?;
                Value::Num(
                    Cast::<Num>::cast(v.v.clone())
                        .ok_or::<Exception>(cast_error!(v, "numeric"))?
                        .neg(),
                )
            }
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
                    Value::Vec(vals) => match index.v {
                        Value::Num(Num::Int(i)) => match vals.get(i as usize..i as usize + 1) {
                            Some(v) => v[0].clone(),
                            None => {
                                return Err(exotic!(
                                    "Indexed out of bounds",
                                    s,
                                    format!(
                                        "Got index {} and the size of the string is {}",
                                        i,
                                        vals.len()
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
            Expr::Vec(exprs) => {
                let mut vals = Vec::new();

                for expr in exprs.iter() {
                    vals.push(self.eval_expr(expr)?.into());
                }

                Value::Vec(vals)
            }
            Expr::Bool(b) => Value::Num(Num::Bool(*b)),
            Expr::And(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(Num::Bool(a)), Value::Num(Num::Bool(b))) => (*a && *b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform and operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Or(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(Num::Bool(a)), Value::Num(Num::Bool(b))) => (*a || *b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform or operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Lt(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => a.lt(b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform less than operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Le(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => a.le(b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform less or equal operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Gt(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => a.gt(b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform greater than operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Ge(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => a.ge(b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform greater than operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }

            Expr::Eq(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => a.eq(b).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform equality operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }

            Expr::Ne(box lhs, box rhs) => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (&lhs.v, &rhs.v) {
                    (Value::Num(a), Value::Num(b)) => (!a.eq(b)).into(),
                    _ => {
                        return Err(type_error!(
                            format!(
                                "Cannot perform inequality operation between {} and {}",
                                lhs.v.ty().name(),
                                rhs.v.ty().name()
                            ),
                            s
                        ))
                    }
                }
            }
            Expr::Not(box expr) => {
                let v = self.eval_expr(expr)?;
                Value::Num(Num::Bool(
                    !(Cast::<bool>::cast(v.v.clone())
                        .ok_or::<Exception>(cast_error!(v, "numeric"))?),
                ))
            }
            Expr::Block(Spanned {
                v: Block { stmts, tail },
                ..
            }) => self.scoped(|interpreter| {
                for stmt in stmts {
                    interpreter.exec_statement(stmt)?;
                }

                match tail {
                    Some(box expr) => Ok::<_, ControlFlow>(interpreter.eval_expr(expr)?.v),
                    None => Ok(Value::Nil),
                }
            })?,
            Expr::If(box condition, body, else_) => {
                let condition = self.eval_expr(condition)?;
                let enter = match Cast::<bool>::cast(condition.clone()) {
                    Some(b) => b,
                    None => return Err(cast_error!(condition, ValueType::Bool.name())),
                };

                if enter {
                    self.eval_block(&body.v)?
                } else if let Some(box expr) = else_ {
                    self.eval_expr(expr)?.v
                } else {
                    Value::Nil
                }
            }
            Expr::Loop(stmts) => 'outer: loop {
                for stmt in stmts {
                    match self.exec_statement(stmt) {
                        Err(ControlFlow::Break(v)) => break 'outer v,
                        v => v,
                    }?;
                }
            },
            Expr::Break(box expr) => {
                let expr = self.eval_expr(expr)?;
                return Err(ControlFlow::Break(expr.v));
            }
        }))
    }

    fn eval_block(&mut self, block: impl Borrow<Block>) -> Result<Value, ControlFlow> {
        let Block { stmts, tail } = block.borrow();

        self.scoped(|interpreter| {
            for stmt in stmts {
                interpreter.exec_statement(stmt)?;
            }

            match tail {
                Some(box expr) => Ok::<_, ControlFlow>(interpreter.eval_expr(expr)?.v),
                None => Ok(Value::Nil),
            }
        })
    }

    pub fn print_env(&self) {
        let mut lines = Vec::new();
        for scope in self.env.scopes.iter() {
            let mut line = Vec::<String>::new();

            for (k, v) in scope.iter() {
                line.push(format!("{}: {}", k, v.repr()));
            }

            lines.push(line);
        }

        println!(
            "{}",
            lines
                .iter()
                .enumerate()
                .map(|(i, line)| format!("{}: {}", i, &line.join(", ")))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Env {
    scopes: Vec<HashMap<Ident, Value>>,
}

impl Env {
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
            if let Some(r) = scope.get_mut(&sident.v) {
                *r = v.clone();
                return Ok(());
            }
        }

        Err(unresolved_error!(sident.v, sident.s))
    }
}

impl Default for Env {
    fn default() -> Self {
        let mut env = Self {
            scopes: vec![HashMap::new()],
        };
        add_natives(&mut env);
        env
    }
}

pub enum ControlFlow {
    Return(Value),
    Break(Value),
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

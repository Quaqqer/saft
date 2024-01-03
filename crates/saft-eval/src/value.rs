use crate::{
    cast_error,
    interpreter::{ControlFlow, Exception},
};
use std::{borrow::Borrow, rc::Rc};

use saft_ast::Statement;
use saft_common::span::Spanned;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Num(Num),
    Function(Function),
    String(String),
}

impl Value {
    pub fn repr(&self) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Num(num) => num.repr(),
            Value::Function(Function::SaftFunction(..)) => "<function>".into(),
            Value::Function(Function::NativeFunction(..)) => "<builtin function>".into(),
            Value::String(s) => format!("\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Num {
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl Num {
    pub fn repr(&self) -> String {
        match self {
            Num::Bool(true) => "true".into(),
            Num::Bool(false) => "false".into(),
            Num::Int(i) => format!("{}", i),
            Num::Float(f) => format!("{:?}", f),
        }
    }

    pub fn cast_float(&self) -> f64 {
        match self {
            Num::Bool(b) => *b as i64 as f64,
            Num::Int(i) => *i as f64,
            Num::Float(f) => *f,
        }
    }
}

impl std::fmt::Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Num::Bool(true) => write!(f, "true"),
            Num::Bool(false) => write!(f, "false"),
            Num::Int(v) => write!(f, "{}", v),
            Num::Float(v) => write!(f, "{:?}", v),
        }
    }
}

fn bin_promote(lhs: &Num, rhs: &Num) -> (Num, Num) {
    match (lhs, rhs) {
        (Num::Int(_), Num::Int(_)) | (Num::Float(_), Num::Float(_)) => (lhs.clone(), rhs.clone()),

        (Num::Bool(a), Num::Bool(b)) => (Num::Int(*a as i64), Num::Int(*b as i64)),

        (Num::Int(_), Num::Bool(b)) => (lhs.clone(), Num::Int(*b as i64)),
        (Num::Float(_), Num::Bool(b)) => (lhs.clone(), Num::Float(*b as i64 as f64)),
        (Num::Float(_), Num::Int(b)) => (lhs.clone(), Num::Float(*b as f64)),

        (Num::Int(a), Num::Float(_)) => (Num::Float(*a as f64), rhs.clone()),
        (Num::Bool(a), Num::Float(_)) => (Num::Float(*a as i64 as f64), rhs.clone()),
        (Num::Bool(a), Num::Int(_)) => (Num::Int(*a as i64), rhs.clone()),
    }
}

impl Num {
    pub fn add(&self, rhs: impl Borrow<Num>) -> Num {
        match bin_promote(self, rhs.borrow()) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a + b),
            _ => unreachable!(),
        }
    }

    pub fn sub(&self, rhs: impl Borrow<Num>) -> Num {
        match bin_promote(self, rhs.borrow()) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a - b),
            _ => unreachable!(),
        }
    }

    pub fn mul(&self, rhs: impl Borrow<Num>) -> Num {
        match bin_promote(self, rhs.borrow()) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a * b),
            _ => unreachable!(),
        }
    }

    pub fn div(&self, rhs: impl Borrow<Num>) -> Num {
        match bin_promote(self, rhs.borrow()) {
            (Num::Int(a), Num::Int(b)) => Num::Float(a as f64 / b as f64),
            (Num::Float(a), Num::Float(b)) => Num::Float(a as f64 / b as f64),
            _ => unreachable!(),
        }
    }

    pub fn pow(&self, rhs: impl Borrow<Num>) -> Num {
        match bin_promote(self, rhs.borrow()) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a.pow(b as u32)),
            (Num::Float(a), Num::Float(b)) => Num::Float(a.powf(b)),
            _ => unreachable!(),
        }
    }

    pub fn neg(&self) -> Num {
        match self {
            Num::Bool(a) => Num::Int(-(*a as i64)),
            Num::Int(a) => Num::Int(-*a),
            Num::Float(a) => Num::Float(-*a),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Num(Num::Bool(_)) => "bool".into(),
            Value::Num(Num::Int(_)) => "int".into(),
            Value::Num(Num::Float(_)) => "float".into(),
            Value::Function(..) => "function".into(),
            Value::String(..) => "string".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    SaftFunction(SaftFunction),
    NativeFunction(NativeFuncData),
}

#[derive(Debug, Clone)]
pub struct SaftFunction {
    pub params: Vec<Spanned<String>>,
    pub body: Rc<Vec<Spanned<Statement>>>,
}

#[derive(Clone, Debug)]
pub struct NativeFuncData {
    pub name: &'static str,
    pub f: fn(Vec<Spanned<Value>>) -> Result<Value, ControlFlow>,
}

pub trait Cast<T> {
    fn cast(&self) -> Result<T, ControlFlow>;
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(Num::Float(value))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Num(Num::Int(value))
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}

impl From<Num> for Value {
    fn from(value: Num) -> Self {
        Value::Num(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

pub struct NativeRes(pub Result<Value, ControlFlow>);

impl<T: Into<Value>> From<T> for NativeRes {
    fn from(value: T) -> Self {
        NativeRes(Ok(value.into()))
    }
}

impl<V: Into<Value>, C: Into<ControlFlow>> From<Result<V, C>> for NativeRes {
    fn from(value: Result<V, C>) -> Self {
        NativeRes(value.map(|v| v.into()).map_err(|e| e.into()))
    }
}

impl Cast<f64> for Spanned<Value> {
    fn cast(&self) -> Result<f64, ControlFlow> {
        match &self.v {
            Value::Num(Num::Float(f)) => Ok(*f),
            _ => Err(cast_error!(self, "float")),
        }
    }
}

impl Cast<i64> for Spanned<Value> {
    fn cast(&self) -> Result<i64, ControlFlow> {
        match &self.v {
            Value::Num(Num::Int(i)) => Ok(*i),
             _=> Err(cast_error!(self, "integer")),
        }
    }
}

impl Cast<Num> for Spanned<Value> {
    fn cast(&self) -> Result<Num, ControlFlow> {
        match &self.v {
            Value::Num(num) => Ok(num.clone()),
            _ => Err(cast_error!(self, "numeric")),
        }
    }
}

impl Cast<String> for Spanned<Value> {
    fn cast(&self) -> Result<String, ControlFlow> {
        match &self.v {
            Value::String(s) => Ok(s.clone()),
            _ => Err(cast_error!(self, "string")),
        }
    }
}

impl<T: Clone> Cast<T> for Spanned<T> {
    fn cast(&self) -> Result<T, ControlFlow> {
        Ok(self.v.clone())
    }
}

pub trait NativeFunc {
    fn data() -> NativeFuncData;
}

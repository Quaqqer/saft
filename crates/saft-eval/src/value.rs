use crate::interpreter::Error;
use saft_macro::native_function;
use std::rc::Rc;

use saft_ast::Statement;
use saft_common::span::Spanned;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Integer(i64),
    Float(f64),
    Function(Function),
}

impl Value {
    pub fn type_name(&self) -> String {
        use Value::*;
        match self {
            Nil => "nil".into(),
            Integer(..) => "integer".into(),
            Float(..) => "float".into(),
            Function(..) => "function".into(),
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
    pub f: fn(Vec<Value>) -> Result<Value, Error>,
}

pub trait Cast<T> {
    fn cast(&self) -> Result<T, Error>;
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Nil
    }
}

impl Cast<f64> for Value {
    fn cast(&self) -> Result<f64, Error> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(Error::Exotic {
                message: "Cannot cast".into(),
                span: None,
                note: None,
            }),
        }
    }
}

impl Cast<i64> for Value {
    fn cast(&self) -> Result<i64, Error> {
        match self {
            Value::Integer(i) => Ok(*i),
            _ => Err(Error::Exotic {
                message: "Cannot cast".into(),
                span: None,
                note: None,
            }),
        }
    }
}

impl Cast<Value> for Value {
    fn cast(&self) -> Result<Value, Error> {
        Ok(self.clone())
    }
}

pub trait NativeFunc: std::fmt::Debug {
    fn data() -> NativeFuncData;
}

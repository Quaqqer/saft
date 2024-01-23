use std::fmt::Write;
use std::rc::Rc;

use saft_common::span::Span;

use crate::{chunk::Chunk, num::Num, vm};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Num(Num),
    Function(Function),
    Vec(Vec<Value>),
}

#[derive(Debug, Clone)]
pub enum Function {
    SaftFunction(SaftFunction),
    NativeFunction(NativeFunction),
}

#[derive(Debug, Clone)]
pub struct SaftFunction {
    pub arity: usize,
    pub chunk: Rc<Chunk>,
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub f: fn(&mut vm::Vm, Vec<Value>, Span) -> Result<Value, vm::Error>,
}

impl Value {
    pub fn or(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => a.or(b),
            _ => None,
        }
    }

    pub fn and(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => a.and(b),
            _ => None,
        }
    }

    pub fn eq(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.eq(b)),
            _ => None,
        }
    }

    pub fn ne(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.ne(b)),
            _ => None,
        }
    }

    pub fn lt(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.lt(b)),
            _ => None,
        }
    }

    pub fn le(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.le(b)),
            _ => None,
        }
    }

    pub fn gt(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.gt(b)),
            _ => None,
        }
    }

    pub fn ge(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.ge(b)),
            _ => None,
        }
    }

    pub fn mul(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.mul(b).into()),
            _ => None,
        }
    }

    pub fn div(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.div(b).into()),
            _ => None,
        }
    }

    pub fn idiv(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.idiv(b).into()),
            _ => None,
        }
    }

    pub fn add(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.add(b).into()),
            _ => None,
        }
    }

    pub fn sub(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.sub(b).into()),
            _ => None,
        }
    }

    pub fn pow(&self, rhs: &Value) -> Option<Value> {
        match (self, rhs) {
            (Value::Num(a), Value::Num(b)) => Some(a.pow(b).into()),
            _ => None,
        }
    }

    pub fn not(&self) -> Option<Value> {
        match self {
            Value::Num(num) => num.not().map(Into::into),
            _ => None,
        }
    }

    pub fn ty(&self) -> ValueType {
        match self {
            Value::Nil => ValueType::Nil,
            Value::Num(Num::Bool(_)) => ValueType::Bool,
            Value::Num(Num::Int(_)) => ValueType::Int,
            Value::Num(Num::Float(_)) => ValueType::Float,
            Value::Function(_) => ValueType::Function,
            Value::Vec(_) => ValueType::Vec,
        }
    }

    pub fn neg(&self) -> Option<Value> {
        match self {
            Value::Num(num) => Some(num.neg().into()),
            _ => None,
        }
    }

    pub fn repr(&self) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Num(num) => num.repr(),
            Value::Function(_) => "<function>".into(),
            Value::Vec(vec) => {
                let mut buf = String::new();
                write!(buf, "[").unwrap();
                for (i, v) in vec.iter().enumerate() {
                    if i != 0 {
                        write!(buf, ", ").unwrap();
                    }
                    write!(buf, "{}", v.repr()).unwrap();
                }
                write!(buf, "]").unwrap();
                buf
            }
        }
    }

    pub fn index<'a>(&'a self, index: &Value) -> IndexRes<'a> {
        match self {
            Value::Vec(vec) => match index {
                Value::Num(Num::Int(i)) => {
                    let Ok(i): Result<usize, _> = (*i).try_into() else {
                        return IndexRes::OutOfBounds;
                    };

                    vec.get(i)
                        .map(IndexRes::Value)
                        .unwrap_or(IndexRes::OutOfBounds)
                }
                _ => IndexRes::Unindexable,
            },
            _ => IndexRes::Unindexable,
        }
    }

    pub fn index_assign(&self, _index: &Value, _value: Value) -> bool {
        false
    }
}

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Value::Nil
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(value.into())
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Num(value.into())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Num(value.into())
    }
}

impl From<Num> for Value {
    fn from(value: Num) -> Self {
        Value::Num(value)
    }
}

pub trait Cast<T> {
    fn name() -> String;
    fn cast(&self) -> Option<T>;
}

impl Cast<Value> for Value {
    fn name() -> String {
        "value".into()
    }

    fn cast(&self) -> Option<Value> {
        Some(self.clone())
    }
}

impl Cast<Num> for Value {
    fn name() -> String {
        "numeric".into()
    }

    fn cast(&self) -> Option<Num> {
        match self {
            Value::Num(num) => Some(*num),
            _ => None,
        }
    }
}

impl Cast<bool> for Value {
    fn name() -> String {
        "bool".into()
    }

    fn cast(&self) -> Option<bool> {
        Cast::<Num>::cast(self).and_then(|v| v.cast())
    }
}

impl Cast<i64> for Value {
    fn name() -> String {
        "bool".into()
    }

    fn cast(&self) -> Option<i64> {
        Cast::<Num>::cast(self).and_then(|v| v.cast())
    }
}

impl Cast<f64> for Value {
    fn name() -> String {
        "bool".into()
    }

    fn cast(&self) -> Option<f64> {
        Cast::<Num>::cast(self).and_then(|v| v.cast())
    }
}

pub enum ValueType {
    Nil,
    Bool,
    Int,
    Float,
    Function,
    Vec,
}

impl ValueType {
    pub fn name(&self) -> &'static str {
        match self {
            ValueType::Nil => "nil",
            ValueType::Bool => "bool",
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Function => "function",
            ValueType::Vec => "vec",
        }
    }
}

pub enum IndexRes<'a> {
    Unindexable,
    OutOfBounds,
    NonExistant,
    Value(&'a Value),
}

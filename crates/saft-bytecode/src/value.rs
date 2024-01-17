use std::rc::Rc;

use crate::{chunk::Chunk, num::Num};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Num(Num),
    Function(Function),
}

#[derive(Debug, Clone)]
pub enum Function {
    SaftFunction(SaftFunction),
}

#[derive(Debug, Clone)]
pub struct SaftFunction {
    pub arity: usize,
    pub chunk: Rc<Chunk>,
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
        }
    }

    pub fn index(&self, _index: &Value) -> Option<&Value> {
        None
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
}

impl ValueType {
    pub fn name(&self) -> &'static str {
        match self {
            ValueType::Nil => "nil",
            ValueType::Bool => "bool",
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Function => "function",
        }
    }
}

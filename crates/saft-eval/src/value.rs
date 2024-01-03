use crate::interpreter::ControlFlow;
use std::{borrow::Borrow, rc::Rc};

use saft_ast::Statement;
use saft_common::span::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Num(Num),
    Function(Function),
    String(String),
    Vec(Vec<Value>),
}

impl Value {
    pub fn repr(&self) -> String {
        match self {
            Value::Nil => "nil".into(),
            Value::Num(num) => num.repr(),
            Value::Function(Function::SaftFunction(..)) => "<function>".into(),
            Value::Function(Function::NativeFunction(..)) => "<builtin function>".into(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Vec(vals) => {
                let mut buf = String::new();

                use std::fmt::Write;

                write!(&mut buf, "[").unwrap();
                let mut first = true;
                for val in vals.iter() {
                    if !first {
                        write!(&mut buf, ", ").unwrap()
                    };
                    first = false;
                    write!(&mut buf, "{}", val.repr()).unwrap();
                }
                write!(&mut buf, "]").unwrap();

                buf
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Num {
    Bool(bool),
    Int(i64),
    Float(f64),
}

pub enum ValueType {
    Nil,
    Bool,
    Int,
    Float,
    Function,
    String,
    Vec,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
            ValueType::Nil => "nil",
            ValueType::Bool => "bool",
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Function => "function",
            ValueType::String => "string",
            ValueType::Vec => "vec",
        }
    }
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
            (Num::Float(a), Num::Float(b)) => Num::Float(a / b),
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
    pub fn ty(&self) -> ValueType {
        use Value as V;
        use ValueType as T;
        match self {
            V::Nil => T::Nil,
            V::Num(Num::Bool(_)) => T::Bool,
            V::Num(Num::Int(_)) => T::Int,
            V::Num(Num::Float(_)) => T::Float,
            V::Function(..) => T::Function,
            V::String(..) => T::String,
            V::Vec(..) => T::Vec,
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
    pub f: fn(&Span, Vec<Spanned<Value>>) -> Result<Value, ControlFlow>,
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

impl<T> From<Vec<T>> for Value
where
    Value: From<T>,
    T: Clone,
{
    fn from(value: Vec<T>) -> Self {
        Value::Vec(value.iter().map(|v| v.clone().into()).collect())
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

impl From<Spanned<Value>> for Value {
    fn from(value: Spanned<Value>) -> Self {
        value.v
    }
}

pub trait Cast<T> {
    fn cast(self) -> Option<T>;
}

impl<T, U> Cast<U> for T
where
    U: CastFrom<T>,
{
    fn cast(self) -> Option<U> {
        CastFrom::cast_from(self)
    }
}

pub trait CastFrom<T>
where
    Self: Sized,
{
    fn ty_name() -> String;

    fn cast_from(value: T) -> Option<Self>;
}

impl CastFrom<Value> for Value {
    fn ty_name() -> String {
        "value".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        Some(value)
    }
}

impl CastFrom<Value> for i64 {
    fn ty_name() -> String {
        "integer".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::Num(Num::Int(i)) => Some(i),
            _ => None,
        }
    }
}

impl CastFrom<Value> for f64 {
    fn ty_name() -> String {
        "float".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::Num(Num::Float(f)) => Some(f),
            _ => None,
        }
    }
}

impl CastFrom<Value> for bool {
    fn ty_name() -> String {
        "bool".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::Num(Num::Bool(b)) => Some(b),
            _ => None,
        }
    }
}

impl CastFrom<Value> for Num {
    fn ty_name() -> String {
        "numeric".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::Num(num) => Some(num.clone()),
            _ => None,
        }
    }
}

impl CastFrom<Value> for String {
    fn ty_name() -> String {
        "string".into()
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl<T> CastFrom<Value> for Vec<T>
where
    T: CastFrom<Value>,
{
    fn ty_name() -> String {
        format!("vec[{}]", T::ty_name())
    }

    fn cast_from(value: Value) -> Option<Self> {
        match value {
            Value::Vec(vals) => {
                let mut cast_vals = Vec::new();
                for val in vals.iter() {
                    cast_vals.push(val.clone().cast()?);
                }
                Some(cast_vals)
            }
            _ => None,
        }
    }
}

pub trait NativeFunc {
    fn data() -> NativeFuncData;
}

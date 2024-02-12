use crate::value::Cast;
use crate::value::NativeFunction;
use crate::value::Value;
use crate::vm;
use saft_macro::native_function;
use saft_syntax::span::Span;

pub struct NativeRes(pub Result<Value, vm::Error>);

impl From<Value> for NativeRes {
    fn from(value: Value) -> Self {
        NativeRes(Ok(value))
    }
}

impl From<Result<Value, vm::Error>> for NativeRes {
    fn from(value: Result<Value, vm::Error>) -> Self {
        NativeRes(value)
    }
}

impl From<()> for NativeRes {
    fn from(_value: ()) -> Self {
        NativeRes(Ok(Value::Nil))
    }
}

#[native_function]
pub fn print(v: Value) {
    match v {
        Value::String(s) => println!("{}", s),
        _ => println!("{}", v.repr()),
    }
}

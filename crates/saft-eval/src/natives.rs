use std::rc::Rc;

use crate::interpreter::{ControlFlow, Interpreter};
use crate::{cast_error, exotic};
use saft_common::span::{Span, Spanned};
use saft_macro::native_function;

use crate::interpreter::Env;
use crate::interpreter::Exception;
use crate::value::{Cast, CastFrom, Function, NativeFunc, NativeFuncData, NativeRes, Num, Value};

#[native_function]
fn sin(arg: Num) -> f64 {
    arg.cast_float().sin()
}

#[native_function]
fn cos(arg: Num) -> f64 {
    arg.cast_float().cos()
}

#[native_function]
fn time() -> f64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64()
}

#[native_function]
fn print(val: Value) {
    match val {
        Value::String(s) => println!("{}", s),
        v => println!("{}", v.repr()),
    }
}

#[native_function]
fn repr(val: Value) -> String {
    val.repr()
}

#[native_function]
fn read(fname: String) -> Result<String, Exception> {
    std::fs::read_to_string(&fname)
        .map_err(|_| exotic!(format!("Could not open file '{}'", &fname)))
}

#[native_function]
fn split(string: String, delim: String) -> Vec<String> {
    string.split(&delim).map(|s| s.into()).collect()
}

#[native_function]
fn join(parts: Vec<String>, separator: String) -> String {
    parts.join(&separator)
}

pub fn add_natives(env: &mut Env) {
    add_native::<sin>(env);
    add_native::<cos>(env);
    add_native::<time>(env);
    add_native::<print>(env);
    add_native::<repr>(env);
    add_native::<read>(env);
    add_native::<split>(env);
    add_native::<join>(env);
}

fn add_native<N: NativeFunc>(env: &mut Env) {
    let data = N::data();
    env.declare_unspanned(
        &data.name.into(),
        Value::Function(Rc::new(Function::NativeFunction(data))),
    );
}

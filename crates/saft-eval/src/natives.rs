use saft_macro::native_function;

use crate::interpreter::{Env, Error};
use crate::value::{Cast, Function, NativeFunc, NativeFuncData, NativeRes, Value};

#[native_function]
fn sin(arg: f64) -> f64 {
    arg.sin()
}

#[native_function]
fn cos(arg: f64) -> f64 {
    arg.cos()
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
fn read(fname: String) -> Result<String, Error> {
    std::fs::read_to_string(&fname).map_err(|_| Error::Exotic {
        message: format!("Could not open file '{}'", &fname),
        span: None,
        note: None,
    })
}

pub fn add_natives(env: &mut Env) {
    add_native::<sin>(env);
    add_native::<cos>(env);
    add_native::<time>(env);
    add_native::<print>(env);
    add_native::<repr>(env);
    add_native::<read>(env);
}

fn add_native<N: NativeFunc>(env: &mut Env) {
    let data = N::data();
    env.declare_unspanned(
        &data.name.into(),
        Value::Function(Function::NativeFunction(data)),
    );
}
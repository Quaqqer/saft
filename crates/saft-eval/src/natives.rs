use saft_macro::native_function;

use crate::interpreter::{Env, Error};
use crate::value::{Cast, Function, NativeFunc, NativeFuncData, Value};

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
    println!("{:?}", val);
}

pub fn add_natives(env: &mut Env) {
    add_native::<sin>(env);
    add_native::<cos>(env);
    add_native::<time>(env);
    add_native::<print>(env);
}

fn add_native<N: NativeFunc>(env: &mut Env) {
    let data = N::data();
    env.declare_unspanned(
        &data.name.into(),
        Value::Function(Function::NativeFunction(data)),
    );
}

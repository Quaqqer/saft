use crate::{value::SaftFunction, vm};

#[derive(Clone, Debug)]
pub enum Item {
    SaftFunction(SaftFunction),
}

pub struct NativeFunction {
    f: fn(&mut vm::Vm) -> Result<(), vm::Error>,
}

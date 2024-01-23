use crate::value::Function;

#[derive(Clone, Debug)]
pub enum Constant {
    Function(Function),
}

use crate::value::Function;

#[derive(Debug, Clone)]
pub struct ConstantRef(pub usize);

#[derive(Clone, Debug)]
pub enum Constant {
    Function(Function),
}

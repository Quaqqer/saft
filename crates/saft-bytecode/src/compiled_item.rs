use crate::constant::ConstantRef;

#[derive(Clone, Debug)]
pub(crate) enum CompiledItem {
    Function(ConstantRef),
}

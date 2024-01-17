use std::rc::Rc;

use crate::value::SaftFunction;

#[derive(Clone)]
pub enum Item {
    SaftFunction(Rc<SaftFunction>),
}

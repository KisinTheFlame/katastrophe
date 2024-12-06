use std::rc::Rc;

use super::ty::Type;

pub enum Reference {
    Binding(Rc<Type>),
}

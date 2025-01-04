use std::rc::Rc;

use super::crumb::Mutability;
use super::ty::Type;

pub enum Reference {
    Binding(Rc<Type>, Mutability),
}

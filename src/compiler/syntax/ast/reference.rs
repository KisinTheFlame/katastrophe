use std::rc::Rc;

use super::{crumb::Mutability, ty::Type};

pub enum Reference {
    Binding(Rc<Type>, Mutability),
}

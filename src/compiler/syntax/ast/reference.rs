use std::rc::Rc;

use crate::compiler::context::StructId;
use crate::util::common::Array;

use super::crumb::Field;
use super::crumb::Identifier;
use super::ty::Type;

pub enum Reference {
    Binding(Rc<Type>),
    StructDef(StructId, Rc<Identifier>, Array<Field>),
}

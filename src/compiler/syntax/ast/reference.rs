use std::rc::Rc;

use crate::{compiler::context::StructId, util::common::Array};

use super::{crumb::{Field, Identifier}, ty::Type};

pub enum Reference {
    Binding(Rc<Type>),
    StructDef(StructId, Rc<Identifier>, Array<Field>)
}

use std::fmt::Display;
use std::fmt::{self};

use super::IrId;

#[derive(Clone)]
pub enum Memory {
    Stack(IrId),
    Global(IrId),
}

impl Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Memory::Stack(id) => write!(f, "%{id}"),
            Memory::Global(id) => write!(f, "@{id}"),
        }
    }
}

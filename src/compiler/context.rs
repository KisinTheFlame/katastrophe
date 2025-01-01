use std::collections::HashMap;
use std::rc::Rc;

use crate::define_id_generator;

use super::ir::instruction::Instruction;
use super::ir::instruction::IrReference;
use super::syntax::ast::Document;
use super::syntax::ast::crumb::Identifier;
use super::syntax::ast::package::DocumentPath;
use super::syntax::ast::reference::Reference;

pub type DocumentId = u32;
pub type StructId = u32;

pub struct Context {
    pub id_map: HashMap<Rc<DocumentPath>, DocumentId>,
    pub path_map: HashMap<DocumentId, Rc<DocumentPath>>,
    pub document_map: HashMap<DocumentId, Document>,
    pub reference_map: HashMap<DocumentId, HashMap<Rc<Identifier>, Rc<Reference>>>,
    pub ir_model_map: HashMap<DocumentId, HashMap<Rc<Identifier>, Rc<IrReference>>>,
    pub instruction: HashMap<DocumentId, Rc<Instruction>>,
}

impl Context {
    #[must_use]
    pub fn new() -> Context {
        Context {
            id_map: HashMap::new(),
            path_map: HashMap::new(),
            document_map: HashMap::new(),
            reference_map: HashMap::new(),
            ir_model_map: HashMap::new(),
            instruction: HashMap::new(),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

define_id_generator!(document, pub);
define_id_generator!(struct, pub);

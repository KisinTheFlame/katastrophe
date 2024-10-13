use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use crate::define_id_generator;

use super::ir::instruction::Instruction;
use super::ir::instruction::IrModel;
use super::syntax::ast::crumb::Identifier;
use super::syntax::ast::crumb::Mutability;
use super::syntax::ast::package::DocumentPath;
use super::syntax::ast::ty::Type;
use super::syntax::ast::Document;

pub type DocumentId = u32;

pub struct Context {
    pub id_map: HashMap<Rc<DocumentPath>, DocumentId>,
    pub path_map: HashMap<DocumentId, Rc<DocumentPath>>,
    pub document_map: HashMap<DocumentId, Document>,
    pub type_map: HashMap<DocumentId, HashMap<Rc<Identifier>, Rc<Type>>>,
    pub mutability_map: HashMap<DocumentId, HashMap<Rc<Identifier>, Mutability>>,
    pub ir_model_map: HashMap<DocumentId, HashMap<Rc<Identifier>, IrModel>>,
    pub instruction: HashMap<DocumentId, Rc<Instruction>>,
}

impl Context {
    #[must_use]
    pub fn new() -> Context {
        Context {
            id_map: HashMap::new(),
            path_map: HashMap::new(),
            document_map: HashMap::new(),
            type_map: HashMap::new(),
            mutability_map: HashMap::new(),
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

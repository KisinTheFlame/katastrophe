use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::context::Context;
use crate::compiler::embedded;
use crate::compiler::err::CompileError;
use crate::compiler::ir::instruction::value::Value;
use crate::compiler::syntax::parser::Parser;
use crate::util::common::Array;

use super::crumb::Identifier;

pub type PathNode = String;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct DocumentPath(pub Array<PathNode>);

impl DocumentPath {
    pub fn to_dir(&self) -> String {
        let DocumentPath(path_nodes) = self;
        path_nodes.iter().map(Rc::as_ref).cloned().collect::<Rc<_>>().join("/")
    }
}

impl Display for DocumentPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let DocumentPath(path_nodes) = self;
        let formatted = path_nodes.iter().map(Rc::as_ref).cloned().collect::<Rc<_>>().join("::");
        write!(f, "{formatted}")
    }
}

pub struct UsingPath(pub Rc<DocumentPath>, pub Rc<Identifier>);

impl Display for UsingPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let UsingPath(document_path, item) = self;
        write!(f, "{document_path}::{item}")
    }
}

/// # Errors
pub fn load_package_path(context: &mut Context, path: Rc<DocumentPath>) -> CompileResult<()> {
    if context.id_map.contains_key(&path) {
        return Ok(());
    }
    load_package(context, path)?;
    Ok(())
}

#[must_use]
pub fn get_builtin(path: &Rc<DocumentPath>, id: &str, value: &Value) -> Option<String> {
    let template = embedded::resolve_builtin_ir(path, id)?;
    Some(template.replace("{value}", value.to_string().as_str()))
}

fn load_package(context: &mut Context, path: Rc<DocumentPath>) -> CompileResult<()> {
    let DocumentPath(nodes) = path.as_ref();
    if nodes.first().map(Rc::as_ref).map(String::as_str) != Some("std") {
        return Err(CompileError::UnsupportedFeature("non-std package path"));
    }
    let Some(code) = embedded::resolve_package_source(&path) else {
        return Err(CompileError::UnknownPackage);
    };
    Parser::new(path, code)?.parse_document(context)?;
    Ok(())
}

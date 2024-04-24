use std::{
    fmt::{self, Display},
    fs,
};

use crate::compiler::{
    context::Context,
    err::CompileError,
    ir::instruction::Value,
    syntax::{
        err::{ParseError, ParseErrorKind},
        parser::Parser,
    },
};

use super::crumb::Identifier;

pub type PathNode = String;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct DocumentPath(pub Vec<PathNode>);

impl DocumentPath {
    pub fn to_dir(&self) -> String {
        let DocumentPath(path_nodes) = self;
        path_nodes.iter().cloned().collect::<Vec<_>>().join("/")
    }
}

impl Display for DocumentPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let DocumentPath(path_nodes) = self;
        let formatted = path_nodes.iter().cloned().collect::<Vec<_>>().join("::");
        write!(f, "{formatted}")
    }
}

pub struct UsingPath(pub DocumentPath, pub Identifier);

impl Display for UsingPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let UsingPath(document_path, item) = self;
        write!(f, "{document_path}::{item}")
    }
}

pub fn load_package_path(context: &mut Context, path: &DocumentPath) -> Result<(), CompileError> {
    if context.id_map.contains_key(path) {
        return Ok(());
    }
    load_package(context, path)?;
    Ok(())
}

pub fn get_builtin(path: &DocumentPath, id: &String, value: &Value) -> Option<String> {
    let document_path = get_package_path(path);
    let file_path = format!("{document_path}/builtin/{id}.ll");
    match fs::read_to_string(file_path) {
        Ok(code) => Some(code.replace("{value}", value.to_string().as_str())),
        Err(_) => None,
    }
}

fn get_package_path(document_path: &DocumentPath) -> String {
    let DocumentPath(path_nodes) = document_path;
    let root_directory = &path_nodes[0];
    match root_directory.as_str() {
        "std" => get_std_package_path(document_path),
        _ => todo!(),
    }
}

fn get_std_package_path(document_path: &DocumentPath) -> String {
    const STD_ROOT: &'static str = "./library";
    let dir = document_path.to_dir();
    format!("{STD_ROOT}/{dir}")
}

fn load_package(context: &mut Context, path: &DocumentPath) -> Result<(), CompileError> {
    let file_path = get_package_path(path) + ".katas";
    let code = match fs::read_to_string(file_path) {
        Ok(code) => code,
        Err(_) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnknownPackage,
            }
            .into());
        }
    };
    Parser::new(path.clone(), &code).parse_document(context)?;
    Ok(())
}

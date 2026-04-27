use std::fmt::Display;
use std::fmt::{self};
use std::fs;
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
    if nodes.first().map(Rc::as_ref).map(String::as_str) == Some("std") {
        let Some(code) = embedded::resolve_package_source(&path) else {
            return Err(CompileError::UnknownPackage {
                path: format!("(embedded stdlib) {path}"),
            });
        };
        Parser::new(path, code)?.parse_document(context)?;
        return Ok(());
    }

    // 非 std 路径：解析为 `<project_root>/<a>/<b>.../<last>.katas`，
    // 在父目录内做大小写字面校验，然后从磁盘读取并 parse。
    let source = load_user_package_source(context, &path)?;
    Parser::new(path, &source)?.parse_document(context)?;
    Ok(())
}

fn load_user_package_source(context: &Context, path: &Rc<DocumentPath>) -> CompileResult<String> {
    let DocumentPath(nodes) = path.as_ref();
    let mut target = context.project_root.clone();
    for node in nodes.iter() {
        target.push(node.as_ref());
    }
    target.set_extension("katas");

    // 父目录列表 + 字面比较，挡住 macOS / Windows 默认大小写不敏感 FS 的"碰巧能读"。
    let parent = target.parent().ok_or_else(|| CompileError::UnknownPackage {
        path: target.to_string_lossy().into_owned(),
    })?;
    let expected_file_name = target.file_name().ok_or_else(|| CompileError::UnknownPackage {
        path: target.to_string_lossy().into_owned(),
    })?;
    if !directory_contains_exact(parent, expected_file_name)? {
        return Err(CompileError::UnknownPackage {
            path: target.to_string_lossy().into_owned(),
        });
    }

    // 字面比对已通过：文件存在且大小写一致。read 仍然失败 → 真实 IO（权限/磁盘/中途消失），
    // 把 io::Error 透出去而不是吞成模糊 UnknownPackage，否则运维拿到这个错误无从下手。
    fs::read_to_string(&target).map_err(|error| CompileError::FileReadFailed {
        path: target.to_string_lossy().into_owned(),
        error: error.to_string(),
    })
}

/// 在目录内查找文件名是否**字面**等于 `expected`。这强制大小写敏感，
/// 即使底层文件系统（APFS / NTFS 默认）大小写不敏感也不放过。
///
/// 返回 `Result` 而非 `bool` 是为了把目录遍历的真实 IO 错误（权限/坏 entry/挂载抖动）
/// 透出去，避免假阴性 + 调试无从下手。但**父目录不存在**（ENOENT）属于正常的
/// "包不存在"情况，由调用方按 `Ok(false)` 处理为 `UnknownPackage`。
fn directory_contains_exact(parent: &std::path::Path, expected: &std::ffi::OsStr) -> CompileResult<bool> {
    let entries = match fs::read_dir(parent) {
        Ok(entries) => entries,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => {
            return Err(CompileError::FileReadFailed {
                path: parent.to_string_lossy().into_owned(),
                error: error.to_string(),
            });
        }
    };
    for entry in entries {
        let entry = entry.map_err(|error| CompileError::FileReadFailed {
            path: parent.to_string_lossy().into_owned(),
            error: error.to_string(),
        })?;
        if entry.file_name() == expected {
            return Ok(true);
        }
    }
    Ok(false)
}

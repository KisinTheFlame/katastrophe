//! 编译期嵌入的标准库与 runtime 资源。
//!
//! 把 `library/` 整棵目录树通过 `include_dir!` 内嵌到二进制里，
//! 让 katastrophe 不依赖运行时 CWD——任何位置 `cargo install` 出来
//! 的二进制都能直接用。

use std::rc::Rc;

use include_dir::Dir;
use include_dir::include_dir;

use super::syntax::ast::package::DocumentPath;

/// 标准库根目录（递归打包）。
static LIBRARY: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/library");

/// 在每段生成的 LLVM IR 之前固定塞进去的 libc 声明片段。
const LIBC_DECLARATION: &str = include_str!("../../static/libc_declaration.ll");

/// 解析 `using` 路径到对应的 `.katas` 源码。仅支持 `std::*`。
#[must_use]
pub fn resolve_package_source(path: &DocumentPath) -> Option<&'static str> {
    let file_path = std_relative_path(path)?;
    LIBRARY.get_file(format!("{file_path}.katas"))?.contents_utf8()
}

/// 解析 builtin 函数的 LLVM IR 模板（含 `{value}` 占位符）。
#[must_use]
pub fn resolve_builtin_ir(path: &DocumentPath, name: &str) -> Option<&'static str> {
    let file_path = std_relative_path(path)?;
    LIBRARY
        .get_file(format!("{file_path}/builtin/{name}.ll"))?
        .contents_utf8()
}

/// libc 声明 IR——katastrophe 生成的所有可执行文件都需要它。
#[must_use]
pub fn libc_declaration() -> &'static str {
    LIBC_DECLARATION
}

/// 把 `DocumentPath` 拼成 `library/` 内的相对路径（不含扩展名）。`include_dir!`
/// 是从 `library/` 起根的，所以保留 `std/` 段；非 `std::*` 路径返回 `None`。
fn std_relative_path(path: &DocumentPath) -> Option<String> {
    let DocumentPath(nodes) = path;
    if nodes.first().map(Rc::as_ref).map(String::as_str) != Some("std") {
        return None;
    }
    Some(nodes.iter().map(Rc::as_ref).cloned().collect::<Vec<_>>().join("/"))
}

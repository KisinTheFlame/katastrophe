use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::rc::Rc;

use clap::Parser;
use katastrophe::CompileResult;
use katastrophe::assemble;
use katastrophe::compiler::context::Context;
use katastrophe::compiler::err::CompileError;
use katastrophe::compiler::semantics::main_function_checker::main_function_check;
use katastrophe::dump_ast;
use katastrophe::ir_generate;
use katastrophe::ir_translate;
use katastrophe::lvalue_check;
use katastrophe::syntax_analyze;
use katastrophe::type_infer;

#[derive(Parser)]
#[command(name = "katastrophe", version, about = "Katastrophe 玩具语言编译器")]
struct Cli {
    /// 输入源文件路径
    input: PathBuf,

    /// 输出文件路径，缺省时按 target 取默认值
    #[arg(short, long, value_name = "PATH")]
    output: Option<PathBuf>,

    /// 仅输出 AST，不生成 IR / 可执行文件
    #[arg(long, group = "target")]
    ast: bool,

    /// 仅输出 LLVM IR，不进行链接
    #[arg(long, group = "target")]
    ir: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Target {
    Executable,
    Ast,
    Ir,
}

impl Target {
    fn from_cli(cli: &Cli) -> Self {
        if cli.ast {
            Target::Ast
        } else if cli.ir {
            Target::Ir
        } else {
            Target::Executable
        }
    }

    fn default_path(self) -> PathBuf {
        PathBuf::from(match self {
            Target::Executable => "a.out",
            Target::Ast => "a.ast",
            Target::Ir => "a.ll",
        })
    }

    fn default_extension(self) -> Option<&'static str> {
        match self {
            Target::Executable => None,
            Target::Ast => Some("ast"),
            Target::Ir => Some("ll"),
        }
    }
}

fn resolve_output_path(target: Target, user_path: Option<PathBuf>) -> PathBuf {
    let mut path = user_path.unwrap_or_else(|| target.default_path());
    if path.extension().is_none() {
        if let Some(ext) = target.default_extension() {
            path.set_extension(ext);
        }
    }
    path
}

fn main() {
    let cli = Cli::parse();
    if let Err(e) = run(cli) {
        eprintln!("error: {e}");
        process::exit(1);
    }
}

fn run(cli: Cli) -> CompileResult<()> {
    let target = Target::from_cli(&cli);
    let output_path = resolve_output_path(target, cli.output);

    let code = fs::read_to_string(&cli.input).map_err(|error| CompileError::FileReadFailed {
        path: cli.input.to_string_lossy().into_owned(),
        error: error.to_string(),
    })?;

    let mut context = Context::new();
    let main_document_id = syntax_analyze(&mut context, &code)?;

    let mut ids = context.document_map.keys().copied().collect::<Vec<_>>();
    ids.sort_unstable();
    let ids = Rc::<[u32]>::from(ids);

    type_infer(&mut context, &ids)?;
    lvalue_check(&context, &ids)?;
    main_function_check(&context, main_document_id)?;

    match target {
        Target::Ast => write_text_output(&output_path, &dump_ast(&context)),
        Target::Ir => {
            ir_translate(&mut context, &ids)?;
            let ir_code = ir_generate(&context, &ids, main_document_id)?;
            write_text_output(&output_path, &ir_code)
        }
        Target::Executable => {
            ir_translate(&mut context, &ids)?;
            let ir_code = ir_generate(&context, &ids, main_document_id)?;
            assemble(ir_code, &output_path.to_string_lossy())
        }
    }
}

fn write_text_output(path: &Path, content: &str) -> CompileResult<()> {
    fs::write(path, content).map_err(|error| CompileError::FileWriteFailed {
        path: path.to_string_lossy().into_owned(),
        error: error.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::PathBuf;
    use super::Target;
    use super::resolve_output_path;

    #[test]
    fn missing_user_path_falls_back_to_target_default() {
        assert_eq!(resolve_output_path(Target::Ast, None), PathBuf::from("a.ast"));
        assert_eq!(resolve_output_path(Target::Ir, None), PathBuf::from("a.ll"));
        assert_eq!(resolve_output_path(Target::Executable, None), PathBuf::from("a.out"));
    }

    #[test]
    fn user_path_without_extension_gets_target_extension() {
        assert_eq!(
            resolve_output_path(Target::Ast, Some(PathBuf::from("output"))),
            PathBuf::from("output.ast"),
        );
        assert_eq!(
            resolve_output_path(Target::Ir, Some(PathBuf::from("output"))),
            PathBuf::from("output.ll"),
        );
    }

    #[test]
    fn user_path_with_existing_extension_is_kept_verbatim() {
        assert_eq!(
            resolve_output_path(Target::Ast, Some(PathBuf::from("foo.txt"))),
            PathBuf::from("foo.txt"),
        );
        assert_eq!(
            resolve_output_path(Target::Ast, Some(PathBuf::from("a.b.c"))),
            PathBuf::from("a.b.c"),
        );
    }

    #[test]
    fn executable_target_does_not_force_extension() {
        assert_eq!(
            resolve_output_path(Target::Executable, Some(PathBuf::from("mybin"))),
            PathBuf::from("mybin"),
        );
    }
}

use std::env::Args;
use std::iter::Peekable;

use std::path::Path;
use std::process::Command;
use std::{env, fs};

use compiler::context::Context;
use compiler::err::CompileError;
use compiler::ir::builtin::{generate_builtin, generate_entry};
use compiler::ir::translator::Translator;
use compiler::semantics::main_function_check::main_function_check;
use compiler::semantics::mutability_checker::MutabilityChecker;
use compiler::semantics::type_inferrer::TypeInferrer;
use compiler::syntax::ast::package::DocumentPath;
use indoc::formatdoc;

use crate::compiler::syntax::parser::Parser;
use crate::util::reportable_error::ReportableError;

pub mod compiler;
pub mod util;

enum CommandError {
    MissingOutputFile,

    DuplicateInputDeclaration,
    DuplicateOutputFileDeclaration,
    DuplicateTargetDeclaration,

    NoInputFile,
}

impl ReportableError for CommandError {
    fn report(&self) -> ! {
        match self {
            CommandError::MissingOutputFile => {
                panic!("unexpected EOF after -o or --output. need a output path.")
            }
            CommandError::DuplicateInputDeclaration => panic!("duplicate input path declarations."),
            CommandError::DuplicateOutputFileDeclaration => {
                panic!("duplicate output path declarations.")
            }
            CommandError::DuplicateTargetDeclaration => panic!("duplicate target declarations."),
            CommandError::NoInputFile => panic!("no input file."),
        }
    }
}

enum Target {
    Executable,
    Ast,
    Ir,
}

struct CommandOptions {
    target: Option<Target>,
    input_path: Option<String>,
    output_path: Option<String>,
}

impl CommandOptions {
    pub fn new() -> CommandOptions {
        CommandOptions {
            target: None,
            input_path: None,
            output_path: None,
        }
    }

    pub fn complete_by_default(&mut self) -> Result<(), CommandError> {
        if self.input_path.is_none() {
            return Err(CommandError::NoInputFile);
        }

        self.target.get_or_insert(Target::Executable);
        self.output_path.get_or_insert(String::from("a.out"));
        Ok(())
    }
}

struct ArgHandler {
    args: Peekable<Args>,
}

impl ArgHandler {
    pub fn new(args: Args) -> ArgHandler {
        ArgHandler {
            args: args.peekable(),
        }
    }

    fn next_else(&mut self, error: CommandError) -> Result<String, CommandError> {
        self.args.next().ok_or(error)
    }

    fn parse(&mut self) -> Result<CommandOptions, CommandError> {
        // skip self path
        self.args.next();

        let mut options = CommandOptions::new();

        while self.args.peek().is_some() {
            let arg = self.args.next().unwrap();
            match arg.as_str() {
                "-o" | "--output" => {
                    let output_filename = self.next_else(CommandError::MissingOutputFile)?;
                    if options.output_path.is_some() {
                        return Err(CommandError::DuplicateOutputFileDeclaration);
                    }
                    options.output_path = Some(output_filename);
                }
                "--ast" => {
                    if options.target.is_some() {
                        return Err(CommandError::DuplicateTargetDeclaration);
                    }
                    options.target = Some(Target::Ast);
                }
                "--ir" => {
                    if options.target.is_some() {
                        return Err(CommandError::DuplicateTargetDeclaration);
                    }
                    options.target = Some(Target::Ir);
                }
                input_filename => {
                    if options.input_path.is_some() {
                        return Err(CommandError::DuplicateInputDeclaration);
                    }
                    options.input_path = Some(input_filename.to_string());
                }
            }
        }
        Ok(options)
    }

    pub fn handle(&mut self) -> CommandOptions {
        let mut options = match self.parse() {
            Ok(options) => options,
            Err(e) => e.report(),
        };
        match options.complete_by_default() {
            Ok(()) => options,
            Err(e) => e.report(),
        }
    }
}

fn execute() -> Result<(), CompileError> {
    let args = env::args();
    let mut arg_handler = ArgHandler::new(args);
    let options = arg_handler.handle();

    let input_filename = options.input_path.unwrap();
    let mut output_path = options.output_path.unwrap();

    let Ok(code) = fs::read_to_string(input_filename) else {
        panic!("encountering fatal error when reading file");
    };

    // parse and semantic check
    let mut context = Context::new();
    let mut parser = Parser::new(DocumentPath(vec![String::from("self")]), code.as_str());
    let main_document_id = parser.parse_document(&mut context)?;

    let mut ids = context.document_map.keys().map(|x| *x).collect::<Vec<_>>();
    ids.sort();

    let mut type_inferrer = TypeInferrer::new();
    ids.iter()
        .try_for_each(|id| type_inferrer.infer(&mut context, *id))?;

    let mut mutability_checker: MutabilityChecker = MutabilityChecker::new();
    ids.iter()
        .try_for_each(|id| mutability_checker.check_document(&context, *id))?;

    main_function_check(&context, main_document_id)?;

    if let Some(Target::Ast) = options.target {
        if !output_path.contains('.') {
            output_path.push_str(".ast");
        }
        let output = ids
            .iter()
            .map(|id| {
                let document_path = context.path_map.get(id).unwrap();
                let ast = context.document_map.get(id).unwrap();
                formatdoc! {"
                    ----- {document_path} -----
                    {ast}
                "}
            })
            .collect::<Vec<_>>()
            .join("\n");
        fs::write(output_path, output).expect("failed to write ast.");
        return Ok(());
    }

    // translate
    let mut id_translators = ids
        .iter()
        .map(|id| {
            let translator = Translator::new(context.path_map.get(id).unwrap().clone());
            (*id, translator)
        })
        .collect::<Vec<_>>();
    id_translators
        .iter_mut()
        .try_for_each(|(id, translator)| translator.pre_scan_global(&mut context, *id))?;
    id_translators
        .iter_mut()
        .try_for_each(|(id, translator)| translator.translate(&mut context, *id))?;

    let ir = ids
        .iter()
        .map(|id| {
            let document_path = context.path_map.get(id).unwrap();
            let ir = context.instruction.get(id).unwrap();
            formatdoc! {"
                ; ----- {document_path} -----
                {ir}
            "}
        })
        .collect::<Vec<_>>()
        .join("\n");
    let builtin_ir = generate_builtin()?;
    let (main_value, _) = context
        .ir_model_map
        .get(&main_document_id)
        .unwrap()
        .get(&String::from("main"))
        .unwrap();
    let entry_ir = generate_entry(main_value.clone())?;
    let ir_code = formatdoc! {"
        {builtin_ir}
        {ir}
        {entry_ir}
    "};

    if let Some(Target::Ir) = options.target {
        if !output_path.contains('.') {
            output_path.push_str(".ll");
        }
        fs::write(output_path, ir_code).expect("failed to write ir.");
        return Ok(());
    }

    // assemble
    let temp_ir_dir = "/tmp/katastrophe";
    if !Path::new(&temp_ir_dir).exists() {
        fs::create_dir(temp_ir_dir).expect("failed to open a temp ir file.");
    }

    let temp_ir_path = format!("{temp_ir_dir}/temp.ll");
    fs::write(&temp_ir_path, ir_code).expect("failed to open a temp ir file.");

    let link_result = Command::new("clang")
        .arg("-mllvm")
        .arg("-opaque-pointers")
        .arg(&temp_ir_path)
        .arg("-o")
        .arg(output_path)
        .output()
        .expect("failed to compile ir.");
    if !link_result.status.success() {
        let error = String::from_utf8(link_result.stderr).expect("failed to read stderr.");
        panic!("{error}");
    }

    Ok(())
}

fn main() {
    if let Err(e) = execute() {
        e.report();
    }
}

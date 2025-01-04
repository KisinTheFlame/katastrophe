use std::env::Args;
use std::env::{self};
use std::fs;
use std::iter::Peekable;
use std::rc::Rc;

use indoc::formatdoc;
use katastrophe::CompileResult;
use katastrophe::assemble;
use katastrophe::compiler::context::Context;
use katastrophe::compiler::semantics::main_function_checker::main_function_check;
use katastrophe::ir_generate;
use katastrophe::ir_translate;
use katastrophe::lvalue_check;
use katastrophe::syntax_analyze;
use katastrophe::type_infer;

enum CommandError {
    MissingOutputFile,

    DuplicateInputDeclaration,
    DuplicateOutputFileDeclaration,
    DuplicateTargetDeclaration,

    NoInputFile,
}

impl CommandError {
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
        ArgHandler { args: args.peekable() }
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

fn main() {
    if let Err(e) = execute() {
        e.report();
    }
}

fn execute() -> CompileResult<()> {
    let args = env::args();
    let mut arg_handler = ArgHandler::new(args);
    let options = arg_handler.handle();

    let input_filename = options.input_path.unwrap();
    let mut output_path = options.output_path.unwrap();

    let Ok(code) = fs::read_to_string(input_filename) else {
        panic!("encountering fatal error when reading file");
    };

    let mut context = Context::new();

    let main_document_id = syntax_analyze(&mut context, code.as_str())?;

    let mut ids = context.document_map.keys().copied().collect::<Vec<_>>();
    ids.sort_unstable();
    let ids = Rc::<[u32]>::from(ids);

    type_infer(&mut context, &ids)?;

    lvalue_check(&context, &ids)?;

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
            .collect::<Rc<_>>()
            .join("\n");
        fs::write(output_path, output).expect("failed to write ast.");
        return Ok(());
    }

    ir_translate(&mut context, &ids)?;

    let ir_code = ir_generate(&context, &ids, main_document_id)?;

    if let Some(Target::Ir) = options.target {
        if !output_path.contains('.') {
            output_path.push_str(".ll");
        }
        fs::write(output_path, ir_code).expect("failed to write ir.");
        return Ok(());
    }

    assemble(ir_code, output_path);

    Ok(())
}

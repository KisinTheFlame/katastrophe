use std::fs;

use crate::compiler::err::CompileError;

use super::{
    err::IrError,
    instruction::{ir_type::IrType, Instruction, IrFunctionPrototype, Value},
};

pub fn generate_builtin() -> Result<Instruction, CompileError> {
    match fs::read_to_string("static/builtin.ll") {
        Ok(code) => Ok(Instruction::BuiltinDefinition(code)),
        Err(_) => Err(IrError::BuiltinFileNotExist.into()),
    }
}

pub fn generate_entry(main_value: Value) -> Result<Instruction, CompileError> {
    let template = Instruction::Definition(
        IrFunctionPrototype {
            function_type: IrType::Function {
                return_type: Box::new(IrType::I32),
                parameter_types: Vec::new(),
            },
            id: Value::Function("main".to_string()),
        },
        Vec::new(),
        Box::new(Instruction::Batch(vec![
            Instruction::Call {
                receiver: Some(Value::Register("main_return".to_string())),
                function: IrFunctionPrototype {
                    function_type: IrType::Function {
                        return_type: Box::new(IrType::I32),
                        parameter_types: Vec::new(),
                    },
                    id: main_value,
                },
                arguments: Vec::new(),
            },
            Instruction::Return {
                return_type: IrType::I32,
                value: Value::Register("main_return".to_string()),
            },
        ])),
    );
    Ok(template)
}

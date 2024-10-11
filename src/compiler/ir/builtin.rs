use std::{fs, rc::Rc};

use crate::compiler::err::CompileError;

use super::{
    err::IrError,
    instruction::{ir_type::IrType, Instruction, IrFunctionPrototype, Value},
};

/// # Errors
pub fn generate_libc_function() -> Result<Instruction, CompileError> {
    match fs::read_to_string("static/libc_declaration.ll") {
        Ok(code) => Ok(Instruction::BuiltinDefinition(code)),
        Err(_) => Err(IrError::BuiltinFileNotExist.into()),
    }
}

/// # Errors
pub fn generate_entry(main_value: Rc<Value>) -> Result<Rc<Instruction>, CompileError> {
    let template = Instruction::Definition(
        IrFunctionPrototype {
            function_type: Rc::new(IrType::Function {
                return_type: IrType::I32.into(),
                parameter_types: [].into(),
            }),
            id: Value::Function("main".to_string()).into(),
        },
        [].into(),
        Rc::new(Instruction::Batch(
            [
                Instruction::Call {
                    receiver: Some(Value::Register("main_return".to_string()).into()),
                    function: IrFunctionPrototype {
                        function_type: Rc::new(IrType::Function {
                            return_type: IrType::I32.into(),
                            parameter_types: [].into(),
                        }),
                        id: main_value,
                    },
                    arguments: [].into(),
                }
                .into(),
                Instruction::Return {
                    return_type: IrType::I32.into(),
                    value: Value::Register("main_return".to_string()).into(),
                }
                .into(),
            ]
            .into(),
        )),
    );
    Ok(template.into())
}

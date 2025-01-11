use std::fs;
use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::bit_width::BitWidth;
use crate::compiler::err::CompileError;
use crate::constants::common::LIBC_DECLARATION;

use super::instruction::Instruction;
use super::instruction::IrFunctionPrototype;
use super::instruction::ir_type::IrType;
use super::instruction::value::Value;

/// # Errors
pub fn generate_libc_function() -> CompileResult<Instruction> {
    match fs::read_to_string(LIBC_DECLARATION) {
        Ok(code) => Ok(Instruction::BuiltinDefinition(code)),
        Err(_) => Err(CompileError::BuiltinFileNotExist),
    }
}

#[must_use]
pub fn generate_entry(main_value: Rc<Value>) -> Rc<Instruction> {
    let template = Instruction::Definition(
        IrFunctionPrototype {
            function_type: Rc::new(IrType::Function {
                return_type: IrType::Int(BitWidth::Bit32).into(),
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
                            return_type: IrType::Int(BitWidth::Bit32).into(),
                            parameter_types: [].into(),
                        }),
                        id: main_value,
                    },
                    arguments: [].into(),
                }
                .into(),
                Instruction::Return {
                    return_type: IrType::Int(BitWidth::Bit32).into(),
                    value: Value::Register("main_return".to_string()).into(),
                }
                .into(),
            ]
            .into(),
        )),
    );
    template.into()
}

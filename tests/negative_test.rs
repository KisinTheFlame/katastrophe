use helper::assert_compile_fails;
use katastrophe::compiler::err::CompileError;

mod helper;

#[test]
fn test_missing_main() {
    assert_compile_fails(
        "
        def not_main() -> i32 {
            return 0;
        }
        ",
        |error| matches!(error, CompileError::UndeclaredMainFunction),
    );
}

#[test]
fn test_illegal_main_type() {
    assert_compile_fails(
        "
        def main() {
            return;
        }
        ",
        |error| matches!(error, CompileError::IllegalMainFunctionType),
    );
}

#[test]
fn test_undeclared_identifier() {
    assert_compile_fails(
        "
        def main() -> i32 {
            return x;
        }
        ",
        |error| matches!(error, CompileError::UndeclaredIdentifier(_)),
    );
}

#[test]
fn test_assign_immutable_variable() {
    assert_compile_fails(
        "
        def main() -> i32 {
            let x = 1;
            x = 2;
            return x;
        }
        ",
        |error| matches!(error, CompileError::AssigningImmutableVariable(_)),
    );
}

#[test]
fn test_assign_type_mismatch() {
    assert_compile_fails(
        "
        def main() -> i32 {
            let x as i8 = 1;
            return 0;
        }
        ",
        |error| matches!(error, CompileError::AssignTypeMismatch { .. }),
    );
}

#[test]
fn test_condition_must_be_bool() {
    assert_compile_fails(
        "
        def main() -> i32 {
            if 1 {
                return 1;
            }
            return 0;
        }
        ",
        |error| matches!(error, CompileError::ConditionNeedBool),
    );
}

#[test]
fn test_unknown_package() {
    assert_compile_fails(
        "
        using std::missing::thing;

        def main() -> i32 {
            return 0;
        }
        ",
        |error| matches!(error, CompileError::UnknownPackage),
    );
}

#[test]
fn test_process_statement_in_global_scope() {
    assert_compile_fails(
        "
        let x = 0;
        x = 1;

        def main() -> i32 {
            return x;
        }
        ",
        |error| matches!(error, CompileError::ProcessInGlobal),
    );
}

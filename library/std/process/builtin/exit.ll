define void {value}(i32 %exit_code) noreturn {
    call i32 @exit(i32 %exit_code)
    unreachable
}

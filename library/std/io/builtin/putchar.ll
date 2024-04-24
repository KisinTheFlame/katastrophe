define i1 {value}(i32 %c) {
    %putchar_result = call i32 @putchar(i32 %c)
    %compare_result = icmp eq i32 %putchar_result, -1
    ret i1 %compare_result
}

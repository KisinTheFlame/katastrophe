define i1 {value}(i8 %c) {
    %c.ext = zext i8 %c to i32
    %putchar_result = call i32 @putchar(i32 %c.ext)
    %compare_result = icmp eq i32 %putchar_result, -1
    ret i1 %compare_result
}

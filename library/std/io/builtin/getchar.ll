define i8 {value}() {
    %c = call i32 @getchar()
    %c.trunc = trunc i32 %c to i8
    ret i8 %c.trunc
}

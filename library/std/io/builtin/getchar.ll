define i32 {value}() {
    %c = call i32 @getchar()
    ret i32 %c
}

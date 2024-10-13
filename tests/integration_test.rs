use helper::test_with;

mod helper;

#[test]
fn test_minimal() {
    test_with("minimal");
}

#[test]
fn test_hello_world() {
    test_with("hello_world");
}

#[test]
fn test_fibonacci() {
    test_with("fibonacci");
}

#[test]
fn test_i8() {
    test_with("i8");
}

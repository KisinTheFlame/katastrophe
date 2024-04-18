# Katastrophe

Katastrophe is a toy language of mine, with a compiler written in Rust.

## Build & Run

To build the compiler, run
```shell
$ cargo build
```

To try out Katastrophe, here is an example code in Katastrophe. save it as a file `test.katas` in current directory.
```
def add(x, y) -> i32 {
    return x + y;
}

def sub(x, y) -> i32 {
    return x - y;
}

def main() -> i32 {
    let a = add(1, 1);
    let b = sub(5, 4);
    let c = add(a, b);
    return a + b + c;
}
```

To compile `test.katas`, just run
```shell
$ cargo run
```

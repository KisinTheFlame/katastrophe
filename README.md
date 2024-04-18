# Katastrophe

Katastrophe is a toy language of mine, with a compiler written in Rust.

## Build & Run

To build the compiler, run
```shell
$ cargo build
```

To try out Katastrophe, here is an example code in Katastrophe. save it as a file `test.katas` in current directory.
```
def main() {
    let a = 1;
    let b = a + 1;
    let c = 5 * (7 + 3) / 4;
    return c - b;
}
```

To compile `test.katas`, just run
```shell
$ cargo run
```

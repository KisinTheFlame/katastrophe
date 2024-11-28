# Katastrophe

Katastrophe is a toy language of mine, with a compiler written in Rust.

## Build & Run

To build the compiler, run
```shell
$ cargo build
```

To try out Katastrophe, here is an example code in Katastrophe. save it as a file `test.katas` in current directory.
```
using std::io::getchar;
using std::io::putchar;
using std::process::exit;

def main() -> i32 {
    let x = read_number();
    print_number(fibonacci(x));
    putchar('\n');
    return 0;
}

def read_number() -> i32 {
    let mut number = 0;
    let mut x = getchar();
    while x == ' ' || x == '\n' {
        x = getchar();
    }
    let mut sign = 1;
    if x == '-' {
        sign = -1;
        x = getchar();
    }
    while '0' <= x && x <= '9' {
        number = number * 10 + (x - '0') as i32;
        x = getchar();
    }
    return number * sign;
}

def print_number(x as i32) {
    if x != 0 && x == -x {
        # TODO: handle this
        exit(1);
    }

    if x < 0 {
        putchar('-');
        print_number(-x);
        return;
    }

    if x <= 9 {
        putchar('0' + x as i8);
        return;
    }
    print_number(x / 10);
    putchar('0' + (x - x / 10 * 10) as i8);
}

def fibonacci(x as i32) -> i32 {
    if x < 0 {
        exit(1);
    }

    if x == 0 {
        return 0;
    }

    if x == 1 {
        return 1;
    }

    return fibonacci(x - 1) + fibonacci(x - 2);
}

```

To compile `test.katas`, just run
```shell
$ cargo run -- test.katas -o test
```

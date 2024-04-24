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

let newline as i32 = 10;
let char_0 as i32 = 48;
let char_9 as i32 = 57;

def print_number(x as i32) {
    if x <= 9 {
        putchar(char_0 + x);
        return;
    }
    print_number(x / 10);
    putchar(char_0 + (x - x / 10 * 10));
}

def main() -> i32 {
    # read number
    let mut x as i32 = 0;
    let mut c as i32 = getchar();
    while c != newline {
        if char_0 <= c && c <= char_9 {
            x = x * 10 + (c - char_0);
        } else {
            return -1;
        }
        c = getchar();
    }

    # print doubled x
    x = 2 * x;
    if x == 0 {
        putchar(char_0);
    } else {
        print_number(x);
    }
    putchar(10);
    return 0;
}

```

To compile `test.katas`, just run
```shell
$ cargo run -- test.katas -o test
```

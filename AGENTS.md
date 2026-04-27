# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Katastrophe is a toy statically-typed language with a compiler written in Rust. The compiler emits LLVM IR and shells out to `clang` to produce a native executable, so `clang` must be on `PATH` when running or testing.

## Commands

- Build: `cargo build`
- Run the compiler: `cargo run -- <input.katas> -o <out>` (default target is an executable `a.out`)
- Emit AST or IR only: `cargo run -- <input.katas> --ast` / `--ir` (output extension is auto-appended when the `-o` path has no dot)
- Run the full test suite: `cargo test`
- Run a single integration test: `cargo test --test integration_test test_fibonacci` (other cases: `test_minimal`, `test_hello_world`, `test_i8`, `test_struct`)
- Lint (matches CI): `cargo clippy -- -Dclippy::pedantic -Dwarnings`
- Format check (matches CI): `cargo fmt --check` — formatting uses stable rustfmt.

Integration tests (`tests/integration_test.rs` → `tests/helper/mod.rs`) compile the `.katas` file under `static/test_files/<name>/`, execute the resulting binary, and diff stdout against the sibling `output` file. Two helper entry points:

- `test_with("<name>")` — single-file fixture, slurps the lone `.katas` source and compiles via the string-based `syntax_analyze`. No `project_root`; non-`std` `using` paths fail as `UnknownPackage`. Use this for compiler-feature tests that don't need the multi-file resolver.
- `test_project_with("<name>", "<entry>.katas")` — multi-file fixture, treats the test directory as the project root and compiles via `syntax_analyze_path`. Use this when the test needs cross-file `using`. Compile-fail variants: `assert_compile_fails(source, predicate)` for string mode, `assert_project_compile_fails(name, entry, predicate)` for project mode.

To add a case, create a new directory under `static/test_files/`, add the `.katas` source(s) and `output` file (omit `output` for compile-fail cases), then add a `test_<name>` function pointing at the right helper.

## Projects with multiple files

- **Project root** is the directory of the entry `.katas` file passed to the CLI (`canonicalize().parent()`). All non-`std` `using` paths resolve **absolutely** from this root; `./` and `../` are not part of the syntax (and are not valid identifiers, so they cannot appear in `using` paths anyway).
- **DocumentPath rule**: `using a::b::c;` means *the symbol `c` from the file `<root>/a/b.katas`*. The last segment is always the symbol; everything before it is the file path under the project root. Two-segment `using a::b;` therefore means symbol `b` from `<root>/a.katas`.
- **`std` is reserved**: the first segment `std` always goes through the embedded stdlib regardless of what's on disk. Putting a `std/` directory at the project root does not shadow the stdlib (the user file is silently unreachable). Naming your entry file `std.katas` is allowed but unusual; the entry is registered under `DocumentPath(["std"])` which never collides with `std::*` lookups (those go to the embedded fs).
- **Strict case sensitivity**: `using Utils::foo;` does **not** match `utils.katas` even on macOS / Windows where the filesystem is case-insensitive by default. The resolver lists the parent directory and compares filenames byte-for-byte before reading. This keeps cross-platform behavior consistent.
- **Flat multi-binary**: putting multiple entry files directly at the project root (e.g. `bin1.katas`, `bin2.katas` alongside shared `utils.katas`) is supported with no extra configuration — each `cargo run -- binN.katas` resolves to the same project root and shares the sibling library files.
- **`def builtin` is std-only**: declaring a builtin function (`def builtin foo() {...}`) in a non-`std` file is rejected at parse time as `BuiltinNotAllowedOutsideStd`. User code cannot define FFI shims; that mechanism is reserved for the embedded stdlib.
- **Current limitations**: no `using a::{b, c}` brace expansion, no `using ... as alias`, no glob `using a::*`, no submodule blocks (`mod foo { ... }`). Each `using` imports exactly one symbol.

## Architecture

`src/main.rs` only handles CLI parsing; the compilation pipeline lives in `src/lib.rs` and is driven as a sequence of passes, all threaded through a single mutable `Context` (`src/compiler/context.rs`) that owns every per-document side table (ASTs, identifier references, IR models, instructions) keyed by a `DocumentId`:

1. `syntax_analyze` / `syntax_analyze_path` — `Parser` (`src/compiler/syntax/parser.rs`) tokenises via `Lexer` (`src/compiler/lexis/`) and recursively parses `using` imports. The string-based `syntax_analyze` is for tests / `--ast` / `--ir` use without a file path; `syntax_analyze_path` is the standard CLI entry — it canonicalizes the entry file, sets `Context.project_root` to its parent directory, and uses the entry's file stem as the entry document's `DocumentPath` single node. `using` resolution: paths whose first node is `std` always go through the embedded stdlib (see `embedded::resolve_package_source`); all other paths resolve to `<project_root>/<a>/<b>.../<last>.katas` with strict case-sensitive filename matching (see `directory_contains_exact` in `src/compiler/syntax/ast/package.rs`). Parsing returns the main document id; other documents accumulate inside `Context`.
2. `type_infer` — `TypeInferrer` (`src/compiler/semantics/type_inferrer.rs`) walks every document and fills the shared `reference_map`.
3. `lvalue_check` — `LValueChecker` enforces assignability rules.
4. `main_function_check` — runs only on the root document id to validate the entry point signature.
5. `ir_translate` — `Translator` (`src/compiler/ir/translator.rs`) runs in **two phases over all documents**: first `pre_scan_global` populates cross-document symbol tables, then `translate` emits instructions. Preserve this ordering when adding new passes that touch globals.
6. `ir_generate` — concatenates per-document IR with the libc shim from `static/libc_declaration.ll` (embedded via `generate_libc_function` in `src/compiler/ir/builtin.rs`) and synthesises an `@main` entry that calls the user's `main`.
7. `assemble` — writes IR to a temp path from `util::file::gen_tmp_ir_path` and invokes `clang` to link.

`CompileError` (`src/compiler/err.rs`) is the uniform error type returned by every fallible pass; unrecoverable invariants use the `sys_error!` macro to panic with a location. IDs are minted through the `define_id_generator!` macro (`src/util/id.rs`) — use it rather than hand-rolling counters.

### Standard library

`library/std/` mixes two file kinds: `.katas` sources written in Katastrophe, and sibling `builtin/` directories holding FFI shims. Functions declared `def builtin foo(...)` in a `.katas` file are expected to be provided by the builtin LLVM IR emitted by `generate_libc_function`; adding a new builtin means wiring both the Katastrophe declaration and the IR-level implementation.

## Conventions

- Nightly-only rustfmt features are in use; do not edit `rustfmt.toml` to drop them without checking CI.
- Clippy pedantic is enforced — new code typically needs `#[must_use]`, `# Errors`/`# Panics` doc sections on public fns, and explicit `Default` impls (see `Context` for the pattern).
- Prefer `Rc<[T]>` / the `Arr` alias in `src/util/common.rs` over `Vec` for shared immutable slices passed between passes; the pipeline relies on cheap cloning.

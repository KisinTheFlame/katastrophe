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
- Format check (matches CI): `cargo +nightly fmt -- --check` — `rustfmt.toml` enables unstable features (`style_edition = "2024"`, `imports_granularity = "Item"`), so **formatting requires a nightly toolchain**; stable `rustfmt` will reject the config.

Integration tests (`tests/integration_test.rs` → `tests/helper/mod.rs`) compile the `.katas` file under `static/test_files/<name>/`, execute the resulting binary, and diff stdout against the sibling `output` file. To add a case, create a new directory there with a `main.katas` and `output` file, then add a `test_<name>` function.

## Architecture

`src/main.rs` only handles CLI parsing; the compilation pipeline lives in `src/lib.rs` and is driven as a sequence of passes, all threaded through a single mutable `Context` (`src/compiler/context.rs`) that owns every per-document side table (ASTs, identifier references, IR models, instructions) keyed by a `DocumentId`:

1. `syntax_analyze` — `Parser` (`src/compiler/syntax/parser.rs`) tokenises via `Lexer` (`src/compiler/lexis/`) and recursively parses `using` imports. Each imported package path is resolved against `./library` (see `load_package_path` in `src/compiler/syntax/ast/package.rs`), so the standard library under `library/std/` is discovered at parse time, not linked later. Parsing returns the main document id; other documents accumulate inside `Context`.
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

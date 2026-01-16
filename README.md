# Glyph

Glyph is an experimental, compiled programming language with a Rust-based toolchain and an LLVM backend.

**Thesis:** a token-efficient language that LLMs can generate easily while remaining readable to humans.

**For newcomers:** the syntax stays small and predictable.
**For LLM power users and vibe coders:** the language is designed to be low-token, high-signal, and easy to prompt.

The repo currently contains a working prototype compiler pipeline:

- Lexer + parser
- Multi-module resolution and (early) type resolution
- MIR lowering (SSA-ish intermediate representation)
- LLVM IR generation, object emission, and native linking
- A tiny C runtime used by the stdlib formatting / I/O layer

> Status: early prototype. The CLI and language are evolving quickly.

## What works today

This is not the final language spec, but these features exist in some form (see `tests/fixtures/` and `examples/`):

- `fn` declarations, locals, `ret`
- `i32` arithmetic and comparisons
- `if`/`else`, `while`, and `for`
- Struct definitions, literals, and field access
- Arrays and indexing
- Basic module discovery / multi-file projects
- Early experiments around interfaces and ownership/borrowing

## Why Glyph?

Glyph is exploring a pragmatic “small language, real binaries” workflow:

- Token efficiency without write-only noise: terse keywords (`fn`, `ret`) and lightweight modules stay easy for both humans and LLMs to read/write.
- Native executables via LLVM
- A standard library surface (`std::print`, `std::println`) backed by a minimal runtime
- Tooling-first layout (compiler crates + formatter + LSP placeholders)
- Friendly entry points for both curious beginners and LLM-assisted, high-velocity prototyping

## Quickstart

### Prerequisites

- Rust (workspace uses Rust edition 2024)
- LLVM (used via `llvm-sys` when building with codegen)

On macOS with Homebrew, you’ll typically need to point `llvm-sys` at your LLVM install:

```bash
export LLVM_SYS_201_PREFIX="/opt/homebrew/opt/llvm"
```

### Build the toolchain

Use Cargo only to build the compiler toolchain itself:

```bash
cargo build --release
```

### Run the demo

```bash
./demo.sh
```

The demo prints the source and the generated LLVM IR for a few fixtures.

## Using the compiler CLI

There are currently two binaries in `crates/glyph-cli`:

- `glyph-cli`: a direct compiler driver (check/build/run)
- `glyph`: the primary workflow tool for working with Glyph projects and files

Use the `glyph` executable for day-to-day Glyph work; reserve Cargo for building the toolchain.

### Compile or check a file

Parse + resolve types (and, if you point at a file inside a folder, it will discover and check all `.glyph` files in that project directory):

```bash
glyph-cli check examples/std_hello/hello.glyph
```

Emit LLVM IR to stdout:

```bash
glyph-cli build tests/fixtures/codegen/simple_ret.glyph
```

Build a native executable (writes `./<stem>` in the current working directory):

```bash
glyph-cli build examples/std_hello/hello.glyph --emit exe
```

Build and run:

```bash
glyph-cli run examples/std_hello/hello.glyph
```

### Project workflow (`glph.toml`)

The `glyph` binary looks for `glph.toml` in the current directory (or parents) and builds targets listed under `[[bin]]`.

Example `glph.toml`:

```toml
[package]
name = "hello"
version = "0.1.0"

[[bin]]
name = "hello"
path = "hello.glyph"
```

Build or run a selected bin (using the `glyph` executable):

```bash
glyph build --release
glyph run --bin hello -- arg1 arg2
```

## Language snapshots

Hello world using the stdlib (concise and LLM-friendly without being cryptic):

```glyph
from std import println

fn main() -> i32 {
  println("hello world")
  ret 0
}
```

A tiny function:

```glyph
fn add(a: i32, b: i32) -> i32 {
  ret a + b
}
```

Control flow:

```glyph
fn main() {
  if true {
    ret 1
  } else {
    ret 0
  }
}
```

FFI-style externs are supported (see `examples/puts_hello`).

## Tooling

- `glyphfmt` (`crates/glyphfmt`): formatter placeholder (currently validates syntax and echoes input)
- `glyphlsp` (`crates/glyphlsp`): LSP placeholder (currently probes a file and prints diagnostic counts as JSON)

## Repo layout

- `crates/glyph-frontend`: lexer/parser, module resolution, MIR lowering
- `crates/glyph-backend`: backend traits and LLVM codegen implementation
- `crates/glyph-core`: shared AST/MIR/types and diagnostics
- `crates/glyph-cli`: compiler and workflow CLIs
- `runtime/`: minimal C runtime used for formatting/I/O
- `examples/`: small end-to-end programs
- `tests/fixtures/`: language / codegen fixtures used by tests and demos

## Development

Run the full test suite:

```bash
cargo test
```

If you’re iterating on the frontend only (no LLVM required):

```bash
cargo test -p glyph-frontend
```

## Contributing

Issues and PRs are welcome, especially around:

- tightening type checking and diagnostics
- stabilizing the CLI and build workflow
- expanding the stdlib and runtime surface
- formatter and LSP becoming “real tools”

## License

Dual-licensed under MIT OR Apache-2.0 (see workspace metadata in `Cargo.toml`).

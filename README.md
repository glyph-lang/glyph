```
 ██████╗ ██╗  ██╗   ██╗██████╗ ██╗  ██╗
██╔════╝ ██║  ╚██╗ ██╔╝██╔══██╗██║  ██║
██║  ███╗██║   ╚████╔╝ ██████╔╝███████║
██║   ██║██║    ╚██╔╝  ██╔═══╝ ██╔══██║
╚██████╔╝███████╗██║   ██║     ██║  ██║
 ╚═════╝ ╚══════╝╚═╝   ╚═╝     ╚═╝  ╚═╝
```

**The programming language designed for the LLM era.**

[![CI](https://github.com/svenkratz/glyph/workflows/CI/badge.svg)](https://github.com/svenkratz/glyph/actions) ![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue) ![Rust Edition 2024](https://img.shields.io/badge/rust-edition%202024-orange)

Glyph is an experimental compiled systems language with a **small, predictable syntax** that LLMs can generate correctly and humans can read effortlessly. Powered by Rust and LLVM, it produces native binaries with zero runtime overhead.

> **Status:** Early prototype. The CLI and language are evolving quickly. This is a research project pushing the boundaries of LLM-oriented language design.

---

## ⚡ Why Glyph?

Traditional languages were designed for human programmers typing at keyboards. Glyph is rethinking this for an era where **AI assistants generate much of our code**.

**The Design Principles:**

- **Small keyword set** — Just 18 keywords total (`fn`, `let`, `mut`, `ret`, `if`, `else`, `for`, `while`, `match`, `struct`, `enum`, `impl`, `use`, `pub`, `type`, `break`, `cont`). Fewer concepts for LLMs to get wrong.

- **Regular, deterministic grammar** — No macros, no context-dependent parsing, no special cases. One obvious way to do things.

- **Terse but readable** — `fn` not `function`, `ret` not `return`, semicolons optional. Short enough to scan quickly, clear enough to understand.

- **Predictable type system** — Strong static typing with inference for locals. No hidden behavior.

- **Native performance** — LLVM backend producing real executables. No garbage collector, no runtime overhead.

**The result?** A language where LLMs make fewer syntax errors, generate more consistent code, and the output remains readable to humans.

---

## 🚀 See It In Action

**Hello World** — clean and simple:

```glyph
from std import println

fn main() -> i32 {
  println("hello world")
  ret 0
}
```

**Working with generics and collections:**

```glyph
from std/vec import Vec
from std import println

fn main() -> i32 {
  let v: Vec<i32> = Vec::with_capacity(2)
  v.push(10)
  v.push(20)
  v.push(30)

  let third: i32 = v[2]
  println($"Element at index 2: {third}")
  ret third
}
```

**Pattern matching with match expressions:**

```glyph
fn classify(opt: Option<i32>) -> &str {
  match opt {
    Some(n) => {
      if n < 0 { ret "negative" }
      if n < 10 { ret "small" }
      ret "large"
    },
    None => ret "empty",
  }
}

fn handle_result(res: Result<File, Error>) -> i32 {
  match res {
    Ok(file) => {
      let _ = file.write_string("success")
      0
    },
    Err(e) => 1,
  }
}
```

**Error handling with ? operator:**

```glyph
fn read_file(path: &str) -> Result<String, Error> {
  let f = open(path)?
  let s = f.read()?
  ret Ok(s)
}
```

**Structs and methods:**

```glyph
struct Point {
  x: f64
  y: f64
}

fn distance(p: &Point) -> f64 {
  ret sqrt(p.x * p.x + p.y * p.y)
}
```

---

## ✨ What Works Today

This is not the final language spec, but these features are working now (see `examples/` and `tests/fixtures/`):

**Core Language**
- Function declarations with `fn`, local variables with `let`/`let mut`
- Control flow: `if`/`else`, `while`, `for`, `match` (expression-oriented)
- Return statements with `ret`
- Primitive types: `i32`, `u32`, `i64`, `u8`, `bool`, `f64`, `char`

**Type System**
- Generics with monomorphization: `Vec<T>`, `Map<K,V>`, `Option<T>`, `Result<T,E>`
- Structs (product types) and enums (sum types with payloads)
- Type inference for local variables
- Ownership and borrowing: `&T` (shared), `&mut T` (exclusive)
- Trait-like interfaces for polymorphism

**Collections & I/O**
- Generic collections: `Vec<T>`, `Map<K,V>`
- String support: string literals, `&str` slices, owned `String`
- File I/O via `std::io::File`
- Command-line arguments via `std::sys::argv`
- Standard output: `std::print`, `std::println` with formatting

**Advanced Features**
- Multi-file projects with module discovery
- C FFI via `extern "C"` declarations
- Error propagation with `?` operator
- Arrays with bounds checking

---

## 📦 Quickstart

### Prerequisites

- **Rust** (workspace uses Rust edition 2024)
- **LLVM** (used via `llvm-sys` for code generation)

On macOS with Homebrew:

```bash
export LLVM_SYS_201_PREFIX="/opt/homebrew/opt/llvm"
```

### Build the Toolchain

```bash
cargo build --release
```

### Try It Out

```bash
# Run the demo (shows source + generated LLVM IR)
./demo.sh

# Check a file (parse + type check)
glyph-cli check examples/std_hello/hello.glyph

# Build and run
glyph-cli run examples/std_hello/hello.glyph

# Build a native executable
glyph-cli build examples/std_hello/hello.glyph --emit exe
./hello
```

---

## 📚 Documentation

The mdBook documentation lives in `docs/book/`. Build it with:

```bash
mdbook build docs/book
```

**Design documents** in `docs/design/` cover:
- `LLMS.md` — Token efficiency and LLM-oriented design principles
- `GENERICS.md` — Generic type system implementation
- `VEC_DESIGN.md` — Generic vector (`Vec<T>`) design
- `MAP.md` — Associative container (`Map<K,V>`) design
- `INTERFACES_DESIGN.md` — Trait/interface system
- Plus many more covering enums, ownership, networking, formatting, etc.

---

## 🏗️ Project Workflow

Glyph supports project-based workflows with `glph.toml`:

```toml
[package]
name = "hello"
version = "0.1.0"

[[bin]]
name = "hello"
path = "hello.glyph"
```

Then use the `glyph` executable:

```bash
glyph build --release
glyph run --bin hello -- arg1 arg2
```

---

## 🔧 Compiler Architecture

The repo contains a complete working compiler pipeline:

**Frontend** (`crates/glyph-frontend`)
- Lexer + parser (using `nom` parser combinators)
- Multi-module resolution and symbol resolution
- Type inference and checking
- Generic monomorphization
- MIR lowering (SSA-ish intermediate representation)

**Backend** (`crates/glyph-backend`)
- LLVM IR code generation
- Native object emission and linking
- Integration with system libraries

**Core** (`crates/glyph-core`)
- Shared AST, MIR, and type system definitions
- Diagnostic and error reporting infrastructure

**Runtime** (`runtime/`)
- Minimal C runtime for formatting and I/O
- Linked with generated binaries

**CLI Tools** (`crates/glyph-cli`)
- `glyph-cli` — Direct compiler driver (check/build/run)
- `glyph` — Project workflow tool (build via `glph.toml`)

**Future Tooling** (placeholders)
- `glyphfmt` — Code formatter
- `glyphlsp` — Language server for IDE integration

---

## 🧪 Development

Run the full test suite:

```bash
cargo test
```

Frontend-only tests (no LLVM required):

```bash
cargo test -p glyph-frontend
```

---

## 🗂️ Repository Layout

```
crates/
  glyph-frontend/   # Lexer, parser, resolver, MIR lowering
  glyph-backend/    # LLVM codegen and linking
  glyph-core/       # Shared AST/MIR/types and diagnostics
  glyph-cli/        # CLI binaries (glyph, glyph-cli)
  glyphfmt/         # Formatter (placeholder)
  glyphlsp/         # LSP server (placeholder)

runtime/            # Minimal C runtime for I/O
examples/           # Working end-to-end programs
tests/fixtures/     # Language and codegen test fixtures
docs/               # mdBook documentation and design docs
```

---

## 🤝 Contributing

This is an experimental research project! Issues and PRs are welcome, especially around:

- **Type checking and diagnostics** — Making error messages even better
- **Stdlib expansion** — Adding more useful standard library functions
- **CLI and build workflow** — Improving the developer experience
- **Formatter and LSP** — Turning placeholders into real tools
- **Language design** — Proposing improvements to syntax or semantics

---

## 📖 Philosophy

###  Glyph Optimizes

1. **LLM correctness** — Simpler grammar means fewer syntax errors when AI generates code
2. **Meta-instruction efficiency** — When you prompt "write Glyph code to do X", the language rules themselves are simpler to explain
3. **Consistency** — One obvious way to do things reduces the decision space for LLMs
4. **Learnability** — Smaller surface area means faster learning for both humans and AI
5. **LLM reasoning efficiency** — Validated through rigorous experimentation (see below)

### Experimental Validation

We ran a **490-experiment study** comparing LLM reasoning costs (total tokens to generate code) across 7 languages with statistical significance testing:

**Results (Mean ± SD tokens):**
1. 🥇 Rust: 239.7 ± 91.5
2. 🥈 **Glyph: 242.4 ± 93.1** ✅ (statistically tied with Rust!)
3. 🥉 Python: 255.5 ± 150.3
4. Go: 273.9 ± 103.5
5. C: 336.7 ± 160.2
6. C++: 339.9 ± 180.0
7. Java: 385.1 ± 238.2

**Key Findings:**
- ✅ **Glyph matches Rust's efficiency** despite being **much simpler** (18 keywords vs Rust's 50+)
- ✅ **Glyph dominates error handling** — tied with Rust, but 259 tokens better than Java (p<0.001, Cohen's d = -3.09)
- ✅ **Simplicity works** — Glyph beats all compiled languages on algorithmic tasks (fibonacci)
- ✅ **Statistical rigor** — 7 repetitions per condition, paired t-tests, 95% CIs

**The breakthrough:** Glyph proves that **radical simplicity** (fewer keywords, regular grammar, explicit error handling) achieves world-class LLM reasoning efficiency.

See `experiments/llm-reasoning-cost/` for full methodology and results.

### Glyph's Value Proposition

Glyph demonstrates that **a language designed for predictability and regularity** can match or beat established languages for LLM code generation — while being dramatically simpler to learn and use.

---

## 📄 License

Dual-licensed under **MIT OR Apache-2.0** (see workspace metadata in `Cargo.toml`).

---

**Built with Rust 🦀 | Powered by LLVM ⚡ | Designed for AI 🤖**

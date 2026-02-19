---
name: glyph-lsp
description: Glyph language compiler diagnostics and LSP usage
triggers:
  - "*.glyph"
  - glyph compiler
  - glyph check
  - glyph diagnostics
---

# Glyph Language & Compiler

## Language Overview

Glyph is a compiled, expression-oriented language with ownership semantics. Key features:
- **Keywords:** `fn`, `let`, `mut`, `ret`, `if`, `else`, `for`, `while`, `match`, `struct`, `enum`, `impl`, `use`, `pub`, `type`, `break`, `cont`, `const`, `from`, `import`, `as`, `in`, `extern`, `interface`, `case`
- **Built-in types:** `i8`, `i32`, `i64`, `u8`, `u32`, `u64`, `usize`, `f32`, `f64`, `bool`, `char`, `str`, `String`
- **Operators:** `+`, `-`, `*`, `/`, `%` (integer remainder), `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`, `?` (error propagation on `Result` types)
- **Comments:** `//` line comments, `/* */` block comments
- **String literals:** `"hello"`, hex escape sequences (`"\x1B[31m"` for `\xHH`), string interpolation (`$"x = {x}"`)
- **Void functions:** functions with no `-> Type` annotation return void; void calls can appear mid-block
- **For-in loops:** `for x in collection { ... }` iterates over collections (AST: `Expr::ForIn`)
- **`?` operator:** works on `Result` types for error propagation (`let val = expr?`)
- **Case system:** projects organized into cases via `glyph.toml` with `[dependencies]` for local path-based deps
- **Modules:** one file = one module, imports via `from module import Symbol` or `import module`

## Checking Glyph Code

To check a glyph source file for errors, run:

```bash
cargo run -p glyph-cli -- check <path-to-file.glyph>
```

Or if the `glyph` binary is built:

```bash
glyph check <path-to-file.glyph>
```

### Diagnostic output format

```
<file>:<line>: <severity>: <message>
```

Example:
```
src/main.glyph:5: error: unknown type 'Foo'
src/utils.glyph:12: warning: unused variable 'x'
```

### Probe mode (JSON diagnostics)

For programmatic checking of a single file:

```bash
cargo run -p glyphlsp -- probe <path-to-file.glyph>
```

Returns JSON: `{ "diagnostics": <count> }`

## Project Structure

- `glyph.toml` - project manifest (if present)
- `src/` directory contains `.glyph` source files
- Module names derived from file paths: `src/math/geometry.glyph` becomes module `math/geometry`
- Entry point is typically `src/main.glyph` or the file passed to `check`/`build`

## Import Syntax

```glyph
// Selective import
from utils import helper
from math/geometry import Point

// Wildcard import
import std

// Aliased import
from graphics import Point as GfxPoint

// Standard library
from std import println
from std/io import read, write, print_str
```

## Common Error Patterns

| Error | Cause | Fix |
|-------|-------|-----|
| `unknown type 'X'` | Type not in scope | Add import or define the type |
| `unknown import: module 'X' not found` | Module file doesn't exist | Create the `.glyph` file or fix the import path |
| `symbol 'X' not found in module 'Y'` | Symbol not exported by module | Check spelling, ensure the function/struct is defined in the target module |
| `circular import detected` | Two modules import each other | Refactor to break the cycle |
| `type mismatch` | Expression type doesn't match expected | Check types in assignments, function args, and return values |
| `void type mismatch` | Using void call result as a value | Void functions (no `-> Type`) cannot be used in expressions |
| `break outside of loop` | `break`/`cont` used outside loop body | Move `break`/`cont` inside a `while` or `for` block |
| `unknown function 'X'` | Function not in scope | Add import or define the function before use |

## LSP Integration

When the `glyphlsp` LSP server is running in VSCode (via the glyph-lang extension), diagnostics appear as editor squiggles in real-time. You can also read diagnostics via the `getDiagnostics` MCP tool when available.

## Workspace Layout

```
glyph/
  crates/
    glyph-core/src/         # AST (ast.rs), MIR (mir.rs), types, spans, diagnostics
    glyph-frontend/src/
      lexer.rs               # Tokenizer
      parser/                # Modularized parser (mod.rs, expr.rs, items.rs, stmts.rs, types.rs, imports.rs)
      resolver/              # Modularized resolver (mod.rs, const_eval.rs, imports.rs, methods.rs, validation.rs)
      mir_lower/             # Modularized MIR lowering (mod.rs, expr.rs, flow.rs, call.rs, context.rs, etc.)
      stdlib.rs              # Standard library definitions
      stdlib/                # Stdlib sub-modules
    glyph-backend/src/
      codegen.rs             # Codegen entry point
      codegen/               # Modularized codegen (rvalue.rs, functions.rs, types.rs, string.rs, vec.rs, etc.)
      linker.rs              # Native linking
    glyph-cli/               # CLI toolchain (check, build, run)
    glyphlsp/                # LSP server
    glyphfmt/                # Formatter
  editors/
    vscode/                  # VSCode extension
  runtime/                   # Minimal C runtime for formatting and I/O
  examples/                  # Example glyph projects
  tests/fixtures/
    parse/                   # Parser test cases
    mir/                     # MIR lowering test cases (44 fixtures)
    codegen/                 # Codegen test cases (37 fixtures)
```

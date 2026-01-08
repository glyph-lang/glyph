# LLMS.txt — Glyph (token-efficient language for LLMs)

## Purpose
Glyph is a compiled, token-efficient, strongly typed programming language designed to be easy for LLMs to emit correctly and compactly, while remaining familiar to Rust/Swift users.

Primary goals:
- minimize token count in typical code
- deterministic, regular grammar (low ambiguity)
- strong static typing with type inference for locals
- no garbage collector (RAII + ownership/borrowing)
- imperative first; functional helpers via standard library
- no macros; no templates; minimal generics
- target LLVM for codegen (keep implementation reasonable)

Non-goals:
- metaprogramming via macros
- C++-style templates / TMP
- complicated contextual syntax tricks
- building/maintaining a custom backend in v0

---

## Token-efficiency design principles (BPE-aware)
- Small fixed keyword set; avoid keyword proliferation.
- Prefer common ASCII punctuation and consistent shapes.
- Minimize mandatory noise:
  - newline ends statements (semicolon optional)
  - return is last expression by default
  - type annotations optional for locals (inference)
- Make common patterns short:
  - Result/Option with `?`
  - iterators: `map/filter/fold/collect`
- One obvious way: avoid many equivalent spellings.

---

## Core surface syntax
- Statements end at newline; `;` permitted but optional.
- Blocks are `{ ... }` always (indentation not semantic).
- Expression-oriented control flow (Rust-like): `if`, `match` yield values.

Examples:
  fn add(a: i, b: i) -> i { a + b }

  let x = foo()
  let mut n: i = 0

  let y = if x > 0 { x } else { -x }

  fn f(x: i) -> i { if x == 0 { ret 1 } x * f(x - 1) }

---

## Keywords (target set; keep small)
Declarations: fn let mut type struct enum impl use pub
Control: if else for while match break cont
Misc: ret

Everything else is library.

---

## Type system
- Strong static typing.
- Type inference:
  - locals inferred by default (`let x = ...`)
  - public function params require annotations (preferred)
  - return type may be inferred for non-public functions (optional rule)
- Traits/protocols are supported but intentionally lean.

Primitive types:
- i, u, f, b, c (int, uint, float, bool, char)
- sized variants: i32, u8, f64, etc.
- str (UTF-8)
- &[T] slice, [T; N] fixed array (exact spelling flexible)

Common std types:
- Option[T], Result[T, E]
- Vec[T], Box[T]

---

## Algebraic Data Types (ADTs)
Glyph supports algebraic types (sum + product) via:
- product types: `struct` (records/tuples as sugar allowed)
- sum types: `enum` with payloads (tagged unions)
- pattern matching: `match` exhaustively checks variants

Examples:
  struct Pt { x: f, y: f }

  enum Maybe[T] {
    None
    Some(T)
  }

  enum Result[T, E] {
    Ok(T)
    Err(E)
  }

  fn head[T](xs: &[T]) -> Maybe[&T] {
    if xs.len() == 0 { None } else { Some(&xs[0]) }
  }

  fn show_res[T, E](r: Result[T, E]) -> str {
    match r {
      Ok(v)  => "ok"
      Err(e) => "err"
    }
  }

Rules:
- `match` should be exhaustive (or require `_`).
- Constructors can be in scope for brevity: `Ok(x)`, `Err(e)`, `Some(v)`, `None`.

---

## Memory model (no GC)
- No garbage collector.
- Deterministic destruction via RAII (drop at scope end).
- Ownership + borrowing (Rust-inspired):
  - moves by default
  - `&T` shared borrow
  - `&mut T` exclusive borrow
- Lifetimes/regions inferred in most cases; explicit lifetime params only at ambiguous public boundaries (policy).

Allocation is explicit and library-driven:
- Vec, Box, Arena/Pool as opt-in tools.
- No hidden allocations in core language constructs.

---

## Errors
- Prefer `Result[T, E]` + propagation operator `?` (token win and ergonomics).

Example:
  fn read_file(p: &str) -> Result[str, IoErr] {
    let f = fs.open(p)?
    let s = f.read_all()?
    Ok(s)
  }

---

## Generics (minimal)
- Allowed: generic functions + generic structs/enums.
- Constraints only through traits: `fn f[T: Show](x: &T) { ... }`
- No templates, no specialization, no metaprogramming.
- Escape hatch: trait objects (`dyn Trait`) for runtime polymorphism.

---

## Functional helpers (library)
Imperative by default; functional style via iterators:
- .iter(), .map(), .filter(), .fold(), .sum(), .collect()
Closures:
- `|x| expr` or `|x| { ... }`

Example:
  let total = xs.iter()
    .filter(|x| *x > 0)
    .map(|x| x * 2)
    .sum()

---

## Compilation model (LLVM target)
## Compilation model (LLVM target)
- Frontend:
  - lexer (simple, deterministic)
  - parser built with **nom** (parser combinators; zero-copy where possible)
  - AST construction
  - name resolution
  - type inference + trait solving
  - borrow/region checks
  - lowering to a small MIR (SSA-ish, explicit drops)

- Backend:
  - MIR → LLVM IR
  - use LLVM for optimization, object emission, linking integration
  - optional secondary target via LLVM: WASM (later)

- Runtime:
  - minimal core/alloc libraries
  - no required GC/runtime; only what libraries opt into

LLVM implications:
- ADTs lower to tagged unions (struct { tag, payload })
- enums with data use layout rules (niche optimization optional later)
- drops become explicit MIR terminators and lower to calls / inline drops
- debug info via LLVM DI (later)

---

## Tooling goals
- Formatter and LSP are easy because syntax is regular and no macros.
- Stable, predictable, BEAUTIFUL (try to exceed the Rust compiler's messages!) error messages (good for LLM correction loops).

---

## Command Line tools
glyph        # main driver (like `rustc` or `swift`).   (*)
glyphc       # compiler frontend (source → object / LLVM IR) (*)
glyphfmt     # formatter
glyphlsp     # language server (LSP)
glyphdoc     # documentation generator
glyphpkg     # package/dependency manager (optional, later)
glyphrun     # compile + run (thin wrapper, optional)
glyphtest    # test runner (optional)

(*) we will focus on glyph and glyphc first and foremost
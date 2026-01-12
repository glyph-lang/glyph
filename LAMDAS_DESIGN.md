# LAMBDAS_DESIGN.txt — Glyph Lambda / Closure Design Brief

## Purpose
Introduce first-class lambdas (closures) into Glyph to enable:
- iterator adapters (map, filter, fold)
- concise callbacks
- expressive but token-efficient functional helpers
- zero-cost abstractions compatible with LLVM lowering

Lambdas are intentionally lightweight and secondary to imperative code.

---

## Selected syntax (locked)
Glyph lambdas use **arrow syntax**:

  x -> x + 1
  x, y -> x + y
  x -> { let y = x * 2; y + 1 }

This syntax is final for v0.

Rationale:
- minimal punctuation
- extremely common in modern languages (JS, Kotlin, Scala)
- easy for LLMs to emit
- unambiguous when combined with a restricted grammar
- visually compact and readable

Rejected syntaxes:
- |x| x+1        (Rust-style pipes)
- { x => x+1 }  (brace ambiguity)
- fn(x) => ...  (too verbose)

---

## Grammar
A lambda expression has the form:

  <params> -> <body>

Where:
- <params> is one or more identifiers separated by commas
- <body> is either:
  - a single expression
  - a block `{ ... }`

Examples:
  a -> a * a
  a, b -> a + b
  x -> { let y = x * 2; y + 1 }

Parsing rule:
- `->` has **very low precedence**
- the entire lambda is parsed as a single expression
- lambdas bind looser than arithmetic, tighter than assignment

---

## Parameters
Rules:
- identifiers only in v0
- no destructuring
- no default values
- no patterns
- optional type annotations allowed when inference fails

Examples:
  x: i -> x + 1
  x: i, y: i -> x + y

---

## Typing model
### Function types
Conceptually:
- non-capturing lambdas behave like plain functions
- capturing lambdas behave like closures with environments

Internal callable categories:
- Fn      (shared borrow capture)
- FnMut   (mutable borrow capture)
- FnOnce  (move capture)

These traits may exist internally but do not need heavy surface exposure.

---

## Type inference
- parameter types inferred from usage context
- return type inferred from body expression
- failure to infer requires explicit annotation

Example:
  xs.map(x -> x + 1)

If ambiguous:
  xs.map((x: i) -> x + 1)

---

## Capture semantics (no GC)
Glyph lambdas follow Rust-like capture rules.

### Default capture behavior
- read-only use → capture by shared borrow
- mutation → capture by mutable borrow
- move/consume → capture by value

Examples:

Borrow capture:
  let k = 3
  xs.map(x -> x + k)

Mutable borrow capture:
  let mut sum = 0
  xs.for_each(x -> { sum = sum + x })

Move capture:
  let v = Vec<i>.new()
  xs.for_each(x -> v.push(x))   // v moved, lambda is FnOnce

---

## Explicit move capture (v0.1)
Optional future syntax:
  move x -> x + k

Forces all captures to be by-value.

---

## Lifetime and borrow checking
- captured references participate fully in borrow checking
- lambdas cannot outlive captured borrows
- enforced during MIR borrow/region analysis
- no runtime checks

---

## Lowering model
### Non-capturing lambdas
Lower to plain functions:

  fn __lambda_12(x: T) -> R { ... }

Lambda expression evaluates to a function pointer.

No allocation. No environment.

---

### Capturing lambdas
Lower to:
- an environment struct
- an invoke function

Conceptual lowering:

  struct __Env12 {
    cap0: &T
    cap1: U
  }

  fn __invoke12(env: *__Env12, x: A) -> R { ... }

Calling convention depends on capture kind:
- Fn:    &env
- FnMut: &mut env
- FnOnce: env (moved)

Environment allocation:
- stack-allocated if lambda does not escape
- heap allocation only via explicit boxing (future)

---

## Escaping closures
v0 rules:
- lambdas may be passed to generic functions
- lambdas may be inlined / monomorphized
- storing lambdas in structs or returning them requires:
  - monomorphization, or
  - explicit boxing (deferred)

Dynamic dispatch (`dyn Fn`) is optional and deferred.

---

## Standard library usage targets
Lambdas must support:

  xs.map(x -> x + 1)
  xs.filter(x -> x > 0)
  xs.fold(0, (a, x) -> a + x)
  xs.for_each(x -> { print(x) })

---

## Restrictions (v0)
- no destructuring parameters
- no pattern parameters
- no async lambdas
- no generators
- no FFI passing of lambdas
- no variadics

---

## Token-efficiency notes
- `x -> x+1` is the canonical idiom
- parentheses avoided entirely
- commas only when arity > 1
- block bodies only when necessary
- no keywords in common lambdas

This syntax is deliberately optimized for:
- LLM emission accuracy
- compact diffs
- visual scanability

---

## Diagnostics
Compiler must emit clear errors for:
- ambiguous parameter types
- borrow violations in captures
- use-after-move due to capture
- lambda escaping with borrowed captures

---

## Implementation checklist
v0:
- parse arrow lambdas
- integrate lambda expressions into AST
- type inference for params + return
- capture analysis
- MIR lowering (env + invoke)
- borrow checking across lambdas

v0.1:
- explicit `move` lambdas
- trait object closures
- inline heuristics

---

End of file.

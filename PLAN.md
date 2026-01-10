# Glyph Implementation Plan

Status key: `[ ]` not started, `[~]` in progress, `[x]` done.

## High-level tracks
- [~] A: Frontend (lexer → parser → resolver → type/borrow → MIR)
- [~] B: Backend (MIR → LLVM via llvm-sys pinned to 21.1.x)
- [~] C: CLI & tooling (commands, diagnostics UI, glyphfmt/glyphlsp stubs)

## Workspace layout
- [x] Root Cargo workspace
- [x] `glyph-core`: tokens/spans, AST/MIR/types, diagnostics, interning helpers
- [x] `glyph-frontend`: lexer/parser/resolver/type+borrow checker, MIR lowering API
- [x] `glyph-backend`: MIR→LLVM IR/object; internal backend interface over llvm-sys
- [x] `glyph-cli`: build/check/run commands, diagnostics rendering
- [x] `glyphfmt`: placeholder parse→normalize formatter
- [x] `glyphlsp`: placeholder LSP server (parse+diagnose only)

## Milestones
- [x] M0: Workspace scaffolding + core data types/tests harness
- [x] M1: Lexer+parser passing golden fixtures (expr/stmt/if/call/let parsing in place)
- [x] M2: LLVM backend with JIT execution - EXCEEDED (skipped basic resolver for now)
- [x] M3: MIR lowering stable; glyphfmt/glyphlsp minimal stubs (locals, consts, binary temps; if/else blocks + logical short-circuit lowering in place; MIR fixtures/snapshots added)
- [x] M4: LLVM IR lowering for core constructs; snapshot IR tests - COMPLETE
- [x] M5: CLI `build/check/run` end-to-end fixture tests - COMPLETE
- [x] M6: Struct support - COMPLETE (all 6 phases: AST, Parser, Resolver, MIR, Codegen, Integration)
- [x] M7: Function calls - COMPLETE (MIR lowering, LLVM codegen, recursion, forward references)
- [x] M8: Fixed-size arrays (parser → MIR → bounds-checked LLVM, arr.len, for-loop integration)
- [ ] M9: Heap ownership pointers (Own<T>, RawPtr<T>)
- [~] M10: Interfaces (parsing/validation/MIR + method dispatch) – LLVM/codegen validation + docs/tests remaining

## TDD & fixtures
- [ ] Fixture tree under `tests/fixtures/{lex,parse,resolution,typeck,borrow,mir,ir,cli}`
- [~] Snapshot testing with `insta` for AST/MIR/IR and diagnostics (34 MIR snapshots incl. arrays/refs/loops/owns)
- [~] Property tests via `proptest` for lexer/parser spans and panic-freedom (lexer spans covered)
- [ ] Backend IR tests behind `codegen` feature to avoid LLVM in fast loop
- [ ] CI flow: fmt (once available), clippy, test (workspace)

## Completed (Jan 8, 2026)
- [x] Capture plan in repo (this file)
- [x] Convert repo to workspace + crates scaffolding
- [x] Add minimal interfaces + stub tests per crate
- [x] Wire CLI commands to frontend/backend stubs
- [x] Add placeholder glyphfmt/glyphlsp commands and tests
- [x] Implement lexer + tests (basic keywords/idents/literals/operators)
- [x] Add parser skeleton and golden tests (expr/stmt support)
- [x] Expand parser coverage (more expr/stmt forms, source map fidelity)
- [x] Populate fixtures tree for lex/parse and future phases
- [x] LLVM backend implementation with JIT execution
- [x] Implicit returns (Rust-style last expression)
- [x] Struct AST nodes (StructDef, FieldDef, StructLit, FieldAccess)
- [x] Struct parsing (definitions, literals, field access)

## Struct Support: COMPLETE ✅
**Status:** All 6 phases complete and committed

**Completed Work:**
- [x] Phase 1-2: AST Extension + Parser
  - Struct definitions, literals, field access parsing
  - 3 parser test fixtures

- [x] Phase 3: Type Resolution
  - resolver.rs with StructType registry
  - Two-pass resolution with validation
  - 6 unit tests for error detection

- [x] Phase 4: MIR Extension
  - StructLit and FieldAccess Rvalue variants
  - struct_types in MirModule
  - 3 MIR snapshot tests + 2 unit tests

- [x] Phase 5: LLVM Codegen
  - Two-pass struct registration (LLVMStructCreateNamed + SetBody)
  - GEP-based field access codegen
  - 3 integration tests verifying LLVM IR

- [x] Phase 6: Integration
  - Full pipeline working (Lex → Parse → Resolve → MIR → LLVM)
  - 20+ tests passing
  - Documentation updated

**Example Working Code:**
```glyph
struct Point { x: i32, y: i32 }
fn main() -> i32 {
  let p = Point { x: 10, y: 20 }
  ret p.x + p.y  // compiles and generates correct LLVM IR
}
```

## Arrays: INTEGRATED ✅

**Highlights:**
- Parser/type resolver understand `[T; N]` annotations and literals.
- MIR has `ArrayLit`, `ArrayIndex`, and `ArrayLen` rvalues with inferred element types.
- LLVM backend emits stack-allocated literals, bounds-checked indexing (traps via `llvm.trap`), and constant-length queries.
- Fixtures cover basic literals, indexing, `.len()`, and for-loop integration; CLI codegen tests assert GEPs + traps.

## Interfaces: In Progress (Phases 0–5 ✅)

- Parser supports `interface` definitions, struct declarations of interfaces, inline/`impl for` implementations, and method-call syntax.
- Resolver validates interface signatures, collects impls, checks missing/duplicate/unknown methods, and tracks self by value/ref/mut.
- MIR lowering resolves method calls across inherent + interface impls with auto-borrowing; builtin `.len()`/`into_raw()` handled early.
- Remaining: dedicated LLVM verification + docs/examples; broaden fixture coverage.

## Function Calls: COMPLETE ✅

**Status:** Fully implemented and tested

**Completed Work:**
- [x] MIR lowering with argument validation
- [x] LLVM codegen with proper function type handling
- [x] Recursion support (factorial tested)
- [x] Forward references (function signature collection)
- [x] Struct parameters and return values
- [x] 5 integration tests + 2 MIR snapshot tests

**Example Working Code:**
```glyph
fn factorial(n: i32) -> i32 {
  if n <= 1 { ret 1 }
  else { ret n * factorial(n - 1) }
}

fn main() -> i32 {
  ret factorial(5)  // returns 120
}
```

## Heap Ownership Pointers: Own<T> + RawPtr<T> (NEXT)

Goal: introduce deterministic heap allocation with move-only `Own<T>` (Box-like) and unsafe `RawPtr<T>` escape hatches, following `own.txt` semantics.

### Phase plan
1. **Type System + Parsing**
   - Extend `Type` enum with `Own(Box<Type>)`, `RawPtr(Box<Type>)`, helpers, and serialization.
   - Accept `Own<T>` / `RawPtr<T>` in annotations via a minimal angle-bracket parser; reject unsized inners for now.
   - Update resolver + diagnostics for unknown inner types / malformed generics.
2. **MIR + Borrowing Rules**
   - Add rvalues for `Own::new`, `into_raw`, `from_raw`, raw pointer ops, and `Drop` terminators.
   - Track move-only state for `Own` locals (use-after-move diagnostics) and auto-drop on scope exits.
   - Support auto-deref of `Own<T>` in field/index access and `&Own` borrowing rules.
3. **Backend & Runtime**
   - Introduce heap runtime hooks (`malloc`, `free`, `drop_in_place`) and emit LLVM IR for allocation, destruction, and conversions.
   - Reuse existing panic/bounds infrastructure for null/dangling checks; ensure drop glue runs on all exit paths.
4. **Safety/Diagnostics**
   - Forbid implicit copies; surface errors for double drops, missing unsafe on `from_raw`, or dereferencing null raw pointers.
   - Document interaction with arrays/loops and planned future `unsafe` blocks.
5. **Testing**
   - MIR fixtures for ownership transfer, move errors, raw conversions, recursive structs (e.g., linked list).
   - Codegen tests asserting `malloc/free` calls and drop glue; CLI fixture exercising `Own` in a loop.

## Next Session: Execution Order

1. **Heap Ownership pointers (`Own<T>`, `RawPtr<T>`).**
2. Enums + pattern matching.
3. Borrow checker & lifetimes.
4. Generics + trait system (longer-term).

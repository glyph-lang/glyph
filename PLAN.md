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
- [~] M1: Lexer+parser passing golden fixtures (expr/stmt/if/call/let parsing in place)
- [ ] M2: Resolver + type/borrow checker with diagnostics fixtures
- [~] M3: MIR lowering stable; glyphfmt/glyphlsp minimal stubs (locals, consts, binary temps; if/else blocks + logical short-circuit lowering in place; MIR fixtures/snapshots added)
- [ ] M4: LLVM IR lowering for core constructs; snapshot IR tests
- [ ] M5: CLI `build/check/run` end-to-end fixture tests

## TDD & fixtures
- [ ] Fixture tree under `tests/fixtures/{lex,parse,resolution,typeck,borrow,mir,ir,cli}`
- [~] Snapshot testing with `insta` for AST/MIR/IR and diagnostics (lex+parser stages in place; if/call/let covered)
- [~] Property tests via `proptest` for lexer/parser spans and panic-freedom (lexer spans covered)
- [ ] Backend IR tests behind `codegen` feature to avoid LLVM in fast loop
- [ ] CI flow: fmt (once available), clippy, test (workspace)

## Immediate next steps
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
- [~] **CURRENT: Struct support (stack-allocated, copy semantics)**

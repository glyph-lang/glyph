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
- [~] M6: Struct support (AST + Parser complete; MIR/Codegen in progress)

## TDD & fixtures
- [ ] Fixture tree under `tests/fixtures/{lex,parse,resolution,typeck,borrow,mir,ir,cli}`
- [~] Snapshot testing with `insta` for AST/MIR/IR and diagnostics (lex+parser stages in place; if/call/let covered)
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

## Next Session: Struct Support Continuation
**Status:** Phases 1-2 complete (AST + Parser)

**Remaining Work:**
- [ ] Phase 3: Type Resolution (~2-3 hours)
  - Create `resolver.rs` with struct type registry
  - Build `StructType` metadata
  - Collect struct definitions in first pass
  - Resolve field types
  
- [ ] Phase 4: MIR Extension (~3-4 hours)
  - Add StructLit and FieldAccess to Rvalue enum
  - Add struct_types to MirModule
  - Lower struct expressions to MIR
  - Create MIR test fixtures
  
- [ ] Phase 5: LLVM Codegen (~4-5 hours)
  - Register LLVM struct types
  - Codegen struct literals (alloca + GEP + store)
  - Codegen field access (GEP + load)
  - JIT execution tests
  
- [ ] Phase 6: Integration (~2-3 hours)
  - Wire resolver into frontend pipeline
  - End-to-end tests
  - Documentation updates

**Design Decisions Made:**
- Stack-allocated structs only (no heap, no pointers in Phase 1)
- Copy semantics (like C structs)
- Type::Named already exists for struct types
- All tokens already in lexer - no changes needed

**Test Strategy:**
- TDD with snapshot tests via `insta`
- Fixtures for each phase
- End-to-end JIT execution tests

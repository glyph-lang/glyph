# Glyph Fixtures

## MIR fixtures

Files under `tests/fixtures/mir` are used by `crates/glyph-frontend/tests/mir_fixtures.rs`
to snapshot the MIR emitted by the frontend. Each `.glyph` file is a small program that
is parsed, lowered, and compared against an insta snapshot. Run `cargo test --package
glyph-frontend --test mir_fixtures` (or `cargo insta review`) after modifying the
lowering logic to refresh snapshots.

Current MIR limitations:

- Calls are lowered only when the callee is a bare identifier. Arguments are lowered to
  values but the call itself is kept as a placeholder `Rvalue::Call`; there is no actual
  effect or callee lookup yet.
- Logical `&&`/`||` use basic short-circuit control flow but still treat operands as
  booleans (non-boolean operands are coerced via `!= 0`).

## Parser fixtures

Files under `tests/fixtures/parse` are consumed by
`crates/glyph-frontend/tests/parser_fixtures.rs` to snapshot the parsed `Module` AST.
Run `cargo test --package glyph-frontend --test parser_fixtures` whenever the parser is
updated so the snapshots stay in sync.

# Compiler Diagnostics Improvement Plan

TODO
[ ] Define diagnostic data model changes and compatibility rules
[ ] Design source map and line lookup helper for byte spans
[ ] Specify output formatting rules and fallback behavior
[ ] Enumerate code changes by file and function
[ ] Define testing and validation criteria
[ ] List risks, edge cases, and mitigations

## Context
Compiler diagnostics are currently emitted using Debug formatting (for example `eprintln!("{:?}", diag)`), which shows only the message, severity, and raw byte `Span`. There is no file path or line information in the output. This makes errors difficult to locate.

## Meta Goal
We want our diagnostics to be as polished and beautiful as Rust's compiler diagnostics. This plan is an incremental step toward that target, and future iterations should keep that end-state in focus.

## Goals
- Always print a file name (or module id fallback) for compiler diagnostics shown by the CLI tools.
- Provide an approximate line number for any diagnostic that has a byte span.
- Keep output stable and readable across `glyph-cli` (both `glyph` and `glyph-cli` binaries).
- Avoid large-scale changes in the frontend; prefer module-level tagging and CLI formatting.

## Non-Goals
- Full source snippets with caret underlines.
- Column numbers (line is sufficient for now).
- Changing the internal span representation (byte offsets stay as-is).
- Replacing diagnostics with a third-party framework (miette remains unused).
- LSP output improvements (currently only reports diagnostic count).

## Current State (Relevant Files)
- `crates/glyph-core/src/lib.rs`: `Diagnostic` contains `message`, `severity`, `span` only.
- `crates/glyph-frontend/*`: produces diagnostics with spans, but does not format output.
- `crates/glyph-cli/src/main.rs` and `crates/glyph-cli/src/glyph.rs`: print `{:?}` diagnostics.
- `crates/glyph-cli/src/module_loader.rs`: prints `{:?} in <path>` only for lex/parse errors during module discovery.

## Proposed Design

### 1) Diagnostic provenance: module id
Add an optional `module_id: Option<String>` to `glyph_core::diag::Diagnostic` so the compiler can attach a module context without changing all call sites. The CLI can then map `module_id` to a file path when available.

Rules:
- Existing diagnostics remain valid (module id default is `None`).
- Introduce a helper `with_module_id(self, module_id: impl Into<String>) -> Self` for minimal call-site changes.
- Only tag module ids at module-level boundaries (frontend compilation loops), not at every `Diagnostic::error` call.

### 2) Source map in the CLI
Extend module discovery to retain source text and path for each module:
- Add a `SourceInfo` struct in the CLI layer:
  - `path: PathBuf`
  - `source: String`
  - `line_starts: Vec<u32>` (byte offsets of each line start)
- Build `line_starts` once per source file by scanning for `\n` and pushing `0` plus each subsequent byte index.
- Provide `line_from_span(span_start: u32) -> usize` via binary search of `line_starts` (1-based line numbers).

### 3) Diagnostic formatting rules
Centralize formatting in a CLI helper (new file `crates/glyph-cli/src/diagnostics.rs`).

Output format (example):
```
path/to/file.glyph:12: error: expected `fn`, `struct`, `enum`, `interface`, `impl`, or `extern`
```

Formatting rules:
- If `module_id` maps to a known `SourceInfo`, print file path and line.
- If `span` is `None`, omit the line number and print `path: error: message`.
- If `module_id` is missing or unknown, fall back to `module_id` if present, otherwise `"<unknown>"`.
- Severity text should be lowercase: `error`, `warning`, `note`, `help`.

### 4) Where module ids get attached
Attach module ids at the top-level frontend compile loops where the current module id is known:
- `crates/glyph-frontend/src/lib.rs`
  - In `compile_modules(...)` and `compile_multi_module_graph(...)`, set `diag.module_id` on diagnostics returned by resolver, validator, and MIR lowering.
- `crates/glyph-frontend/src/module_resolver.rs`
  - When emitting diagnostics during dependency and import resolution, tag with the current `module_id` in the loop.

Note: Diagnostics produced by monomorphization may not have a clear module id. Leave them untagged and rely on fallback formatting.

## Implementation Plan (Detailed)

### A) Update Diagnostic type in core
File: `crates/glyph-core/src/lib.rs`
- Add `pub module_id: Option<String>` to `Diagnostic`.
- Update `Diagnostic::new`, `error`, and `warning` to initialize `module_id` as `None`.
- Add a helper:
  - `pub fn with_module_id(mut self, module_id: impl Into<String>) -> Self`

Compatibility notes:
- All existing diagnostics compile without changes if `module_id` has a default value.
- Serde derives stay valid because `Option<String>` is serializable.

### B) Tag module ids in frontend compilation
File: `crates/glyph-frontend/src/lib.rs`
- In `compile_multi_module_graph(...)`:
  - For each module id in `compile_order`, after calling `resolve_types`, `validate_named_types`, `validate_map_usage`, and `mir_lower`, map any diagnostics with `diag = diag.with_module_id(module_id.clone())`.
- In `compile_modules(...)`:
  - When `opts.include_std` is false and we compile only the entry module, tag diagnostics with the entry module id.

File: `crates/glyph-frontend/src/module_resolver.rs`
- In `build_dependency_graph(...)`, when creating diagnostics for missing imports, set the module id of the module currently being processed.

### C) Add source map and formatter in CLI
New file: `crates/glyph-cli/src/diagnostics.rs`
- Define `SourceInfo` with `path`, `source`, `line_starts`.
- Provide `build_line_starts(source: &str) -> Vec<u32>`.
- Provide `line_from_span(span_start: u32, line_starts: &[u32]) -> usize` (1-based).
- Provide `format_diagnostic(diag: &Diagnostic, sources: &HashMap<String, SourceInfo>) -> String`.

Update `crates/glyph-cli/src/module_loader.rs`:
- Change `discover_and_parse_modules(...)` to return both module ASTs and source info (for example a struct like `ModuleLoadResult { modules, sources }`).
- Avoid printing diagnostics inside this function. Return diagnostics to the caller so formatting is centralized.

Update `crates/glyph-cli/src/main.rs` and `crates/glyph-cli/src/glyph.rs`:
- Replace `eprintln!("{:?}", diag)` with `eprintln!("{}", format_diagnostic(...))`.
- Pass the source map obtained from module discovery.

### D) Update tests
- Adjust tests in `crates/glyph-cli/src/main.rs` that assume the old `discover_and_parse_modules(...)` signature.
- Add a unit test for line mapping in `crates/glyph-cli/src/diagnostics.rs`:
  - Example source with multiple lines and known byte offsets.
- Add a simple integration-style test for error formatting if feasible (optional if the test harness does not expose stderr easily).

## Testing and Validation Criteria

### Unit Tests
- `build_line_starts` produces correct line start offsets for:
  - Empty string
  - Single-line file with no trailing newline
  - Multi-line file with trailing newline
- `line_from_span` returns the expected 1-based line numbers for a set of offsets.

### Integration Validation (Manual or Test Harness)
- Running `glyph check` on a file with a syntax error prints:
  - File path
  - Line number
  - Lowercase severity
  - Human-readable message
- Example acceptance output:
  - `src/main.glyph:3: error: expected \\`fn\\`, \\`struct\\`, ...`

### Regression Checks
- Existing unit tests in the repo still pass.
- `glyph build` and `glyph run` still report diagnostics (now formatted) and return non-zero exit codes on error.
- No panics in the CLI when diagnostics have no span or no module id.

## Risks and Edge Cases
- Diagnostics without a span: must still include file/module and message (no line).
- Diagnostics without module id: fallback to `"<unknown>"`.
- Std modules: may produce diagnostics without user file paths; fallback to module id or `"<unknown>"`.
- Byte offsets in UTF-8: line numbers remain correct, but column numbers (if added later) would need grapheme-aware logic.

## Open Questions (If Needed Later)
- Should we include a short source snippet and caret once line mapping exists?
- Should the LSP JSON output include file and line information?

## Success Criteria (Summary)
- Every CLI-reported diagnostic includes a file or module label.
- Diagnostics with spans show approximate line numbers.
- Output remains stable and easy to scan.
- Changes keep the Rust-level diagnostics north star explicitly in view.

# Map Support Plan

Status key: `[ ]` not started, `[~]` in progress, `[x]` done.

## Scope & Assumptions
- Hash table `Map<K, V>` with separate chaining; no order guarantees.
- Require `Std::Hashing::Hash` for non-atomic keys; equality via `==`.
- Error type `Std::Err { msg: String }`; operations return `Result<T, Err>` as specified in MAP design.
- Average O(1) target; single-threaded only; no serialization yet.

## Epic A — Stdlib Surface & Types
- [x] Define `Std::Err { msg: String }` in std/enums (or std/error) and update `Result<T, Err>` shape.
- [x] Introduce `Std::Hashing::Hash` interface with `hash(&self) -> u64`; add impls for atomic/scalar types.
- [x] Add `std/map` module skeleton and re-export from `std`.
- [x] Document API contract (`add/update/del/get/has/keys/vals`) in std module declarations.

## Epic B — Frontend Parsing, Resolution, Monomorphization
- [x] Ensure parser/type rendering accepts `Map<K, V>` type applications.
- [x] Resolver: validate `Map` arity and enforce `Hash` requirement for `K` (non-atomic keys must implement interface).
- [x] Monomorphization: collect templates for `Map<K, V>`, `Result<T, Err>`, and hashing calls.
- [x] Diagnostics: clear errors for missing `Hash` impls or wrong arity.

## Epic C — MIR & Builtins
- [x] Add MIR constructs/builtins for `Map::new`, `with_capacity`, `add`, `update`, `del`, `get`, `has`, `keys`, `vals`.
- [x] Lower method calls to Map intrinsics (including branching for `Result`/`Option` paths).
- [ ] Model resize/grow paths and hash dispatch in MIR (call out to helpers as needed).

## Epic D — Backend Codegen
- [x] Generate `Map<K, V>` layout (buckets vector + len/cap metadata) per monomorphized type.
- [x] Codegen for `add`/`update`/`del`: hash key, bucket walk, equality check, insert/update/remove with swap-remove.
- [x] Codegen for `get`/`has`: hash + bucket scan; return `Option`/bool.
- [x] Codegen for `keys`/`vals`: allocate `Vec`, clone entries, handle zero buckets.
- [ ] Implement resize: allocate new bucket array, rehash entries, free old buckets.
- [ ] Integrate drop glue: drop keys/values then free buckets.

## Epic E — Runtime/Helpers (if needed)
- [x] Add hashing glue/helper functions if backend cannot inline hashing; ensure consistent hasher use.
- [ ] Provide malloc/free helpers for bucket arrays compatible with existing allocator paths.

## Epic F — Testing & Fixtures
- [ ] Add MIR fixtures for `add/update/del/get/has/keys/vals`, success + error paths.
- [ ] Add codegen/integration tests for collisions, resize, and drop behavior.
- [ ] Add sample program (e2e) using `std/map` (similar to `examples/vector`).

## Epic G — Documentation & Examples
- [ ] Update `MAP.md` if API or constraints change during implementation.
- [ ] Add user-facing doc snippet to `DOCS.md`/guide explaining `Map` usage and hashing requirement.
- [ ] Add example code to `examples` folder (buildable via glyph-cli) demonstrating basic map workflow.

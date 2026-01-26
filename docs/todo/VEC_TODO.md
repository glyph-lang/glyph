# Vec<T> — Implementation TODO (detailed)

Legend: `[ ]` not started, `[~]` in progress, `[x]` done

Scope: follow `docs/design/VEC_DESIGN.md` to deliver `Vec<T>` + generic `Option<T>` + `usize` end-to-end.

## Prereqs / Foundations
- [x] Add builtin `usize` (parse/resolve) mapped to backend `i64` (`glyph-core`, `glyph-frontend`, `glyph-backend`)
- [x] Generalize `Option<T>` to a generic enum (stdlib + monomorphization; retire mono-`i32` form)

## Type System + Templates
- [x] Represent `Vec<T>` as `Type::App { base: "Vec", args: [T] }` (no `Type::Vec`)
- [x] Ensure `type_expr_to_type` accepts `Vec<T>` alongside `Own/Shared/RawPtr`
- [x] Extend template collection to gather `Vec` struct and generic `Option` instantiations

## Struct Definition
- [x] Add `Vec<T>` struct in stdlib with fields `data: RawPtr<T>`, `len: usize`, `cap: usize`
- [ ] Enforce invariants: `len <= cap`, `data` null iff `cap == 0` (runtime checks/traps where applicable)

## MIR Intrinsics / Lowering
- [~] Builtins wired: `Vec::new`, `Vec::with_capacity`, `Vec::push`, `Vec::pop`, `Vec::len`, indexing `vec[idx]` → new Vec rvalues
  - [ ] Allow type inference for `Vec::new/with_capacity` via annotations or context (currently error without annotation)
  - [~] Indexing: uses `VecIndex` with bounds_check flag
  - [~] `push`: rvalue created with inferred elem type from receiver
  - [~] `pop`: returns `Option<T>` rvalue
  - [x] `len`: lowers to `VecLen` and returns `usize`
- [ ] Drop: MIR lowering ensures element drops before buffer free (align with backend once implemented)

## Backend Codegen (Stage v3)
- [ ] Add LLVM layout for monomorphized `Vec$T` `[ptr, len, cap]` using `i64` for `usize`
- [ ] Implement `VecNew`/`VecWithCapacity`: allocate cap=0 as null; cap>0 alloc via malloc; init len=0
- [ ] Implement `VecLen`: load len field
- [ ] Implement `VecIndex`: bounds check (`idx < len`), trap on OOB (reuse array trap), compute element ptr, load value (respect elem type)
- [ ] Implement `VecPush`: check len==cap → grow (cap'=max(1, cap*2)), malloc new buffer, move elements, free old; write element at old len; len++
- [ ] Implement `VecPop`: if len==0 return Option::None; else len--, load element, wrap in Option::Some; leave cap unchanged
- [ ] Implement drop for `Vec<T>`: iterate 0..len dropping elements (call drop for Own/Shared/String/etc.), then free buffer if cap>0
- [ ] Plumb destination typing if needed (mutating rvalues) or special-case Vec in `codegen_inst` to update struct fields correctly
- [ ] Reuse existing malloc/free helpers; consider memmove for growth (element-wise for drop correctness?)

## Tests / Fixtures (Stage v4)
- [ ] Runtime fixture: push→grow→len with primitive element (ensures geometric growth and len correctness)
- [ ] Runtime fixture: pop from empty returns Option::None; pop after pushes returns Some(last)
- [ ] Bounds-trap fixture: `vec[idx]` with OOB index traps (mir/ir snapshot)
- [ ] Drop tests: Vec<Own<T>>, Vec<Shared<T>>, Vec<String> to ensure element drop paths run and buffer freed
- [ ] usize coverage: parsing/sizing plus Vec len/cap operations use `usize`
- [ ] MIR/codegen snapshots: Vec push/pop/len/index IR, Option<T> from pop, growth path, drop path

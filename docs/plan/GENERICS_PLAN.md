# Generics Plan (v0.1)

This plan operationalizes `docs/design/GENERICS.md` against the current compiler architecture.

## Current Baseline (what exists today)

- Type annotations are stored as `ast::Ident` strings (e.g. `"i32"`, `"Own<i32>"`, `"[i32; 4]"`).
- Type parsing currently happens in the frontend via string parsing (see `resolve_type_name` in MIR lowering).
- The parser supports limited single-argument generics only for `Own<T>`, `RawPtr<T>`, `Shared<T>` in type annotations.
- Structs/enums are non-generic in AST and type layout.
- Resolver tracks `struct_types`/`enum_types` by name.
- MIR + LLVM backend require concrete layouts (no polymorphic layout).

Implication: v0.1 generics must be monomorphized before (or during) MIR/LLVM so that backend remains concrete.

## v0.1 Scope

**In scope**
- Generic type application in type positions: `Vec<i32>`, `Result<str, IoErr>`, nested: `Map<str, Vec<i32>>`.
- Generic type definitions for `struct` and `enum`: `struct Vec<T> { ... }`, `enum Option<T> { None, Some(T) }`.
- Generic impl blocks and methods for generic types.
- Monomorphization only (no runtime polymorphism).
- Minimal, local inference for generic parameters (bidirectional typing, no HM).

**Explicitly deferred**
- Interface bounds (`T: Trait`, `where` clauses).
- HKTs, specialization, default type params, GATs.
- Fully generic `Map<K,V>` unless we also decide on key constraints.

## Big Design Choice (must decide early)

We currently treat types as strings in the AST and parse them later. For generics we have two viable implementation strategies:

1) **Structured type AST (recommended)**
   - Introduce an `ast::TypeExpr` and migrate `Param.ty`, `FieldDef.ty`, return types, etc.
   - Pros: cleaner parsing + better diagnostics + easier substitution + future bounds.
   - Cons: large refactor touching parser/resolver/MIR lowering.

2) **Keep string types (incremental)**
   - Keep `ast::Ident` for type annotations but extend the string parser to understand `Foo<A,B>`.
   - Add a generic template registry keyed by base name (`Foo`) and use a canonical string for instantiated names (`Foo<i32>`).
   - Pros: smaller AST change.
   - Cons: substitution/mangling/diagnostics become string-heavy and more fragile.

This plan proceeds with (1) but can be adapted to (2). If we choose (2), the “TypeExpr” steps become “extend type-string parser + canonicalizer”.

---

# Phase 0 — Parser correctness prerequisites

Goal: remove existing parsing ambiguities that will also affect `<...>` generics.

- [ ] Fix control-flow condition parsing to suppress struct-literal parsing when the next token is a `{` block.
  - Rationale: today `while i < n { ... }` mis-parses `n {` as a struct literal.
  - Apply same suppression used in `match` scrutinee parsing.
  - Add targeted parser tests for `if`, `while`, `for` conditions ending in identifiers.
- [ ] Decide/encode precedence rules for `<` as comparison vs `Foo<...>` as type application.
  - In expressions: `<` is binary op.
  - In type positions: `<...>` belongs to type application.
  - Ensure lexer continues emitting `Lt`/`Gt` separately; parser must not rely on a `>>` token.

Deliverable: green tests, no regressions.

---

# Phase 1 — Introduce a structured type representation

Goal: represent generic applications and type parameters explicitly.

## 1.1 Add `TypeExpr` to `glyph-core`

- [ ] Add `ast::TypeExpr` (or `ast::Ty`) with spans.
  - Minimal shape:
    - `TypeExpr::Path { segments: Vec<String>, span }` (handles `std::Vec`)
    - `TypeExpr::App { base: Box<TypeExpr>, args: Vec<TypeExpr>, span }` (handles `Vec<i32>`)
    - `TypeExpr::Ref { mutability, inner, span }` (handles `&T`, `&mut T`)
    - `TypeExpr::Array { elem, size, span }` (handles `[T; N]`)
  - Optional early:
    - `TypeExpr::Param { name, span }` (or represent params as `Path` and resolve in scope)

## 1.2 Migrate AST nodes that store types

- [ ] Replace `Option<Ident>` type fields with `Option<TypeExpr>`:
  - `ast::Param.ty`
  - `ast::Function.ret_type`
  - `ast::FieldDef.ty`
  - `ast::EnumVariantDef.payload`
  - Interface method signatures
  - Extern function params/return type (keep restricted grammar and validation)

## 1.3 Parser support for type expressions

- [ ] Implement `parse_type_expr` with full generic application support.
  - Handle:
    - bare paths: `i32`, `Point`, `std::Vec`
    - `&` / `&mut` prefixes
    - `[T; N]` arrays (existing concept)
    - `Foo<...>` application with comma-separated args
    - nested `Foo<Bar<Baz<i32>>>` without relying on `>>` tokenization
- [ ] Ensure `parse_type_expr` has good error recovery and spans.
- [ ] Keep formatter constraints in mind: canonical output should avoid spaces in `Foo<Bar<i32>>`.

Deliverable: parsing + AST serialization for the new type nodes.

---

# Phase 2 — Add generic parameters to item definitions

Goal: allow declaring generic types and (restricted) generic functions.

## 2.1 AST changes

- [ ] Add `generic_params: Vec<Ident>` to:
  - `StructDef`
  - `EnumDef`
  - (Optionally later) `InterfaceDef`
- [ ] Add generic params for functions where allowed:
  - For v0.1: allow on methods of generic types and a small set of stdlib helpers.
  - Represent as `generic_params: Vec<Ident>` on `Function`.
- [ ] Extend `ImplBlock` target to include type application, e.g. `impl Vec<T> { ... }`.
  - With `TypeExpr` this becomes `target: TypeExpr` rather than `Ident`.

## 2.2 Parser support

- [ ] Parse generic param lists after `struct` / `enum` / `fn` names:
  - `struct Vec<T> { ... }`
  - `enum Option<T> { ... }`
  - `fn swap<T>(a: &mut T, b: &mut T)` (if enabled)
- [ ] Validate param naming rules (convention only initially) and uniqueness within the param list.
- [ ] Add parser tests:
  - single param, multi param, nested uses in fields/variants
  - empty `<>` rejected
  - trailing comma rejected or allowed explicitly (pick a rule and test it)

Deliverable: AST can express generic declarations.

---

# Phase 3 — Resolver: generic scopes and type resolution

Goal: resolve type expressions into `glyph_core::types::Type` extended for generics.

## 3.1 Extend `glyph_core::types::Type`

- [ ] Add support for type parameters and generic application:
  - `Type::Param(String)`
  - `Type::App { base: String, args: Vec<Type> }` OR `Type::Named(String)` with canonicalized `Name<...>` (but prefer structured).
  - Decide how to represent module-qualified names (`std::Vec`) so mangling remains stable.

## 3.2 Track generic templates

- [ ] Split “template definition” from “instantiated type” in resolver context:
  - e.g. `struct_templates: HashMap<String, StructTemplate>`
  - where `StructTemplate` stores:
    - base name
    - param names
    - field types (as `TypeExpr` or unresolved `Type` with params)
  - same for enums.

## 3.3 Generic scope rules

- [ ] Implement a generic scope stack during resolution:
  - When resolving inside `struct Vec<T>` body, `T` is in scope.
  - When resolving inside `impl Vec<T> { ... }`, `T` is in scope and tied to the impl target.
  - When resolving inside `fn foo<T>(...)`, `T` is in scope for that function only.
- [ ] Diagnostics:
  - E-GEN-002 wrong arity in `Foo<...>`
  - E-GEN-003 unknown type parameter
  - E-GEN-004 instantiation depth limit

## 3.4 Extern function subset compatibility

- [ ] Decide whether extern signatures accept generic types (likely **no** in v0.1).
- [ ] Keep `resolve_ffi_type` narrow: primitives, `String`, `&str`, `RawPtr<T>` for FFI only.

Deliverable: resolver can produce parametric templates and resolve type expressions with parameters.

---

# Phase 4 — Type checking and minimal inference

Goal: infer concrete type arguments at call sites when unambiguous.

## 4.1 Bidirectional unification engine

- [ ] Implement local type variable inference for generic params:
  - Inputs:
    - expected type (from annotation / context)
    - argument types
    - callee signature with `Type::Param` placeholders
  - Output:
    - substitution map `T -> concrete Type`
  - Constraints:
    - occurs check (prevent `T = Vec<T>` blowups)
    - depth limit

## 4.2 Inference sources (v0.1)

- [ ] From explicit type annotation:
  - `let v: Vec<i32> = Vec.new();` gives `T = i32`.
- [ ] From constructor/variant arguments:
  - `Option.Some(3)` gives `T = i32`.
  - `Result.Ok("hi")` gives `T = str`, but `E` remains unknown → require context.
- [ ] From method receiver type:
  - if `v: Vec<i32>`, then `v.push(x)` requires `x: i32`.

## 4.3 When inference must fail

- [ ] Emit E-GEN-001 when a generic parameter remains unconstrained.
- [ ] Decide numeric literal defaulting policy (as per design Q1).

Deliverable: stable, predictable inference with high-quality diagnostics.

---

# Phase 5 — Monomorphization and symbol mangling

Goal: generate concrete functions/types for all used instantiations.

## 5.1 Choose monomorphization insertion point

- Option A: monomorphize on typed AST/HIR before MIR lowering.
- Option B: lower to a polymorphic MIR (with `Type::Param`) then monomorphize MIR.

Recommendation: **B** if MIR already has enough type info; otherwise **A**.

## 5.2 Instantiation collection

- [ ] Walk typed program to collect all concrete instantiations:
  - generic struct/enum instantiations appearing in variable types, field types, return types
  - method/function instantiations inferred from calls
- [ ] Deduplicate instantiations by canonical key:
  - `Vec<i32>` is the same everywhere
  - canonicalize nested types
- [ ] Enforce recursion depth limit (E-GEN-004).

## 5.3 Specialization

- [ ] For each instantiation:
  - produce a concrete struct layout (substitute params in fields)
  - produce a concrete enum layout (substitute params in payloads)
  - generate specialized methods/functions with substituted param types

## 5.4 Mangling

- [ ] Implement a stable mangling scheme that includes:
  - module path
  - base type/function name
  - concrete type args
  - method name
- [ ] Ensure mangling is used consistently by:
  - resolver symbol keys
  - MIR function names
  - backend emission/linking

Deliverable: LLVM sees only monomorphic symbols.

---

# Phase 6 — Backend compatibility

Goal: ensure LLVM backend works with instantiated types.

- [ ] Ensure `MirModule.struct_types` / `enum_types` contain only concrete (fully substituted) layouts.
- [ ] Ensure `Type::App` / `Type::Param` never reach the backend.
- [ ] Validate that codegen for enums/structs works for the new specialized names.

Deliverable: codegen remains unchanged or minimally changed.

---

# Phase 7 — Stdlib bootstrap with generics

Goal: make generics useful immediately.

- [ ] Implement `Option<T>` in std modules.
  - Constructors: `Option.Some(x)` and `Option.None` patterns as supported.
  - Minimal helpers: `is_some`, `unwrap_or` (avoid panics if possible).
- [ ] Implement `Result<T,E>`.
  - Minimal helpers: `is_ok`, `unwrap`, `unwrap_err`.
- [ ] `Vec<T>` (only after drop story is clear):
  - prefer explicit `free()` until drop glue exists.
  - initial operations: `new`, `push`, `len`, `cap`.

Notes:
- `Map<K,V>` should likely be delayed until bounds/hashing story exists, or implemented as `MapStr<V>`/`MapI32<V>`.

---

# Phase 8 — Tests, fixtures, and docs

Goal: lock in behavior, prevent regressions.

- [ ] Parser fixtures:
  - type application parsing (arity, nesting)
  - generic struct/enum declarations
  - impl blocks on generic targets
- [ ] Resolver/typecheck unit tests:
  - wrong arity
  - unknown type param
  - ambiguous inference (E-GEN-001)
  - depth limit
- [ ] MIR snapshots:
  - verify specialized symbol names
  - verify concrete layouts after substitution
- [ ] Codegen tests:
  - Option<i32> match-like access
  - Vec<i32> push/len basic
- [ ] Documentation updates:
  - add a “Generics” chapter in the mdbook (once bootstrapped)
  - document inference rules and when annotations are required

---

# Suggested Milestones / PR Breakdown

- **PR1**: parser suppression fix + tests (struct-lit ambiguity in control flow).
- **PR2**: `TypeExpr` added + migrate AST type fields + parser support.
- **PR3**: generic params on structs/enums/impls + parser tests.
- **PR4**: resolver generic scopes + `Type::Param`/`Type::App` + diagnostics.
- **PR5**: local inference/unification for constructors + method calls.
- **PR6**: monomorphization + mangling + MIR/codegen integration.
- **PR7**: std Option/Result generic definitions + end-to-end examples.

# Open Questions (to resolve before Phase 4)

- Numeric literal default type (i32 default vs required suffixes).
- Whether to allow generic free functions broadly in v0.1.
- Where to monomorphize (typed AST vs MIR).
- Whether to support `type` aliases (e.g. `type Option<T> = enum { ... }`) or only `enum Option<T>` for now.

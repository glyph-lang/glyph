# Next Session Quick Start Guide

**Last Updated:** January 8, 2026
**Commit:** c608508 (feat: implement glyph compiler with LLVM backend and struct parsing)

---

## Current Status

### ✅ What's Working

**Compiler Features:**
- Complete lexer and parser for functions, expressions, control flow
- Return types and function parameters
- Implicit returns (Rust-style)
- MIR (Mid-level IR) with SSA-like representation
- LLVM codegen with JIT execution
- Struct parsing (definitions, literals, field access)

**Test Coverage:**
- 28 tests passing
- 12 MIR snapshot tests
- 5 AST unit tests
- 4 backend tests with JIT
- 3 parser fixtures for structs

**Example Code That Works:**
```glyph
// Functions with implicit returns
fn add(a: i32, b: i32) -> i32 {
  a + b
}

// Control flow
fn abs(x: i32) -> i32 {
  if x < 0 { -x } else { x }
}

// Struct parsing (AST only - not compiled yet)
struct Point {
  x: i32
  y: i32
}

fn main() {
  let p = Point { x: 10, y: 20 }
  p.x + p.y
}
```

---

## What to Build Next

### Phase 3: Type Resolution (~2-3 hours)

**Goal:** Build struct type registry and resolver

**Files to Create:**
- `crates/glyph-frontend/src/resolver.rs`

**What to Implement:**
1. `StructType` definition with field metadata
2. `ResolverContext` with `HashMap<String, StructType>`
3. `resolve_types()` function:
   - First pass: collect all struct definitions
   - Build field type map
   - Validate field types exist

**Test Strategy:**
- Unit tests for type resolution
- Test undefined struct references
- Test circular dependencies (future)

**Integration Point:**
- Update `lower_module()` in `mir_lower.rs` to call resolver first
- Pass `struct_types` registry through MIR lowering

---

### Phase 4: MIR Extension (~3-4 hours)

**Goal:** Represent struct operations in MIR

**Files to Modify:**
- `crates/glyph-core/src/lib.rs` (mir module)
- `crates/glyph-frontend/src/mir_lower.rs`

**What to Add:**

1. **Extend Rvalue enum:**
```rust
pub enum Rvalue {
    // ... existing variants ...
    StructLit {
        struct_name: String,
        field_values: Vec<(String, MirValue)>,
    },
    FieldAccess {
        base: LocalId,
        field_name: String,
        field_index: u32,
    },
}
```

2. **Add to MirModule:**
```rust
pub struct MirModule {
    pub struct_types: HashMap<String, StructType>,  // NEW
    pub functions: Vec<MirFunction>,
}
```

3. **Lower struct expressions:**
- `Expr::StructLit` → `Rvalue::StructLit`
- `Expr::FieldAccess` → `Rvalue::FieldAccess`

**Test Files to Create:**
- `tests/fixtures/mir/struct_lit.glyph`
- `tests/fixtures/mir/field_access.glyph`
- `tests/fixtures/mir/struct_return.glyph`

---

### Phase 5: LLVM Codegen (~4-5 hours)

**Goal:** Generate LLVM IR for struct operations

**Files to Modify:**
- `crates/glyph-backend/src/codegen.rs`

**What to Implement:**

1. **Struct type registration:**
```rust
struct_types: HashMap<String, LLVMTypeRef>  // cache

fn register_struct_types(&mut self, mir_module: &MirModule) {
    // Use LLVMStructCreateNamed + LLVMStructSetBody
}
```

2. **Codegen struct literals:**
- Use `LLVMBuildAlloca()` for stack allocation
- Use `LLVMBuildGEP2()` to get field pointers
- Store each field value
- Load complete struct (for copy semantics)

3. **Codegen field access:**
- GEP to field: `getelementptr inbounds %Point, ptr %p, i32 0, i32 N`
- Load field value

**Test Files:**
- `tests/fixtures/codegen/struct_basic.glyph`
- `tests/fixtures/codegen/struct_arithmetic.glyph`

---

## Environment Setup

```bash
# Set LLVM path
export LLVM_SYS_201_PREFIX=/opt/homebrew/Cellar/llvm/20.1.8

# Build
cargo build --release

# Run all tests
cargo test --workspace

# Test specific package
cargo test --package glyph-frontend

# Update snapshots
INSTA_UPDATE=always cargo test

# Test CLI
./target/release/glyph-cli check file.glyph
./target/release/glyph-cli build file.glyph
```

---

## Key Design Decisions

✅ **Stack allocation only** - No heap, no Box[T] in Phase 1  
✅ **Copy semantics** - Structs copied by value like C  
✅ **Type::Named exists** - Already in type system  
✅ **All tokens exist** - No lexer changes needed  

❌ **Not implementing yet:**
- References (`&Point`)
- Heap allocation (`Box<Point>`)
- Methods/impl blocks
- Generic structs

---

## Files Organization

```
crates/
├── glyph-core/src/lib.rs         # AST, MIR, Types
├── glyph-frontend/src/
│   ├── lexer.rs                  # ✅ Complete
│   ├── parser.rs                 # ✅ Struct parsing done
│   ├── resolver.rs               # 🔜 TODO: Create this
│   ├── mir_lower.rs              # 🔜 TODO: Add struct lowering
│   └── lib.rs                    # 🔜 TODO: Wire resolver
└── glyph-backend/src/
    ├── codegen.rs                # 🔜 TODO: Add struct codegen
    └── lib.rs                    # ✅ Complete

tests/fixtures/
├── parse/                        # ✅ 3 struct fixtures
├── mir/                          # 🔜 Add struct fixtures
└── codegen/                      # 🔜 Add struct fixtures
```

---

## Quick Commands

```bash
# Check what's changed
git status
git diff

# Run demo
./demo.sh

# Test struct parsing
./target/release/glyph-cli check tests/fixtures/parse/struct_def.glyph

# Run specific test
cargo test struct_def

# See test output
cargo test -- --nocapture
```

---

## Success Criteria for Next Session

**Minimum:**
- [ ] Resolver created and working
- [ ] MIR extended with struct operations
- [ ] Can lower struct literals to MIR
- [ ] MIR snapshot tests pass

**Full Success:**
- [ ] LLVM codegen for structs working
- [ ] Can JIT execute struct operations
- [ ] End-to-end test: create struct, access fields, return sum
- [ ] All tests passing

---

## Estimated Time Remaining

- Phase 3: 2-3 hours
- Phase 4: 3-4 hours  
- Phase 5: 4-5 hours
- Phase 6: 2-3 hours

**Total: 11-15 hours (2-3 focused work sessions)**

---

## Questions to Consider

1. Should we add proper type checking or keep it minimal?
2. Do we need field index caching or compute on-the-fly?
3. Should we support struct returns by-value in Phase 5?
4. Do we need better error messages for struct errors?

---

Ready to continue! Start with Phase 3 (Type Resolution) when you begin the next session.

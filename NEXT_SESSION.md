# Next Session Quick Start Guide

**Last Updated:** January 8, 2026
**Commit:** 368dd20 (feat: complete struct support with type resolution, MIR lowering, and LLVM codegen)

---

## Current Status

### ✅ What's Working

**Compiler Features:**
- Complete lexer and parser for functions, expressions, control flow
- Return types and function parameters
- Implicit returns (Rust-style)
- MIR (Mid-level IR) with SSA-like representation
- LLVM codegen with JIT execution
- **COMPLETE STRUCT SUPPORT:**
  - Struct definitions
  - Struct literals
  - Field access
  - Type resolution with validation
  - MIR lowering
  - LLVM codegen

**Test Coverage:**
- 30+ tests passing
- 15 MIR snapshot tests
- 7 backend codegen tests
- 6 resolver unit tests
- 5 AST unit tests

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

// Structs - FULLY WORKING!
struct Point {
  x: i32
  y: i32
}

fn main() -> i32 {
  let p = Point { x: 10, y: 20 }
  ret p.x + p.y  // compiles and generates correct LLVM IR!
}
```

---

## Struct Support ✅ COMPLETE

All struct implementation phases are now complete and committed:

- ✅ Phase 1-2: AST + Parser (struct definitions, literals, field access)
- ✅ Phase 3: Type Resolution (resolver.rs with validation)
- ✅ Phase 4: MIR Extension (StructLit, FieldAccess rvalues)
- ✅ Phase 5: LLVM Codegen (struct registration, GEP instructions)
- ✅ Phase 6: Integration (full pipeline working, 20+ tests passing)

## What to Build Next: Function Calls

**Status:** MIR placeholder exists, codegen partially implemented, needs completion

**Current Limitations:**
- Parser supports function call syntax: `foo(1, 2, 3)`
- MIR has `Rvalue::Call { name, args }` variant
- LLVM codegen has function declaration support
- **Missing:** Call expression lowering and proper codegen

### Phase 1: Research & Design (~2 hours)

**Goal:** Understand current state and design implementation

**Tasks:**
1. Examine `Expr::Call` in AST (likely exists)
2. Check if `lower_call()` exists in mir_lower.rs
3. Review LLVM `LLVMBuildCall` usage in codegen.rs
4. Design function signature registry
5. Plan parameter/return value handling

### Phase 2: MIR Lowering (~2-3 hours)

**Goal:** Lower function calls to MIR

**Files to Modify:**
- `crates/glyph-frontend/src/mir_lower.rs`

**What to Implement:**
1. `lower_call()` function (if not exists)
   - Evaluate argument expressions
   - Look up function signature
   - Create Call rvalue
   - Handle return value

2. Add function registry to LowerContext
   - Map function names to signatures
   - Validate argument count/types

**Test Fixtures:**
- `tests/fixtures/mir/call_basic.glyph` - Simple call
- `tests/fixtures/mir/call_with_return.glyph` - Use return value
- `tests/fixtures/mir/call_multiple_args.glyph` - Multi-arg

### Phase 3: LLVM Codegen (~2-3 hours)

**Goal:** Generate LLVM call instructions

**Files to Modify:**
- `crates/glyph-backend/src/codegen.rs`

**What to Implement:**
1. Codegen for `Rvalue::Call`
   - Look up function declaration
   - Build argument array
   - Call `LLVMBuildCall`
   - Handle return value (store to local)

2. Test edge cases:
   - Zero arguments
   - Void returns
   - Struct parameters/returns

**Test Fixtures:**
- `tests/fixtures/codegen/call_basic.glyph` - LLVM IR test
- `tests/fixtures/codegen/call_recursive.glyph` - Recursion test
- `tests/fixtures/codegen/factorial.glyph` - End-to-end test

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

## Success Criteria for Function Calls

**Phase 1 Success (Research & Design):**
- [ ] Understand current Call implementation
- [ ] Design function signature registry
- [ ] Create test specifications
- [ ] Plan MIR lowering strategy
- [ ] Plan LLVM codegen strategy

**Phase 2 Success (MIR Lowering):**
- [ ] lower_call() function implemented
- [ ] Function registry in LowerContext
- [ ] MIR snapshot tests passing
- [ ] Can lower calls with multiple arguments

**Phase 3 Success (LLVM Codegen):**
- [ ] Rvalue::Call codegen working
- [ ] LLVMBuildCall generates correct IR
- [ ] Can compile and execute function calls
- [ ] Recursion works (factorial, fibonacci)

---

## Estimated Time for Function Calls

- Phase 1 (Research & Design): 2 hours
- Phase 2 (MIR Lowering): 2-3 hours
- Phase 3 (LLVM Codegen): 2-3 hours
- Testing & Integration: 1-2 hours

**Total: 7-10 hours (1-2 focused work sessions)**

---

## Questions to Consider

1. Should we add proper type checking or keep it minimal?
2. Do we need field index caching or compute on-the-fly?
3. Should we support struct returns by-value in Phase 5?
4. Do we need better error messages for struct errors?

---

Ready to continue! Start with Phase 1 (Research & Design) for function calls when you begin the next session.

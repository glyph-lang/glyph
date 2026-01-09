# Next Session Quick Start Guide

**Last Updated:** January 9, 2026
**Commit:** 12901e6 (feat: complete function call implementation with LLVM codegen)

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
  - Struct definitions, literals, field access
  - Type resolution with validation
  - Return by value
- **COMPLETE FUNCTION CALLS:**
  - Direct function calls
  - Recursive functions
  - Forward references
  - Struct parameters and returns

**Test Coverage:**
- 35+ tests passing
- 17 MIR snapshot tests
- 10 backend codegen tests
- 6 resolver unit tests
- 5 AST unit tests

**Example Code That Works:**
```glyph
// Structs with functions
struct Point {
  x: i32
  y: i32
}

fn make_point(x: i32, y: i32) -> Point {
  Point { x: x, y: y }  // implicit return
}

// Recursive functions
fn factorial(n: i32) -> i32 {
  if n <= 1 {
    ret 1
  } else {
    ret n * factorial(n - 1)
  }
}

fn main() -> i32 {
  let pt = make_point(10, 20)
  let fact = factorial(5)
  ret pt.x + pt.y + fact  // returns 150 (30 + 120)
}
```

---

## Struct Support ✅ COMPLETE

All struct implementation phases complete:
- ✅ AST + Parser + Type Resolution + MIR + LLVM Codegen
- ✅ 20+ tests passing

## Function Calls ✅ COMPLETE

All function call implementation complete:
- ✅ MIR lowering with validation (argument count checked)
- ✅ LLVM codegen for direct calls
- ✅ Recursive functions working (factorial tested)
- ✅ Forward references supported (any order)
- ✅ Struct parameters and return values
- ✅ 5 integration tests passing

## What to Build Next

Choose the next major feature:

### Option 1: Loops (Recommended - High Value)
**Complexity:** Medium
**Value:** Essential for practical programs

**Features:**
- `while` loops with condition
- `for` loops over ranges
- `break` and `continue` statements
- Loop expressions (can return values)

**Implementation:**
- Add loop AST nodes
- MIR loop blocks with phi nodes
- LLVM loop codegen with proper branching

**Test Strategy:**
- Count to N loop
- Fibonacci via iteration
- Early break/continue

### Option 2: Arrays and Slices
**Complexity:** Medium-High
**Value:** Complement structs well

**Features:**
- Fixed-size arrays: `[i32; 10]`
- Array literals: `[1, 2, 3, 4, 5]`
- Indexing: `arr[i]`
- Slices: `&[T]`

**Implementation:**
- Array types in type system
- Bounds checking (optional/runtime)
- LLVM array/GEP operations

### Option 3: Enums + Pattern Matching
**Complexity:** High
**Value:** Powerful language feature

**Features:**
- Enum definitions with payloads
- Match expressions (exhaustive)
- Option[T] and Result[T, E]

**Implementation:**
- Tagged unions in LLVM
- Pattern matching compilation
- Exhaustiveness checking

### Option 4: Better Type System
**Complexity:** High
**Value:** Improves correctness

**Features:**
- Full type inference (Hindley-Milner)
- Proper type checking in MIR lowering
- Better error messages

**Implementation:**
- Type inference engine
- Constraint solving
- Type unification

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

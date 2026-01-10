# Next Session Quick Start Guide

**Last Updated:** January 10, 2026
**Current:** Interfaces Phase 6 – verify LLVM/codegen + docs/tests

---

## 🎯 IMMEDIATE NEXT STEPS

1) **Phase 6: Interfaces LLVM verification**
   - Confirm interface method calls emit correct LLVM (reuse call paths) and expand CLI/codegen fixtures if needed.
2) **Phase 7: Interfaces docs & tests**
   - Add examples covering interface parsing, impl validation errors, and mixed inherent/interface dispatch.
3) **Stretch:** start Heap Ownership pointers (Own/RawPtr) once interface docs/tests land.

**Previous Status:** Arrays + loops integrated; function calls and references fully green.

---

## Current Status

### ✅ What's Working

**Compiler Features:**
- Complete lexer/parser/resolver + MIR lowering
- LLVM codegen with JIT execution
- Structs, function calls, references, loops
- Arrays: literals, indexing with bounds checks, `.len()`
- Interfaces: parsing/impl validation, method-call resolution (inherent + interface dispatch), auto-borrowing in MIR

**Test Coverage:**
- Workspace tests green (`cargo test`)
- 34 MIR snapshots refreshed (arrays, refs, loops, owns, interface call)
- Backend codegen fixtures (arrays/structs/owns) passing
- Resolver/parser/unit coverage intact

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

// While loops with break
fn find_value(target: i32) -> i32 {
  let i = 0
  while true {
    if i == target {
      break
    }
    i = i + 1
  }
  ret i
}

// For loops (NEW!)
fn sum_range(n: i32) -> i32 {
  let sum = 0
  for i in 0..n {
    sum = sum + i
  }
  ret sum  // returns sum of 0..n-1
}

fn main() -> i32 {
  let pt = make_point(10, 20)
  let fact = factorial(5)
  let found = find_value(7)
  let range_sum = sum_range(10)
  ret pt.x + pt.y + fact + found + range_sum  // returns 202 (30 + 120 + 7 + 45)
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

## Reference Support 🔧 (Phases 4–6 done)

Latest progress on the pointer roadmap:
- ✅ **Phase 4** – resolver understands `&T` / `&mut T` names and reports malformed references.
- ✅ **Phase 5** – MIR gained `Rvalue::Ref`, assignment lowering, and auto-deref logic for field access.
- ✅ **Phase 6** – LLVM codegen now emits raw pointer types for references, keeps locals/params in stack slots, and auto-derefs before field GEPs.
- 🧪 Added MIR fixtures `ref_field_access.glyph` and `ref_param_call.glyph` to exercise reference semantics.
- 📦 `cargo test -p glyph-frontend` fully green (19 MIR snapshots).

## For Loop Support ✅ COMPLETE

For loops with range syntax are now fully implemented:

**Features:**
- ✅ Syntax: `for var in start..end { body }`
- ✅ Added `In` and `DotDot` tokens to lexer
- ✅ Added `For` variant to `Expr` enum with var, start, end, body
- ✅ Parser implementation following `parse_while()` pattern
- ✅ MIR lowering desugars for loops to while loops
- ✅ Proper variable initialization and increment
- ✅ Loop context tracking (break/continue work in for loops)
- ✅ 2 test fixtures with snapshots

**Implementation Details:**
- For loops desugar at MIR lowering time into equivalent while loops
- Loop variable initialized before entering loop
- Condition checked at loop header: `var < end`
- Body executed, then variable incremented by 1
- Back edge to header for next iteration

**Example:**
```glyph
fn sum_to_n(n: i32) -> i32 {
  let sum = 0
  for i in 0..n {
    sum = sum + i
  }
  ret sum
}
```

**Desugars to:**
```glyph
fn sum_to_n(n: i32) -> i32 {
  let sum = 0
  let i = 0
  while i < n {
    sum = sum + i
    i = i + 1
  }
  ret sum
}
```

## What to Build Next

1) Finish interface track (LLVM validation + docs/tests)
2) Move to Heap Ownership pointers (Own<T>, RawPtr<T>) once interfaces close
3) Longer-term: enums/pattern matching, borrow checker, generics
- Parser for array syntax
- MIR lowering for arrays and indexing
- LLVM array/GEP operations
- Optional bounds checking

**Test Strategy:**
- Array initialization
- Array indexing
- Passing arrays to functions
- Iterating with for loops

### Option 2: Enums + Pattern Matching
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
│   ├── parser.rs                 # ✅ Struct + reference parsing
│   ├── resolver.rs               # ✅ Struct + reference resolver
│   ├── mir_lower.rs              # ✅ Struct + reference MIR lowering
│   └── lib.rs                    # ✅ Frontend pipeline wiring
└── glyph-backend/src/
    ├── codegen.rs                # ✅ Struct + reference codegen
    └── lib.rs                    # ✅ Complete

tests/fixtures/
├── parse/                        # ✅ 3 struct fixtures
├── mir/                          # ✅ 19 MIR fixtures (structs, calls, references)
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

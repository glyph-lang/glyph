# Next Session Quick Start Guide

**Last Updated:** January 10, 2026
**Current:** Shared<T> Reference Counting COMPLETE ✅ – All Ownership Pointers Implemented

---

## 🎯 IMMEDIATE NEXT STEPS

1) **Consider Next Major Feature:**
   - Enums + pattern matching (high value, medium-high complexity)
   - Better type inference (Hindley-Milner, high complexity)
   - Standard library development (collections, strings, I/O)
   - Borrow checker & lifetimes (very high complexity)

**Previous Status:** Shared<T> implementation complete with refcounting, clone semantics, and comprehensive testing (110+ tests passing).

---

## Current Status

### ✅ What's Working

**Compiler Features:**
- Complete lexer/parser/resolver + MIR lowering
- LLVM codegen with JIT execution
- Structs, function calls, references, loops
- Arrays: literals, indexing with bounds checks, `.len()`
- Ownership pointers: Own<T> (exclusive), Shared<T> (refcounted), RawPtr<T> (unsafe)
- Interfaces: parsing/impl validation, method-call resolution, auto-borrowing, LLVM codegen verified, full documentation

**Test Coverage:**
- Workspace tests green (`cargo test`) - 110+ tests passing
- 38 MIR snapshots (arrays, refs, loops, owns, shared, interface calls)
- 6 codegen tests verifying LLVM IR (interfaces, shared, arrays, structs, owns)
- Backend codegen fixtures (arrays/structs/owns/shared) passing
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

// Reference-counted pointers (NEW!)
fn use_shared() -> i32 {
  let s1 = Shared::new(42)
  let s2 = s1.clone()  // refcount = 2
  let s3 = s2.clone()  // refcount = 3
  ret 0  // drops s3, s2, s1 in order, last one frees
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

## Shared<T> Reference Counting ✅ COMPLETE

**Status:** Fully implemented and tested

**Completed Work:**
- [x] Type system: Type::Shared(Box<Type>) variant
- [x] Parser: Shared<T> syntax recognition
- [x] MIR lowering: SharedNew and SharedClone rvalues
- [x] Copy semantics: No move errors (unlike Own<T>)
- [x] LLVM codegen: Memory layout [refcount: usize, data: T]
- [x] Drop glue: Decrement refcount, free when zero
- [x] 4 MIR snapshot tests + 1 codegen test

**Example Working Code:**
```glyph
fn main() -> i32 {
  let s1 = Shared::new(42)
  let s2 = s1.clone()  // refcount = 2
  let s3 = s2.clone()  // refcount = 3
  ret 0
  // s3, s2, s1 drop in order, last one frees memory
}
```

**See**: `SHARED_DESIGN.md` for full documentation

## What to Build Next

1) **Enums + pattern matching** - HIGH VALUE
   - Algebraic data types with payload support
   - Match expressions with exhaustiveness checking
   - Option<T> and Result<T, E> types
2) **Standard library development**
   - Vec<T>, String, HashMap collections
   - I/O primitives
   - Standard prelude
3) Borrow checker & lifetimes
4) Generics + trait system

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

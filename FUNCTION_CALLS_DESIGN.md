# Function Call Implementation Design

**Date:** January 8, 2026
**Status:** Design Phase - Implementation ~90% Complete, Needs Testing & Debugging

---

## Executive Summary

**Key Finding:** Function call implementation is **largely complete** but has execution issues:
- ✅ Parser supports `Expr::Call` syntax
- ✅ MIR lowering is fully implemented (`lower_call()`)
- ✅ Function signature collection and validation works
- ✅ LLVM codegen for `Rvalue::Call` exists
- ⚠️ Build command hangs when compiling programs with function calls
- ❌ No integration tests verifying end-to-end compilation

**Remaining Work:** Debug hanging build, add comprehensive tests, verify correctness

---

## Current State Analysis

### 1. AST Support ✅ COMPLETE

**Location:** `crates/glyph-core/src/lib.rs:84-97`

```rust
pub enum Expr {
    Call {
        callee: Box<Expr>,  // Function being called
        args: Vec<Expr>,     // Argument expressions
        span: Span,
    },
    // ... other variants
}
```

**Status:**
- Parser generates `Expr::Call` nodes correctly
- Callee is an expression (allows `foo.bar()` in future)
- Arguments are expression list

---

### 2. Function Signature Collection ✅ COMPLETE

**Location:** `crates/glyph-frontend/src/mir_lower.rs:43-103`

**Function:** `collect_function_signatures(module, resolver)`

**What it does:**
1. First pass through module items
2. Collects all function definitions
3. Builds `HashMap<String, FnSig>` mapping function names to signatures
4. Validates:
   - No duplicate function names
   - Parameter types exist (via resolver)
   - Return types exist (via resolver)
5. Returns signatures + diagnostics

**FnSig Structure:**
```rust
struct FnSig {
    params: Vec<Option<Type>>,  // Parameter types
    ret: Option<Type>,           // Return type
}
```

**Integration:**
- Called from `lower_module()` before lowering any functions
- Passed to all `lower_function()` calls via `LowerCtx`
- Enables forward references (function can call any other function)

---

### 3. MIR Lowering ✅ COMPLETE

**Location:** `crates/glyph-frontend/src/mir_lower.rs:435-490`

**Function:** `lower_call(ctx, callee, args, span, require_value)`

**Implementation Details:**

```rust
fn lower_call<'a>(
    ctx: &mut LowerCtx<'a>,
    callee: &'a Expr,
    args: &'a [Expr],
    span: Span,
    require_value: bool,
) -> Option<Rvalue> {
    // 1. Validate callee is identifier
    let Expr::Ident(name, _) = callee else {
        ctx.error("call target must be an identifier", Some(span));
        return None;
    };

    // 2. Look up function signature
    let Some(sig) = ctx.fn_sigs.get(&name.0) else {
        ctx.error(format!("unknown function '{}'", name.0), Some(span));
        return None;
    };

    // 3. Validate argument count
    if sig.params.len() != args.len() {
        ctx.error(...);
        return None;
    }

    // 4. Validate return type if value required
    if require_value && sig.ret.is_none() {
        ctx.error("void function used in value context", Some(span));
        return None;
    }

    // 5. Lower argument expressions
    let mut lowered_args = Vec::new();
    for arg in args {
        if let Some(val) = lower_expr(ctx, arg) {
            lowered_args.push(val);
        } else {
            return None;  // Error in argument
        }
    }

    // 6. Create Call rvalue
    Some(Rvalue::Call {
        name: name.0.clone(),
        args: lowered_args,
    })
}
```

**Validation:**
- ✅ Callee must be identifier (no function pointers yet)
- ✅ Function must exist
- ✅ Argument count must match
- ✅ Return type checked when used in value context
- ❌ Argument types NOT validated (future work)

**Called From:**
- `lower_expr_stmt()` - when call is statement: `foo();`
- `lower_expr()` - when call is expression: `let x = foo();`

**Test Coverage:**
- No unit tests for `lower_call()` found
- MIR fixture exists: `tests/fixtures/mir/call_placeholder.glyph`

---

### 4. LLVM Codegen ✅ IMPLEMENTED, ⚠️ ISSUES

**Location:** `crates/glyph-backend/src/codegen.rs:413-438`

**Implementation:**

```rust
Rvalue::Call { name, args } => {
    // 1. Look up function declaration
    let callee = functions
        .get(name)
        .copied()
        .ok_or_else(|| anyhow!("unknown function {}", name))?;

    // 2. Codegen arguments
    let mut llvm_args: Vec<LLVMValueRef> = Vec::new();
    for arg in args {
        llvm_args.push(self.codegen_value(arg, func, local_map)?);
    }

    // 3. Get function type
    let fn_ty = LLVMGetElementType(LLVMTypeOf(callee));
    if fn_ty.is_null() {
        bail!("failed to get llvm function type for {}", name);
    }

    // 4. Build call instruction
    let call_name = CString::new("call")?;
    Ok(LLVMBuildCall2(
        self.builder,
        fn_ty,
        callee,
        llvm_args.as_mut_ptr(),
        llvm_args.len() as u32,
        call_name.as_ptr(),
    ))
}
```

**Issues Found:**
1. **LLVMGetElementType usage:** This API might not be correct for functions
   - Should possibly use `LLVMGlobalGetValueType` instead
   - Or retrieve type from function declaration directly

2. **Implicit Returns:** The test fixture `call_basic.glyph` uses implicit returns:
   ```glyph
   fn add(a: i32, b: i32) -> i32 {
     a + b  // implicit return
   }
   ```
   This might be causing issues in lowering or codegen

3. **Build Hangs:** `glyph-cli build` hangs when compiling function calls
   - Check passes: `glyph-cli check call_basic.glyph` → 0 diagnostics
   - Build hangs: `glyph-cli build call_basic.glyph` → timeout
   - Suggests issue in codegen or LLVM IR generation, not MIR lowering

**Function Declaration:**
- Functions declared upfront in `declare_functions()` (line 126-139)
- Stored in `HashMap<String, LLVMValueRef>`
- Passed to `codegen_function_body()`

---

## Design Decisions & Trade-offs

### 1. Direct Calls Only (Phase 1)
**Decision:** Callee must be identifier
**Rationale:** Simplifies implementation, no function pointers needed
**Future:** Allow `Expr` callee for method calls, closures

### 2. No Type Checking (Phase 1)
**Decision:** Don't validate argument types match parameters
**Rationale:** Type system not fully built, focus on working implementation
**Risk:** Runtime errors for type mismatches
**Future:** Add type checking pass before MIR lowering

### 3. Forward References Allowed
**Decision:** Functions can call any function in module
**Rationale:** Natural for developers, matches Rust/C behavior
**Implementation:** `collect_function_signatures()` before lowering

### 4. Recursion Supported
**Decision:** No special handling for recursive calls
**Rationale:** LLVM handles recursion naturally
**Note:** No tail call optimization yet

---

## Test Strategy

### Current Test Coverage
- ✅ Parser test: function call syntax
- ✅ MIR fixture: `call_placeholder.glyph`
- ✅ Codegen fixture: `call_basic.glyph`
- ❌ No MIR snapshot tests for calls
- ❌ No codegen integration tests
- ❌ No JIT execution tests

### Proposed Tests

#### Unit Tests

**mir_lower tests:**
```rust
#[test]
fn lowers_function_call_to_rvalue() {
    // Test: let x = add(1, 2)
    // Verify: Rvalue::Call with correct args
}

#[test]
fn call_unknown_function_reports_error() {
    // Test: foo()  // foo doesn't exist
    // Verify: diagnostic generated
}

#[test]
fn call_wrong_arg_count_reports_error() {
    // Test: add(1)  // expects 2 args
    // Verify: diagnostic generated
}

#[test]
fn call_void_function_in_value_context_reports_error() {
    // Test: let x = print()  // print returns void
    // Verify: diagnostic generated
}
```

**codegen tests:**
```rust
#[test]
fn codegen_call_instruction() {
    // Test: Simple call
    // Verify: LLVM IR contains "call i32 @add"
}
```

#### Integration Tests

**MIR Snapshot Tests:**
- `tests/fixtures/mir/call_basic.glyph`
- `tests/fixtures/mir/call_with_return.glyph`
- `tests/fixtures/mir/call_multiple_args.glyph`
- `tests/fixtures/mir/call_recursive.glyph`

**Codegen Integration Tests:**
- `tests/fixtures/codegen/call_add.glyph` - Simple addition call
- `tests/fixtures/codegen/call_factorial.glyph` - Recursive factorial
- `tests/fixtures/codegen/call_fibonacci.glyph` - Recursive fibonacci
- `tests/fixtures/codegen/call_chain.glyph` - Chain calls: f(g(h(x)))

#### End-to-End Tests

**JIT Execution Tests:**
```rust
#[test]
fn jit_execute_function_call() {
    let code = r#"
        fn add(a: i32, b: i32) -> i32 {
            ret a + b
        }
        fn main() -> i32 {
            ret add(10, 20)
        }
    "#;
    // Compile and execute
    // Assert: returns 30
}

#[test]
fn jit_execute_factorial() {
    let code = r#"
        fn factorial(n: i32) -> i32 {
            if n <= 1 {
                ret 1
            } else {
                ret n * factorial(n - 1)
            }
        }
        fn main() -> i32 {
            ret factorial(5)
        }
    "#;
    // Compile and execute
    // Assert: returns 120
}
```

---

## Remaining Work

### Priority 1: Debug Hanging Build ⚠️

**Issue:** `glyph-cli build` hangs on function call programs

**Investigation Steps:**
1. Add debug prints to codegen_rvalue for Call case
2. Check if LLVMGetElementType returns valid type
3. Verify LLVMBuildCall2 parameters are correct
4. Test with explicit returns instead of implicit
5. Check if dump_ir() is causing hang

**Potential Fixes:**
- Replace `LLVMGetElementType(LLVMTypeOf(callee))` with correct API
- Use `LLVMGlobalGetValueType(callee)` for function types
- Ensure function type matches parameter count/types

### Priority 2: Add Comprehensive Tests

**Tasks:**
1. Create MIR snapshot tests (3-4 fixtures)
2. Add codegen integration tests (3-4 fixtures)
3. Add unit tests for lower_call validation
4. Add JIT execution tests for factorial/fibonacci

**Est. Time:** 2-3 hours

### Priority 3: Verify Correctness

**Tasks:**
1. Verify LLVM IR contains correct call instructions
2. Test recursion works
3. Test chained calls work
4. Test void functions work
5. Verify return values are properly handled

**Est. Time:** 1-2 hours

---

## Implementation Plan

### Phase 1: Debug & Fix (2-3 hours)

**Step 1.1:** Isolate hang location
- Add debug prints to codegen
- Identify exact line causing hang

**Step 1.2:** Fix LLVM API usage
- Research correct LLVM function type API
- Update codegen_rvalue Call case
- Test with simple call

**Step 1.3:** Test implicit returns
- Verify implicit returns work with calls
- Add explicit returns if needed temporarily

### Phase 2: Testing (2-3 hours)

**Step 2.1:** MIR snapshot tests
- Create 4 test fixtures
- Run with INSTA_UPDATE=always
- Verify snapshots are correct

**Step 2.2:** Codegen integration tests
- Create test module in glyph-cli/tests/
- Add 4 tests verifying LLVM IR
- Ensure tests pass

**Step 2.3:** JIT execution tests
- Create test module in glyph-backend/tests/
- Add factorial and fibonacci tests
- Verify execution produces correct results

### Phase 3: Documentation (1 hour)

**Step 3.1:** Update PROGRESS.md
- Mark function calls as complete
- Update test coverage numbers

**Step 3.2:** Update NEXT_SESSION.md
- Document function call completion
- Plan next feature

**Step 3.3:** Update PLAN.md
- Mark M7 complete
- Add M8 for next feature

---

## Success Criteria

### Minimum Success:
- [ ] Build no longer hangs on function calls
- [ ] Can compile simple add(1, 2) program
- [ ] LLVM IR contains correct call instructions
- [ ] At least 2 integration tests passing

### Full Success:
- [ ] All 4 MIR snapshot tests passing
- [ ] All 4 codegen integration tests passing
- [ ] JIT execution tests for factorial/fibonacci passing
- [ ] Recursion verified working
- [ ] Documentation updated
- [ ] Committed to git

---

## Known Limitations

1. **No function pointers:** Callee must be identifier
2. **No type checking:** Argument types not validated
3. **No varargs:** Fixed argument count only
4. **No extern functions:** Can't call C functions yet
5. **No method calls:** No struct.method() syntax
6. **No closures:** No lambda expressions

---

## Future Enhancements

### Short Term (Next Sprint):
- Add argument type validation
- Support void functions properly
- Add better error messages

### Medium Term:
- Function pointers as first-class values
- Extern function declarations
- Method calls on structs

### Long Term:
- Closures and lambda expressions
- Tail call optimization
- Inline functions

---

## References

**Files:**
- `crates/glyph-core/src/lib.rs` - AST definitions
- `crates/glyph-frontend/src/mir_lower.rs` - MIR lowering (lines 43-490)
- `crates/glyph-backend/src/codegen.rs` - LLVM codegen (lines 413-438)

**Test Fixtures:**
- `tests/fixtures/codegen/call_basic.glyph`
- `tests/fixtures/mir/call_placeholder.glyph`

**LLVM APIs:**
- `LLVMBuildCall2` - Build call instruction
- `LLVMGlobalGetValueType` - Get function type from global
- `LLVMTypeOf` - Get type of value

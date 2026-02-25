# argv Double-Free Bug: Investigation & Resolution

**Status: RESOLVED** — scribe builds and runs with full argv usage.

## Original Symptom

Running any scribe binary crashed with `malloc: pointer being freed was not allocated` — even a trivial `main` that just does `return 0`, as long as `from std/sys import argv` is in the module. The hex pointer `0x7465677261742f2e` decodes to `"./target"` = start of `argv[0]`.

## Root Cause

Two interacting bugs in the MIR lowering for argv:

### Bug 1: Scope-exit drop of shallow-copy argv local

In `flow.rs:76-93`, when a function imports `argv`, MIR lowering injects a local that **loads** the `Vec<String>` from the `glyph.argv.vec` global — a shallow bitwise copy (same data pointer). The local is marked `Initialized`, so `drop_all_active_locals()` drops it on return, freeing the global's backing memory. This caused the original `malloc: pointer being freed was not allocated` crash.

### Bug 2: `handle_reassign` drops argv local before first assignment

Even after adding `skip_drop` to prevent scope-exit drops, the `handle_reassign()` method in `context.rs` still emitted a `Drop` before the first assignment to the argv local. This dropped uninitialized memory in `__glyph_main`, causing the binary to **hang** before user code ever ran (the `println` on the first line of main never fired).

## How argv Works End-to-End

### Codegen Level (glyph-backend)

1. **Global variables** (`entry.rs:29-63`): Three globals are created:
   - `glyph.argc` — i64
   - `glyph.argv` — i8** (raw C argv)
   - `glyph.argv.vec` — Vec<String> (processed Glyph vector)

2. **Main wrapper** (`entry.rs:227-330`): When any MIR function uses argv:
   - User's `main()` is renamed to `__glyph_main()`
   - A new C-style `main(int argc, char** argv)` is generated
   - This wrapper stores argc/argv in globals, builds the Vec<String> by `strdup()`-ing each C string, stores it in `glyph.argv.vec`, then calls `__glyph_main()`

3. **argv "call"** (`rvalue.rs:514-515`): `Rvalue::Call { name: "argv" }` is **not** a real function call. It's special-cased to emit `LLVMBuildLoad2(builder, vec_ty, glyph.argv.vec)` — a bitwise load from the global. This produces a **shallow copy** (same internal data/length/capacity pointers).

### MIR Level (glyph-frontend)

In `flow.rs:76-93`, for every function in a module that imports `std/sys/argv`:
```rust
if imports_sys_argv(resolver) && !ctx.bindings.contains_key("argv") {
    let argv_local = ctx.fresh_local(Some("argv"));
    // Set type to Vec<String>
    // Mark as Initialized
    // Mark skip_drop = true  ← NEW
    // Bind "argv" name to this local
    // Emit: argv_local = call argv()  (becomes a global load at codegen)
}
```

### Drop Behavior

- `drop_local_if_needed()` checks if a local has type with drop glue (Vec<String> does) and is Initialized, then emits `MirInst::Drop`
- `handle_reassign()` emits a pre-assignment drop for previously-initialized locals (to prevent leaks on re-assignment)
- `drop_all_active_locals()` is called at function return, iterating all scoped locals in reverse
- The codegen only processes explicit `MirInst::Drop` instructions — no automatic cleanup

## Fix: `skip_drop` Field on `Local`

### Step 1: Added `skip_drop` to `Local` struct
**File:** `glyph-core/src/mir.rs`
```rust
pub struct Local {
    pub name: Option<String>,
    pub ty: Option<Type>,
    pub mutable: bool,
    #[serde(default)]
    pub skip_drop: bool,
}
```

### Step 2: Set `skip_drop = true` on injected argv local
**File:** `glyph-frontend/src/mir_lower/flow.rs` (after line 83)
```rust
ctx.locals[argv_local.0 as usize].skip_drop = true;
```

### Step 3: Guard `handle_reassign` (fixes the hang)
**File:** `glyph-frontend/src/mir_lower/context.rs`
```rust
pub(crate) fn handle_reassign(&mut self, local: LocalId) {
    if self.locals.get(local.0 as usize).map_or(false, |l| l.skip_drop) {
        return;  // ← prevents pre-assignment drop on argv locals
    }
    // ... rest unchanged
}
```

### Step 4: Guard `drop_local_if_needed` (fixes scope-exit double-free)
**File:** `glyph-frontend/src/mir_lower/context.rs`
```rust
pub(crate) fn drop_local_if_needed(&mut self, local: LocalId) {
    if self.locals.get(local.0 as usize).map_or(false, |l| l.skip_drop) {
        return;  // ← prevents scope-exit drop on argv locals
    }
    // ... rest unchanged
}
```

### Step 5: Initialize `skip_drop: false` in `fresh_local`
**File:** `glyph-frontend/src/mir_lower/context.rs`

### Step 6: Propagate `skip_drop` on Move
**File:** `glyph-frontend/src/mir_lower/context.rs` (in `push_inst`, Move handling)

When `Assign { local, value: Rvalue::Move(src) }` is processed and `src.skip_drop == true`, also set `local.skip_drop = true`. This handles `let args = argv` which moves the shallow-copy value to a new local.

```rust
if self.locals.get(src.0 as usize).map_or(false, |l| l.skip_drop) {
    if let Some(dest) = self.locals.get_mut(local.0 as usize) {
        dest.skip_drop = true;
    }
}
```

## Verification Results

### All passing
- `cargo build` in glyph repo — compiles clean
- All 14 unit tests in glyph-cli — pass
- `glyph build --bin scribe` — binary produced
- `./target/debug/scribe` — prints usage, exits 2 (no crash, no hang)
- `./target/debug/scribe run snake` — executes workflow (argv args parsed correctly)

### Safe argv pattern (use this in scribe)
```glyph
let args = argv
let opt = args.get(i)
let val = match opt {
  Some(s) => s.clone(),
  None => { ret error_code },
}
```

### Known limitation: direct indexing still unsafe
Direct index `args[i]` still risks double-free because it produces a reference into the Vec's internal buffer that can be freed as if it were an owned String. These tests remain `#[ignore]` in `argv_redteam.rs`:
- `argv_direct_index_str_ref`
- `argv_direct_index_string_move`
- `argv_loop_direct_index`

**Always use `.get(i)` + `match` + `.clone()` to access argv elements safely.**

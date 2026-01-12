# Formatting TODO

## Completed ✅

### Stdlib Infrastructure
- [x] Stdlib writers: `Stdout`/`Stderr` with `write_all` using `raw_write`, plus `stdout()`/`stderr()` constructor functions
- [x] Stdlib module structure: `std/fmt` module with imports from `std/io` for type resolution
- [x] Type resolution fixes: fmt functions use unqualified types (`&mut Stdout`) with proper imports
- [x] Fixed module resolution: use qualified names (`std::io::raw_write`) for wildcard imports

### Formatting Functions
- [x] fmt_i32: Calls `std::io::fmt_write_i32` extern function
- [x] fmt_bool: Calls `std::io::fmt_write_bool` extern function
- [x] fmt_str: Calls `std::io::fmt_write_str` extern function
- [x] Extern declarations: Added `fmt_write_i32`, `fmt_write_bool`, `fmt_write_str` to std::io

### Runtime Implementation
- [x] Created `runtime/glyph_fmt.c` with implementations of formatting functions
- [x] `glyph_fmt_write_i32`: Converts i32 to decimal ASCII and writes to fd
- [x] `glyph_fmt_write_bool`: Writes "true" or "false" to fd
- [x] `glyph_fmt_write_str`: Writes null-terminated string to fd
- [x] Runtime documentation: Created `runtime/README.md` with build and linking instructions

### Testing
- [x] All tests pass including `formatting_basic`
- [x] No regressions in existing test suite (38 tests passing)

## Recently Completed ✅

### Print API & Lowering (Just Finished!)
- [x] Updated print/println/eprint/eprintln to use streaming lowering
- [x] Type dispatch for interpolation holes: i32, bool, str all working
- [x] Literals use `std::io::raw_write` (streaming, no intermediate allocation)
- [x] Expression holes call appropriate `std::fmt::fmt_*` functions
- [x] Stdout/Stderr selection works correctly (print vs eprint)
- [x] Newline handling works (println vs print)

### Comprehensive Testing
- [x] 12 end-to-end formatting tests all passing
- [x] Tests cover: i32, bool, str holes
- [x] Tests cover: mixed types, inline literals, multiple holes
- [x] Tests cover: empty strings, holes-only, literal segments
- [x] Tests cover: stdout/stderr, newline behavior

## TODO 📝

### Additional Primitives
- [x] Add fmt_u32: unsigned 32-bit integer formatting
- [x] Add fmt_i64: signed 64-bit integer formatting
- [x] Add fmt_u64: unsigned 64-bit integer formatting
- [x] Add fmt_char: character formatting
- [x] Add runtime implementations for new primitives in `glyph_fmt.c`

### Type Checking & Lowering
- [x] Type dispatch for holes: i32, bool, str all implemented
- [x] Updated lowering to call appropriate fmt_* based on type
- [x] Error message for unsupported types
- [ ] Type checking: enforce Format at compile-time (currently runtime check)
- [ ] Better diagnostics for missing Format implementations

### Print API (COMPLETED ✅)
- [x] Updated print/println/eprint/eprintln to use streaming lowering
- [x] No longer uses `puts` - everything streams via raw_write and fmt_*
- [x] Empty messages handled correctly
- [x] Newline appended only for println/eprintln variants

### Testing & Validation (COMPLETED ✅)
- [x] End-to-end test: print with integer holes
- [x] End-to-end test: print with bool holes
- [x] End-to-end test: print with string holes
- [x] End-to-end test: print with mixed types
- [x] End-to-end test: stderr printing (eprint/eprintln)
- [x] End-to-end test: newline behavior differences
- [x] End-to-end test: inline literals (42, true)
- [x] End-to-end test: multiple holes
- [x] End-to-end test: empty interpolation
- [ ] End-to-end test: user-defined `Format` implementation

### Diagnostics
- [ ] Friendly error for holes with types that don't implement `Format`
- [ ] Validate escape handling in interpolated strings ({{, }}, \n, etc.)
- [ ] Test compile-time errors for malformed interpolation

### Codegen Verification
- [ ] Verify LLVM IR uses streaming model (no intermediate String allocation)
- [ ] Verify correct lowering of stdout()/stderr() calls
- [ ] Verify correct lowering of fmt_* calls with proper arguments
- [ ] Add codegen tests for formatting IR

## Notes

### Linking
To use formatting in compiled executables, link against the runtime:
```bash
gcc -c -o glyph_fmt.o runtime/glyph_fmt.c
clang -o myprogram myprogram.o glyph_fmt.o
```

### Current Limitations
- Supported primitives: i32, u32, i64, u64, char, bool, and str; floats and pointers remain unimplemented
- No Format interface implementation for user-defined types yet (needs interface method dispatch)
- Compile-time Format checking not yet enforced (currently fails at MIR lowering)

### What's Working Now ✅
- **Full streaming implementation**: No intermediate string allocation
- **Type dispatch**: i32, bool, str all work in interpolation holes
- **Print family**: print, println, eprint, eprintln all use streaming
- **Runtime**: C implementations ready for i32, bool, str formatting
- **Tests**: 12 comprehensive tests + 38 existing tests all passing

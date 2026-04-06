# File::open Error Path SIGABRT

## Summary

`File::open(path)` crashes with SIGABRT when the file does not exist and the
`Err` branch of the Result is taken. The crash occurs in codegen, not in user
code.

## Reproduction

Minimal Glyph program that triggers the crash:

```glyph
from std/io import File
from std/enums import Result

fn main() -> i32 {
  let result = File::open("/nonexistent_file")
  let status = match result {
    Ok(_f) => 0,
    Err(_e) => 1,
  }
  ret status
}
```

Compiles without errors, but running the binary produces:

```
Signal: SIGABRT (abort)
```

## Root Cause

Located in `crates/glyph-backend/src/codegen/file.rs`. The `err_bb` basic block
that constructs the `Err` variant after `fopen()` returns NULL is incorrect.
The Err variant construction likely has an invalid pointer or incorrect type
layout, causing the abort at runtime.

## Workaround

Check file existence before calling `File::open` to avoid hitting the Err path:

```glyph
from std/net import file_size

fn file_exists(path: str) -> i32 {
  let size = file_size(path)
  if size >= 0 {
    ret 1
  }
  ret 0
}

// Only call File::open when you know the file exists
let exists = file_exists("some_path")
if exists == 1 {
  let result = File::open("some_path")
  // Safe to match — Ok branch will be taken
}
```

## Impact

- Any program that handles missing files via `File::open` + `Err` match will crash
- The Apex web server uses the `file_size()` workaround for 404 detection
- `File::create` is not affected (only `File::open`)

## Found By

Discovered during Apex web server development (2026-04-05) by test-engineer.
Confirmed isolated to codegen — the frontend produces correct MIR.

## Status

Open. Needs investigation in `crates/glyph-backend/src/codegen/file.rs`, specifically
the `err_bb` branch that builds the Err Result variant when fopen returns NULL.

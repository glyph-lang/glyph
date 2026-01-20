# Vector Example

This example demonstrates the new `Vec<T>` support with `push`, `pop`, `len`, and indexed access.

## What it does
- Allocates a `Vec<i32>` with an initial capacity of 2, then pushes 3 items (exercising growth).
- Reads the third element via indexing (bounds-checked).
- Calls `pop()` to produce an `Option<i32>`.
- Calls `len()` (returns `usize`).
- Prints a short message and returns the fetched element.

## Files
- `glyph.toml` — package definition
- `src/main.glyph` — example program

## Build and run
From this directory:

```bash
cargo run --bin glyph-cli --features codegen -- run src/main.glyph
```

Expected behavior: program exits with status based on the returned `i32` (30) and prints `vector demo ran`.

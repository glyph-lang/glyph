# Command-line Arguments Example

This example demonstrates reading command-line arguments via `std/sys::argv` using the `glyph` utility.

## Build and run

From this directory:

```bash
glyph run
```

To pass arguments:

```bash
glyph run -- one two three
```

The program exits with status `0` when arguments exist, `1` otherwise.

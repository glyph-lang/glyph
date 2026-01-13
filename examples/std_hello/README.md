# Stdlib Hello World Example

This is a simple "hello world" program written in Glyph that uses the standard library `std::println`.

It demonstrates:
- Importing the stdlib module
- Printing a string literal with `std::println` (no language comments yet)

## Building

From this directory, run:

```bash
./build.sh
```

Or build manually:

```bash
cargo run --bin glyph-cli --features codegen -- build hello.glyph --emit exe
```

## Running

After building, run the executable:

```bash
./hello
```

Expected output:
```
hello world
```

Or build and run in one command:

```bash
cargo run --bin glyph-cli --features codegen -- run hello.glyph
```

## What's Happening

The program:
1. Imports the `std` module.
2. Calls `std::println` with a string literal, which writes to stdout and appends a newline.
3. Returns `0` to signal success.

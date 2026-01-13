# Hello World Example

This is a simple "Hello World" program written in Glyph that demonstrates:
- Calling C library functions via `extern "C"` FFI
- Compiling Glyph code to native executables
- Using the `putchar` function to output characters

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

After building, you can run the executable:

```bash
./hello
```

Expected output:
```
Hello World!
```

Or build and run in one command:

```bash
cargo run --bin glyph-cli --features codegen -- run hello.glyph
```

## What's Happening

The program:
1. Declares the C standard library function `putchar` using `extern "C"`
2. Calls `putchar` multiple times with ASCII character codes
3. Returns 0 to indicate success

The Glyph compiler:
1. Compiles the source to LLVM IR
2. Generates an object file (.o) using LLVM
3. Links the object file with the C runtime library
4. Produces a native executable for your platform

## Executable Details

The resulting `hello` executable is:
- A native binary (Mach-O on macOS, ELF on Linux)
- Approximately 34KB in size
- Directly executable without any runtime interpreter
- Linked against the system C library

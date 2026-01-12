# Glyph Runtime Library

This directory contains the C runtime library that provides low-level formatting and I/O support for Glyph programs.

## Overview

The Glyph compiler generates LLVM IR that references external C functions for certain operations. This runtime library provides implementations of those functions.

## Files

- `glyph_fmt.c` - Formatting functions for converting Glyph values to text output

## Formatting Functions

### `glyph_fmt_write_i32`
```c
int glyph_fmt_write_i32(int fd, int32_t value)
```
Converts an i32 value to decimal ASCII representation and writes it to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: The i32 value to format
- **Returns:** Number of bytes written, or -1 on error

### `glyph_fmt_write_bool`
```c
int glyph_fmt_write_bool(int fd, bool value)
```
Writes "true" or "false" to the file descriptor based on the boolean value.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: The boolean value to format
- **Returns:** Number of bytes written (4 or 5), or -1 on error

### `glyph_fmt_write_u32`
```c
int glyph_fmt_write_u32(int fd, uint32_t value)
```
Formats an unsigned 32-bit integer as decimal ASCII and writes it to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: Unsigned 32-bit value to format
- **Returns:** Number of bytes written, or -1 on error

### `glyph_fmt_write_i64`
```c
int glyph_fmt_write_i64(int fd, int64_t value)
```
Formats a signed 64-bit integer as decimal ASCII and writes it to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: Signed 64-bit value to format
- **Returns:** Number of bytes written, or -1 on error

### `glyph_fmt_write_u64`
```c
int glyph_fmt_write_u64(int fd, uint64_t value)
```
Formats an unsigned 64-bit integer as decimal ASCII and writes it to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: Unsigned 64-bit value to format
- **Returns:** Number of bytes written, or -1 on error

### `glyph_fmt_write_str`
```c
int glyph_fmt_write_str(int fd, const char* str)
```
Writes a null-terminated string to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `str`: Pointer to null-terminated string
- **Returns:** Number of bytes written, or -1 on error

### `glyph_fmt_write_char`
```c
int glyph_fmt_write_char(int fd, uint32_t value)
```
Encodes a Unicode scalar value to UTF-8 and writes it to the file descriptor.
- **Parameters:**
  - `fd`: File descriptor (1 for stdout, 2 for stderr)
  - `value`: Unicode scalar value to format
- **Returns:** Number of bytes written, or -1 on error

## Building

To build the runtime library:

```bash
# Compile to object file
gcc -c -O2 -o glyph_fmt.o runtime/glyph_fmt.c

# Or create a static library
ar rcs libglyph_fmt.a glyph_fmt.o
```

## Linking

When compiling Glyph programs that use formatting (std::print, std::println, etc.), link against this runtime:

```bash
# Link with runtime
clang -o myprogram myprogram.o glyph_fmt.o

# Or link with static library
clang -o myprogram myprogram.o -L. -lglyph_fmt
```

## Usage in Glyph

These functions are automatically called by the stdlib formatting system:

```glyph
import std

fn main() -> i32 {
  std::print($"The answer is {42}");
  std::println($"Done: {true}");
  ret 0
}
```

The compiler lowers this to calls to:
- `std::io::fmt_write_i32(fd, 42)`
- `std::io::fmt_write_bool(fd, true)`

Which are extern declarations that link to `glyph_fmt_write_i32` and `glyph_fmt_write_bool`.

## Future Additions

Additional formatting functions to be added:
- `glyph_fmt_write_ptr` - Format pointer addresses

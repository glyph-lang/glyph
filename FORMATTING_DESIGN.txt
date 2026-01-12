FORMATTING_DESIGN.txt — Glyph Formatting + Interpolation (No Macros, No `dyn`)
===============================================================================

Purpose
-------
Provide ergonomic, token-efficient printing and formatting without macros:
- print("hello") and print($"x={x} y={y}") unified under one API
- strong static typing and compile-time checking (formatting interface conformance)
- allocation-free by default (stream formatting)
- integrates with Glyph interfaces
- clean MIR + LLVM lowering

Constraints
-----------
- no macros (v0)
- no `dyn` keyword (v0)
- no generics (v0)
- no function overloading (v0)
- no GC; deterministic destruction (RAII)
- compiled to LLVM

Non-goals (v0)
--------------
- printf-style runtime formatting
- rich format specifiers: {x:04}, {x:?}, alignment, precision
- user-defined formatting DSLs
- stable ABI for “format argument lists” as values (defer)
- async printing / buffered logging framework (defer)

Design principle
----------------
Formatting in Glyph is a *language feature*, not a library trick.
Interpolated strings are parsed, type-checked, and lowered by the compiler,
not implemented via macros or runtime formatting.

-------------------------------------------------------------------------------
1) Core abstraction: byte sink (std::io)
-------------------------------------------------------------------------------

1.1 interface Write
-------------------
Minimal byte sink used by formatting and IO.

interface Write {
  fn write_all(self: &mut Self, bytes: &[u8]);
}

Notes:
- write_all is infallible at the type level.
- On OS I/O failure, write_all triggers the runtime failure policy (§4.2).
- write_all must loop internally until all bytes are written or a failure occurs.
- Keep surface area tiny: write_all is the only required method in v0.

1.2 Stdout / Stderr (concrete writers)
-------------------------------------
Stdout and stderr are concrete writer handles.

struct Stdout
struct Stderr

fn stdout() -> Stdout
fn stderr() -> Stderr

impl Write for Stdout { ... }
impl Write for Stderr { ... }

Backend approach (Unix-first):
- wrap file descriptors 1 and 2
- call extern "C" write(fd, buf, len)
- negative return or errno → runtime failure

-------------------------------------------------------------------------------
2) Core abstraction: formatting interface (std::fmt)
-------------------------------------------------------------------------------

2.1 interface Format
--------------------
Any type used inside interpolation holes {expr} must implement Format.

interface Format {
  fn fmt(self: &Self, w: &mut Write);
}

Properties:
- streaming: writes directly into sink
- allocation-free by default
- usable by user-defined types
- no generics required
- interface-dispatched writer (`&mut Write`)

2.2 Formatting styles (v0)
--------------------------
v0 supports a single “display” style.

Not supported in v0:
- debug formatting
- width / precision / alignment
- format specifiers

These may be added later via:
- additional interfaces (e.g. DebugFormat)
- or extended interpolation syntax

-------------------------------------------------------------------------------
3) Interpolated strings (language feature)
-------------------------------------------------------------------------------

3.1 Syntax
----------
Interpolated string literal:
  $"..."

Interpolation holes:
  {expr}

Examples:
  print("hello")
  print($"x={x} y={y}")
  println($"Point={p}")

Escaping:
- {{  → literal '{'
- }}  → literal '}'
- standard string escapes apply (\n, \t, \\, \")

3.2 Unification of plain strings and interpolated strings
---------------------------------------------------------
Plain string literals are implicitly coerced to interpolated strings.

Rules:
- "hello" is treated as equivalent to $"hello"
- $"..." enables interpolation but is otherwise the same type
- The function print always receives an InterpStr

This avoids:
- function overloading
- print_str / println_str APIs
- macro-based formatting

This design is required because Glyph v0 has:
- no overloading
- no macros
- no generics

3.3 Type rules (compile-time)
-----------------------------
For each hole {expr}:
- expr is parsed and type-checked normally
- expr’s static type must implement Format
- otherwise: compile-time error

Guarantee:
- formatting/type mismatches are compile-time errors
- runtime I/O failures are handled by policy (§4.2)

-------------------------------------------------------------------------------
4) Printing API
-------------------------------------------------------------------------------

4.1 Functions (required v0)
---------------------------
Exactly four printing functions exist:

fn print(msg: InterpStr) -> void
fn println(msg: InterpStr) -> void
fn eprint(msg: InterpStr) -> void
fn eprintln(msg: InterpStr) -> void

Behavior:
- print / eprint: write msg exactly
- println / eprintln: write msg, then write "\n"

No Result return type.
Printing is not fallible at the type level.

4.2 Runtime failure policy (v0)
-------------------------------
If an OS write fails:
- program triggers a runtime failure (trap / abort)
- debug builds may emit a diagnostic
- release builds may abort silently

Rationale:
- keeps APIs token-efficient
- avoids Result plumbing for logging/printing
- compile-time checks apply where meaningful (format correctness)

Recoverable I/O may be added later via separate APIs.

-------------------------------------------------------------------------------
5) Lowering / compilation model
-------------------------------------------------------------------------------

5.1 Direct streaming lowering
-----------------------------
The compiler lowers print-family calls directly.

Example:

  print($"x={x} y={y}\n")

Desugars to:

  let mut out = stdout()
  out.write_all(b"x=")
  x.fmt(&mut out)
  out.write_all(b" y=")
  y.fmt(&mut out)
  out.write_all(b"\n")

println adds an unconditional trailing newline.

eprint / eprintln use stderr().

Properties:
- no intermediate String allocation
- no runtime formatting engine
- fully explicit MIR

5.2 InterpStr representation
----------------------------
InterpStr is a compiler-generated value.

In v0:
- InterpStr is only required to be accepted by print-family functions
- passing InterpStr arbitrarily is deferred

Internal model:
- sequence of literal byte slices and typed holes
- compiler expands directly during lowering

-------------------------------------------------------------------------------
6) Default Format implementations
-------------------------------------------------------------------------------

6.1 Required in v0
------------------
Format must be implemented for:
- signed integers: i8, i16, i32, i64, isize
- unsigned integers: u8, u16, u32, u64, usize
- bool
- char
- &str
- pointers (*T, *mut T) — optional but recommended

Numeric formatting:
- base-10
- stack-buffered, no heap allocation

Floats:
- optional in v0
- can be added once integer path is stable

6.2 Containers
--------------
Formatting for containers (Option, Result, arrays, slices) is deferred
until generics exist.

-------------------------------------------------------------------------------
7) User-defined formatting
-------------------------------------------------------------------------------

Example:

struct Point { x: i, y: i }

impl Format for Point {
  fn fmt(self: &Point, w: &mut Write) {
    w.write_all(b"Point(")
    self.x.fmt(w)
    w.write_all(b", ")
    self.y.fmt(w)
    w.write_all(b")")
  }
}

Notes:
- no allocation
- compositional formatting
- relies only on Format + Write

-------------------------------------------------------------------------------
8) Interfaces without `dyn`
-------------------------------------------------------------------------------

Glyph v0 does not expose a `dyn` keyword.

Rule:
- using an interface name in a type position (e.g. &mut Write)
  implies interface-based dispatch

Compiler responsibilities:
- define interface object layout
- generate vtables or equivalent
- support method calls via interface references

This is required only for:
- Write
- Format

-------------------------------------------------------------------------------
9) Diagnostics
-------------------------------------------------------------------------------

Compiler must diagnose:
- missing Format implementation for interpolation holes
- malformed $"..." syntax
- invalid escapes
- type errors inside {expr}

Suggested diagnostic:
  “Type T does not implement interface Format (required for interpolation)”

-------------------------------------------------------------------------------
10) Implementation checklist (v0)
-------------------------------------------------------------------------------

A) Parser
- recognize $"..." literals
- parse holes and escapes

B) Type checker
- type-check hole expressions
- enforce Format conformance

C) Lowering
- special-case print / println / eprint / eprintln
- emit streaming writes + fmt calls

D) Stdlib
- stdout / stderr writers
- Write.write_all implementation
- Format implementations for primitives

E) Tests
- plain string printing
- interpolated printing
- user-defined Format
- missing Format compile error
- println newline behavior

-------------------------------------------------------------------------------
End of file.
-------------------------------------------------------------------------------

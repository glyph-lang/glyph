# Glyph Language Reference (Compressed)

Glyph is a statically-typed compiled language with Rust-like syntax but simpler rules.

## Keywords
`fn` `ret` `let` `mut` `if` `else` `for` `while` `match` `struct` `enum` `interface` `impl` `from` `import` `pub` `type` `break` `cont`

## Syntax Rules
- Statements end at newlines; semicolons optional
- Blocks use `{ }` braces always
- `ret` for return (not `return`), `cont` for continue (not `continue`)
- Expression-oriented: `if`/`match` yield values
- The last expression in a block is the block's return value
- `ret` is optional — the last expression in a function is its return value (prefer implicit return)
- Call void functions directly; `let _ = expr` is unnecessary

## Types
**Primitives:** `i8` `i32` `i64` `u8` `u32` `u64` `usize` `bool` `f32` `f64` `char`
**Generics:** `Vec<T>` `Map<K,V>` `Option<T>` `Result<T,E>`
**Strings:** `str` (borrowed slice) `String` (owned heap string)

## Functions
```glyph
fn add(a: i32, b: i32) -> i32 {
  a + b
}
fn greet(name: str) {
  println($"hello {name}")
}
```

## Variables
```glyph
let x: i32 = 10
let y = 20
let mut z = 0
```

## Control Flow
```glyph
if x > 0 {
  "positive"
} else {
  "non-positive"
}

while x < 10 {
  x = x + 1
}

for i in 0..10 {
  x = x + i
}
```

## Collections
```glyph
let mut v: Vec<i32> = Vec::new()
v.push(10)
v.push(20)
let x = v[0]
let len = v.len()
```

```glyph
from std/map import Map

let m: Map<i32, i32> = Map::new()
m.add(1, 100)
let val = m.get(1)
```

## Error Handling
```glyph
fn safe_divide(a: i32, b: i32) -> Result<i32, String> {
  if b == 0 {
    ret Err(String::from_str("division by zero"))
  }
  Ok(a / b)
}
```

## Enums and Match
```glyph
enum Shape {
  Circle(f64)
  Rect(f64)
}

let result = match shape {
  Circle(r) => r * r,
  Rect(s) => s * s,
}
```

## Structs with Methods
```glyph
struct Point {
  x: i32
  y: i32

  fn norm(self: &Point) -> i32 {
    self.x * self.x + self.y * self.y
  }
}
```

## Imports
```glyph
from std import println
from std/io import File
```
`std` re-exports `std/io`, `std/vec`, `std/enums`, `std/map`, `std/string`.

## I/O
```glyph
from std import println

fn main() -> i32 {
  println("hello")
  println($"value: {x}")
  0
}
```

File I/O uses the `File` type from `std/io`. Error type is `Err` (from `std/enums`).
- `File::create(path: str) -> Result<File, Err>` — create/truncate file for writing
- `File::open(path: str) -> Result<File, Err>` — open existing file for reading
- `file.write(content) -> Result<u32, Err>` — write str or String to file
- `file.read_to_string() -> Result<String, Err>` — read entire file contents
- `file.close() -> Result<i32, Err>` — close the file handle

```glyph
from std/io import File
from std/enums import Err

fn write_greeting(filename: str) -> Result<i32, Err> {
  let created = File::create(filename)
  let file = match created {
    Ok(f) => f,
    Err(e) => {
      ret Err(e)
    },
  }
  file.write("Hello from the program!")
  file.close()
  Ok(0)
}
```

## Key Differences from Rust
- Use `ret` instead of `return`, `cont` instead of `continue`
- Use `from X import Y` instead of `use X::Y`
- Methods inside struct body with explicit `self: &Type` parameter
- Semicolons are optional (newline-terminated)
- Simpler: no lifetimes, no macros, no traits (uses `interface` instead)
- `String::from_str("...")` to create owned String from literal

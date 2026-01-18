# Glyph Language Reference (Compressed)

Glyph is a statically-typed compiled language with Rust-like syntax but simpler rules.

## Keywords
`fn` `ret` `let` `mut` `if` `else` `for` `while` `match` `struct` `enum` `impl` `use` `from` `import` `pub` `type` `break` `cont`

## Syntax Rules
- Statements end at newlines; semicolons optional
- Blocks use `{ }` braces always
- `ret` for return (not `return`)
- Expression-oriented: `if`/`match` yield values

## Types
**Primitives:** `i32` `u32` `i64` `u8` `bool` `f64` `char`
**References:** `&T` (shared) `&mut T` (exclusive)
**Generics:** `Vec<T>` `Map<K,V>` `Option<T>` `Result<T,E>`
**Strings:** `&str` (slice) `String` (owned)

## Functions
```glyph
fn add(a: i32, b: i32) -> i32 {
  ret a + b
}
```

## Variables
```glyph
let x: i32 = 10         // immutable, typed
let y = 20              // immutable, inferred
let mut z = 0           // mutable
```

## Control Flow
```glyph
if x > 0 {
  ret "positive"
} else {
  ret "non-positive"
}

while x < 10 {
  x = x + 1
}

for i in items {
  println($"{i}")
}
```

## Collections
```glyph
from std/vec import Vec

let v: Vec<i32> = Vec::new()
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
fn read_file(path: &str) -> Result<String, Error> {
  let f = open(path)?    // ? operator propagates errors
  let s = f.read()?
  ret Ok(s)
}
```

## Imports
```glyph
from std import println
from std/vec import Vec
from std/io import File
```

## I/O
```glyph
println("hello")              // print with newline
println($"value: {x}")        // string interpolation with $
```

```glyph
from std/io import File

let f = File::create("out.txt")?
f.write("content")?
```

## Structs
```glyph
struct Point {
  x: f64
  y: f64
}

let p = Point { x: 1.0, y: 2.0 }
let x = p.x
```

## Key Differences from Rust
- Use `ret` instead of `return`
- Use `from X import Y` instead of `use X::Y`
- Semicolons are optional
- Simpler: no lifetimes, no macros, fewer type system features

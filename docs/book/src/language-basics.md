# Language Basics

Glyph favors a small set of keywords and a readable, expression-oriented style.

## A Small Program

```glyph
from std import println

fn add(a: i32, b: i32) -> i32 {
  a + b
}

fn main() -> i32 {
  let x = add(2, 3)
  println($"x = {x}")
  ret 0
}
```

## Values, Variables, and Types

- `let name = expr` binds an immutable local.
- `let mut name = expr` binds a mutable local.
- You can annotate types: `let v: Vec<i32> = Vec::new()`.

Common built-in types include:

- integers: `i8`, `i32`, `i64`, `u8`, `u32`, `u64`, `usize`
- floating point: `f32`, `f64`
- `bool`, `char`
- strings: `str` (borrowed string), `String` (owned string)

String literals like `"hello"` have type `str`. Use `String::from_str("...")` when you need an owned `String`.

## Expressions and Return Values

Blocks produce a value: the last expression is the block result.

```glyph
fn abs_i32(x: i32) -> i32 {
  if x < 0 { -x } else { x }
}
```

Use `ret expr` for an explicit return (useful for early exits).

## Control Flow

`if` and `match` are expressions, so you can assign their result.

Loops:

```glyph
fn main() -> i32 {
  let i = 0
  while i < 3 {
    i = i + 1
  }

  let sum = 0
  for n in 0..10 {
    sum = sum + n
  }

  ret sum
}
```

`break` and `continue` work inside `while` and `for`.

## Imports

Imports are file-based. Use `from ... import ...` for local names or `import ...`
for qualified access:

```glyph
from std import println
from std/io import File
import std/enums
```

See [Imports](imports.md) for path rules, aliasing, and collisions.

## Structs

Define structs with named fields:

```glyph
struct Point {
  x: i32
  y: i32
}

fn main() -> i32 {
  let p = Point { x: 1, y: 2 }
  let sum = p.x + p.y
  ret sum
}
```

Struct field ownership:

- You cannot move individual fields out of a struct.
- Borrow or clone the field, or move the whole struct.

```glyph
let name: str = user.name
let owned = user.name.clone()
```

## Enums

Enums can be simple tags or carry payload values:

```glyph
enum OptionI32 {
  None
  Some(i32)
}

fn main() -> i32 {
  let a: OptionI32 = OptionI32::Some(123)
  let b: OptionI32 = OptionI32::None
  let _keep_alive = (a, b)
  ret 0
}
```

## match

Use `match` to branch on enum variants.

```glyph
from std/enums import Result
from std/io import File
from std import println

fn main() -> i32 {
  let created = File::create("out.txt")
  let status = match created {
    Ok(file) => {
      let _ = file.close()
      0
    },
    Err(_err) => {
      println("failed")
      1
    },
  }
  ret status
}
```

## Vectors (Vec)

`Vec<T>` is the standard growable list.

```glyph
fn main() -> i32 {
  let mut v: Vec<i32> = Vec::new()
  v.push(1)
  v.push(2)
  let _popped = v.pop()
  let _len: usize = v.len()
  ret 0
}
```

## Maps (Map)

`Map<K, V>` is a hash map.

```glyph
from std/map import Map

fn main() -> i32 {
  let m: Map<i32, i32> = Map::new()
  let _ = m.add(1, 2)
  let _ = m.update(1, 3)
  let _ = m.get(1)
  ret 0
}
```

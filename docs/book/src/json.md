# JSON

Glyph ships a small JSON type model in `std/json`.

## Types

`JsonValue` is an enum that can represent any JSON value:

```glyph
from std/json import JsonValue

fn main() -> i32 {
  let n = JsonValue::Null
  let b = JsonValue::Bool(true)
  let x = JsonValue::Number(42.5)
  let s = JsonValue::String(String::from_str("hello"))

  // Arrays and objects use `Vec` and `Map`.
  let arr: Vec<JsonValue> = Vec::new()
  let a = JsonValue::Array(arr)

  let obj: Map<String, JsonValue> = Map::new()
  let o = JsonValue::Object(obj)

  let _keep_alive = (n, b, x, s, a, o)
  ret 0
}
```

Parse errors are represented by `ParseError { message: String, position: usize }`.

## Parsing

The parser lives in `std/json/parser` and returns `ParseResult<T>`:

```glyph
from std/json import JsonValue, ParseResult
from std/json/parser import parse

fn main() -> i32 {
  let r: ParseResult<JsonValue> = parse("{\"k\": 1}")
  // Handle Ok(value) / Err(parse_error)
  let _keep_alive = r
  ret 0
}
```

## Status

`std/json` always provides the JSON types.

`std/json/parser::parse` is intended to be a real parser, but some builds may ship a minimal stub while the compiler/runtime evolves. If you observe `parse(...)` always returning `Ok(JsonValue::Null)`, you're on a stubbed build.

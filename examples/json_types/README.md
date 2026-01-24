# JSON Types Example

This example demonstrates using the JSON types provided by Glyph's standard library (`std/json`).

## Overview

The `std/json` module provides:
- **`JsonValue`** enum: Represents any JSON value
  - `Null` - JSON null
  - `Bool(bool)` - JSON boolean
  - `Number(f64)` - JSON number
  - `String(String)` - JSON string
  - `Array(Vec<JsonValue>)` - JSON array
  - `Object(Map<String, JsonValue>)` - JSON object
- **`ParseError`** struct: Error information for parsing failures

## What This Example Shows

1. **Creating JSON values** of all primitive types (Null, Bool, Number, String)
2. **Creating JSON arrays** using the Array variant
3. **Creating JSON objects** using `Map<String, JsonValue>`
4. **Type safety** - all JSON values are statically typed and checked at compile time

## Build and Run

From this directory:

```bash
glyph run
```

Expected output:
```
=== JSON Types Example ===

Creating JSON primitive types:
  - Null
  - Bool(true)
  - Bool(false)
  - Number(42.5)
  - String

Creating a JSON array:
  - Empty array created

Creating a JSON object:
  - Object

=== Example Complete ===

The JSON type system is working!
All JsonValue variants can be created.
```

## Parsing

`std/json/parser::parse` exists and returns `ParseResult<JsonValue>`. Depending on your Glyph build, it may be a real parser or a temporary stub.

## Use Cases

The JSON types can be used for:
- Building JSON-like data structures programmatically
- Type-safe representation of dynamic data
- Foundation for future JSON parsing/serialization

## Learn More

- See `tests/fixtures/codegen/json_*.glyph` for more examples
- The type definitions are in `crates/glyph-frontend/src/stdlib.rs`

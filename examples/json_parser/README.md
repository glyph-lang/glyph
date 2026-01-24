# JSON Parser Example

Demonstrates comprehensive JSON parsing in Glyph using the newly implemented language features.

## Features Showcased

### 1. Mutable Variables (`mut` keyword)
```glyph
let mut pos = 0
pos = skip_whitespace(input, pos)
```

Position tracking through the input string uses mutable variables to maintain state during parsing.

### 2. Tuple Return Types
```glyph
fn parse_value(input: &str, pos: usize) -> Result<(JsonValue, usize), ParseError>
```

Parser functions return tuples containing both the parsed value and the new position in the input stream. This pattern enables sequential parsing without global state.

### 3. Pattern Matching
```glyph
match value {
    JsonValue::Null => { /* handle null */ },
    JsonValue::Bool(b) => { /* handle bool */ },
    JsonValue::Number(n) => { /* handle number */ },
    JsonValue::String(s) => { /* handle string */ },
    JsonValue::Array(arr) => { /* handle array */ },
    JsonValue::Object => { /* handle object */ },
}
```

Exhaustive pattern matching ensures all JSON value types are handled.

### 4. Recursive Data Structures
```glyph
JsonValue::Array(Vec<JsonValue>)  // Arrays can contain arrays
```

The parser handles arbitrary nesting of arrays through recursive calls to `parse_value()`.

### 5. Result-based Error Handling
```glyph
fn parse(input: &str) -> Result<JsonValue, ParseError>
```

Errors propagate through the `Result` type with position information for debugging.

## Supported JSON Types

| Type | Examples | Status |
|------|----------|--------|
| **null** | `null` | ✅ Fully supported |
| **Boolean** | `true`, `false` | ✅ Fully supported |
| **Number** | `42`, `3.14`, `-10` | ✅ Fully supported |
| **String** | `"hello"`, `"with \"escapes\""` | ✅ With escape sequences |
| **Array** | `[1, 2, 3]`, `[[1, 2], [3, 4]]` | ✅ Recursive parsing |
| **Object** | `{}` | ⚠️ Empty only (awaits Map) |

### Escape Sequences
The string parser handles common escape sequences:
- `\"` - Double quote
- `\\` - Backslash
- `\n` - Newline
- `\t` - Tab

## Parser Design

The parser uses a **recursive descent** approach:

1. **Tokenization by byte**: Uses `byte_at()` for character-by-character inspection
2. **Position tracking**: Mutable `pos` variable tracks current position
3. **Tuple returns**: Each parser function returns `(parsed_value, new_position)`
4. **Whitespace skipping**: Automatic handling of spaces, tabs, and newlines
5. **Error propagation**: `Result` type carries errors with position information

### Parser Flow

```
parse(input)
    ↓
parse_value(input, 0)
    ↓
skip_whitespace()
    ↓
[Dispatch based on first character]
    ├─ 'n' → parse_null()
    ├─ 't'/'f' → parse_bool()
    ├─ '"' → parse_string()
    ├─ '[' → parse_array()
    │          ├─ parse_value() [recursive]
    │          ├─ parse_value() [recursive]
    │          └─ ...
    ├─ '{' → parse_object()
    └─ digit/'-' → parse_number()
```

## Running the Example

Build and run:
```bash
cd examples/json_parser
glyph build
./target/debug/json_parser
```

Or run directly:
```bash
glyph run examples/json_parser/src/main.glyph
```

## Example Output

```
╔════════════════════════════════════════╗
║   Glyph JSON Parser Demo              ║
╚════════════════════════════════════════╝

Showcasing Glyph language features:
  ✓ Mutable variables (mut keyword)
  ✓ Tuple return types: (JsonValue, usize)
  ✓ Result-based error handling
  ✓ Recursive data structures
  ✓ Pattern matching with match expressions

=== Testing Primitives ===

Testing: null ... OK
  Parsed: null
Testing: true ... OK
  Parsed: true
Testing: false ... OK
  Parsed: false
...
```

## Implementation Details

### Key Files
- `crates/glyph-frontend/src/stdlib/json/parser.glyph` - Core parser implementation
- `crates/glyph-frontend/src/stdlib.rs` - JSON module definition (lines 870-973)

### ASCII Character Codes
The parser uses ASCII codes for token matching:
- `32` - Space
- `34` - Double quote `"`
- `44` - Comma `,`
- `45` - Minus `-`
- `46` - Dot `.`
- `48-57` - Digits `0-9`
- `91` - Left bracket `[`
- `93` - Right bracket `]`
- `123` - Left brace `{`
- `125` - Right brace `}`

## Future Enhancements

1. **Object Support**: Full object parsing with `Map<String, JsonValue>` (requires Map implementation)
2. **Pretty Printing**: Add JSON serialization (`to_string()` method on `JsonValue`)
3. **Streaming Parser**: Parse JSON from file/network streams
4. **Query API**: JSONPath-like query interface for extracting values
5. **Schema Validation**: Optional schema validation support
6. **Number Formatting**: Convert numbers to strings for display
7. **Unicode Support**: Handle `\uXXXX` escape sequences in strings

## Learning Resources

This example demonstrates practical use of:
- **Mutable variables** for stateful parsing
- **Tuples** for returning multiple values
- **Pattern matching** for type dispatch
- **Recursive functions** for nested structures
- **Error handling** with `Result` type

Study the parser implementation to learn these patterns in a real-world context.

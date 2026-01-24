# JSON Parser Example

Demonstrates using Glyph's standard-library JSON parser: `std/json/parser::parse`.

## What this example does

- Calls `parse(...)` on a handful of JSON inputs.
- Uses pattern matching to verify the parsed value has the expected shape (including nested arrays/objects).
- Exits non-zero if parsing fails (or if the parser is stubbed/broken).

## Build and run

From this directory:

```bash
glyph run
```

## API surface

```glyph
from std/json import JsonValue, ParseResult
from std/json/parser import parse

fn parse(input: &str) -> ParseResult<JsonValue>
```

## Parser notes

The stdlib parser is currently a minimal placeholder (it always returns `Null`).

This example is written to detect that: it inspects nested results and exits non-zero until the full parser is shipped.

TODO
x Review std/json parser behavior
x Decide if string redesign is needed
x Provide downstream JSON syntax limits
String type redesign
- Not needed based on current evidence. The parser returns correct bytes for "ab", so the mismatch points to hashing or String→str interop, not a fundamental string-representation flaw.
Downstream JSON syntax (acceptable vs limited)
Accepts
- One JSON value per input with ASCII whitespace (space, tab, CR, LF) around tokens.
- null, true, false, numbers, strings, arrays, objects.
- Strings with escapes: \", \\, \/, \b, \f, \n, \r, \t, \uXXXX (including valid surrogate pairs).
Limitations
- Trailing characters after the value are rejected; no comments.
- \u0000 is rejected.
- Duplicate object keys have undefined behavior (Map::add result ignored).
- Numbers are parsed via atof (float64); leading zeros are accepted (non‑strict JSON).
- Very long numbers or deeply nested arrays/objects can hit recursion limits.
- Unescaped control chars in strings are rejected (standard JSON rule).
Verification
- Based on crates/glyph-frontend/src/stdlib/json/parser.glyph.
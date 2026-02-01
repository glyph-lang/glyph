# Struct Field Access ABI Bug

## Summary
When passing a large struct (~160 bytes) containing nested structs by value to a function, accessing its fields causes SIGSEGV or corrupted data. The struct is passed correctly, but field access reads from wrong memory locations due to an ABI mismatch.

## Root Cause
The caller and callee disagree on struct layout when:
1. Struct is large (>16 bytes, at ABI boundary for x86-64 System V)
2. Contains nested structs with String fields
3. Passed by value (not by reference)
4. Fields are accessed sequentially, causing partial moves

## Observed Behavior in Scribe
In `/Users/svenkratz/repos/scribe/src/session.glyph`:

```glyph
fn encode_session(session: Session) -> String {
  // Session struct: ~160 bytes with nested structs
  let mut obj = begin_object()
  obj = add_string_field(obj, "project_root", session.project_root, false)  // Moves field
  obj = add_field(obj, "docs", encode_docs(session.docs), false)  // ← CRASH or corruption here
  // After first field access, subsequent accesses read garbage
  ...
}
```

### What Works
```glyph
fn encode_session(_session: Session) -> String {
  ret String::from_str("{\"test\":123}")  // ← No crash - struct passed correctly
}
```

### What Crashes
```glyph
fn encode_session(session: Session) -> String {
  let v = session.version  // Accessing any field causes crash
  ...
}
```

## Workaround
Use String.clone() on all fields OR completely inline encoding logic to avoid passing nested structs.

## Test Case Location
- File: `/Users/svenkratz/repos/glyph/test/struct_field_access_bug/src/main.glyph`
- Build: `cd /Users/svenkratz/repos/glyph/test/struct_field_access_bug && glyph build`
- Run: `glyph run`

The simple test case in main.glyph currently works, suggesting the bug requires a specific size/layout combination found in the real scribe Session struct.

## Real World Example
The scribe init command crashes when trying to serialize a Session struct to JSON. The Session struct contains multiple nested structs (Docs, Models, Planning, Run, etc.) with String fields, totaling ~160 bytes.

# Case Demo

This example demonstrates the v0.1 case system with a local path dependency.

Structure
- `my_lib/`: a library case exporting `greet` from `src/lib.glyph`
- `my_app/`: a binary case that depends on `my_lib`

Try It
1. `cd examples/case_demo/my_app`
2. `glyph build`
3. `glyph run`

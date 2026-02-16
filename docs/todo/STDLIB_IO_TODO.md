# Stdlib I/O TODO

Legend: `[ ]` not started, `[~]` in progress, `[x]` done

## File::open str-vs-&str behavior
TODO
[ ] Decide whether `File::open(&str)` should be rejected with a clear diagnostic or supported via auto-deref
[ ] If rejected: add a compile-time diagnostic test that passes `&str` to `File::open` and asserts the error
[ ] If supported: implement auto-deref for `&str` -> `str` at call sites and add a runtime test that opens a real file

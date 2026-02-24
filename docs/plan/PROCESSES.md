# External Process Launching

## Goal

Add a minimal `std/process` API to Glyph to:

- launch an external process (`cmd` + `args`)
- wait for it to complete
- return an `i32` exit status

## Non-goals (v1)

- capturing stdout/stderr, piping, streaming IO
- environment variables, working directory, timeouts
- process handles / async polling
- Windows support unless explicitly required

## Open Question

- Do we need Windows support now, or is macOS/Linux-only OK for v1?

## Public API (Glyph stdlib)

Add a new module `std/process` with a single extern function:

- `extern fn run(cmd: str, args: Vec<String>) -> i32`

Semantics:

- Normal exit: return the child exit code.
- Terminated by signal: return `128 + signal` (Unix convention).
- Spawn/wait failure: return `-(errno)` (negative).

Notes:

- Keep return type as `i32` for v1 (as requested). We can add a richer `Result`/`ExitStatus` later.

## Runtime Implementation (C)

Add `runtime/glyph_process.c` and compile it into the existing `libglyph_runtime.a`.

Export a runtime symbol called `glyph_process_run`.

### ABI

Glyph currently represents:

- `str` / `String` as `char*`
- `Vec<T>` as `{ T* data; i64 len; i64 cap }`

So `Vec<String>` is `Vec<char*>`.

Suggested C signature:

- `int32_t glyph_process_run(const char* cmd, GlyphVec args)`

Where `GlyphVec` is the runtime struct matching the codegen layout:

- `void* data; int64_t len; int64_t cap;`

and `data` points to an array of `char*`.

### Implementation (macOS/Linux)

Use `posix_spawnp` + `waitpid`:

- Build `argv`:
  - `argv[0] = cmd`
  - `argv[1..] = args[i]`
  - `argv[last] = NULL`
- `posix_spawnp(&pid, cmd, NULL, NULL, argv, environ)`
- `waitpid(pid, &status, 0)`
- Map `status`:
  - `WIFEXITED(status)` -> `WEXITSTATUS(status)`
  - `WIFSIGNALED(status)` -> `128 + WTERMSIG(status)`

Error handling:

- if `posix_spawnp` fails: return `-err` (where `err` is the posix_spawnp return value)
- if `waitpid` fails: return `-errno`

Memory:

- allocate the `argv` pointer array via `malloc` and `free` it after spawn
- do not copy the strings; just reference `cmd` and the passed-in `char*` pointers

Unsupported platforms:

- return `-ENOSYS` (or `-1` if we prefer to avoid errno dependencies)

## Toolchain Integration

### Runtime build

- Update `crates/glyph-backend/build.rs` to compile `runtime/glyph_process.c` into `libglyph_runtime.a`.

### Stdlib module

- Update `crates/glyph-frontend/src/stdlib.rs` to add:
  - module `std/process`
  - `ExternFunctionDecl` for `run`:
    - `abi: Some("C")`
    - params: `cmd: str`, `args: Vec<String>`
    - ret: `i32`
    - `link_name: Some("glyph_process_run")`

### Linking

- No extra flags expected:
  - macOS already links `-lSystem`
  - Linux already links `-lc`

### JIT note

JIT execution currently requires explicit symbol mapping for runtime helpers.

Options:

1) AOT-only for v1 (recommended for reliability)
2) Add a JIT mapping for `glyph_process_run` similarly to other runtime helpers

## Tests / Validation

Add an integration test that builds an executable and runs it (AOT, not JIT):

- `run("true", [])` -> expect `0`
- `run("false", [])` -> expect `1`

Optionally:

- `run("sh", ["-c", "exit 7"])` -> expect `7` (if `sh` is available)

Test harness options:

1) Rust integration test in `crates/glyph-cli/tests/` that builds an exe and executes it
2) An `examples/process_run/` example plus a CI step

## Milestones

1) Implement runtime `glyph_process_run` (macOS/Linux) and compile it into the runtime library
2) Add `std/process::run` extern declaration
3) Add an example program demonstrating spawn + exit-code behavior
4) Add an AOT integration test that validates exit codes
5) (Optional) Add JIT support by providing `glyph_process_run` to the JIT symbol resolver

# External Processes

To launch external (non-Glyph) programs, use `std/process::run`.

```glyph
from std/process import run

fn main() -> i32 {
  // Note: `Vec::new()` currently defaults to `Vec<i32>`, so annotate the type.
  let mut args: Vec<String> = Vec::new()
  args.push(String::from_str("--version"))

  // Returns the process exit code (0 means success).
  ret run("git", args)
}
```

## Return value

`run(cmd, args) -> i32` returns:

- `0..=255`: normal process exit code
- `128 + signal`: if the process exits due to a signal (Unix)
- negative values: an OS error code (typically `-errno`)

Platform note: currently implemented on Unix; on other platforms it returns `-ENOSYS`.

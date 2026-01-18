# File I/O

File utilities live in `std::io::File` and return `Result` values.

```glyph
from std/io import File

fn main() -> i32 {
  let created = File::create("out.txt")
  let status = match created {
    Ok(file) => {
      let _ = file.write_string(String::from_str("hello"))
      let _ = file.close()
      0
    },
    Err(_) => 1,
  }
  ret status
}
```

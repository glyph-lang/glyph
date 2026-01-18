```rust
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;

pub fn write_greeting(filename: &str) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let mut file = File::create(&path)?;

    file.write_all(b"Hello from the program!")?;
    
    Ok(())
}
```
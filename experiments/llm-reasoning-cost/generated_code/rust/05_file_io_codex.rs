use std::fs::File;
use std::io::{self, Write};
use std::path::Path;

pub fn write_greeting<P: AsRef<Path>>(filename: P) -> io::Result<()> {
    let mut file = File::create(filename)?;
    file.write_all(b"Hello from the program!")?;
    file.flush()?;
    Ok(())
}
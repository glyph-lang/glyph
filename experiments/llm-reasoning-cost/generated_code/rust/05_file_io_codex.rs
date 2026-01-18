use std::fs::File;
use std::io::{self, Write};

pub fn write_greeting(filename: &str) -> io::Result<()> {
    let mut file = File::create(filename)?;
    file.write_all(b"Hello from the program!")?;
    Ok(())
}
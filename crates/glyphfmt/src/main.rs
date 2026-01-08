use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use glyph_frontend::{FrontendOptions, compile_source};

#[derive(Parser, Debug)]
#[command(name = "glyphfmt", about = "Minimal formatter placeholder")]
struct Args {
    /// File to format
    path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let source = fs::read_to_string(&args.path)?;

    // Placeholder: round-trip through parser to validate syntax.
    let output = compile_source(&source, FrontendOptions::default());
    if output.diagnostics.is_empty() {
        // Emit input unchanged for now to keep idempotent behavior.
        print!("{}", source);
        Ok(())
    } else {
        for diag in output.diagnostics {
            eprintln!("{:?}", diag);
        }
        anyhow::bail!("format failed: parse errors")
    }
}

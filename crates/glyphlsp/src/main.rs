use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;
use glyph_frontend::{FrontendOptions, compile_source};
use serde::Serialize;

#[derive(Parser, Debug)]
#[command(name = "glyphlsp", about = "Glyph LSP placeholder server")]
struct Args {
    /// Probe a file and emit diagnostics as JSON
    #[arg(long, value_name = "PATH")]
    probe: PathBuf,
}

#[derive(Serialize)]
struct ProbeResult {
    diagnostics: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let source = fs::read_to_string(&args.probe)?;
    let output = compile_source(&source, FrontendOptions::default());
    let payload = ProbeResult {
        diagnostics: output.diagnostics.len(),
    };
    let json = serde_json::to_string_pretty(&payload)?;
    println!("{}", json);
    Ok(())
}

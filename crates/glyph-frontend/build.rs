use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    // Ship the full std/json/parser implementation.
    let parser_path = PathBuf::from(&manifest_dir).join("src/stdlib/json/parser.glyph");

    // Read the parser source
    let source = fs::read_to_string(&parser_path).expect("Failed to read parser.glyph");

    // We need to lex and parse the file to get the AST Module
    // However, we can't use glyph_frontend here because it's the crate being built
    // So we'll use a simple approach: just serialize the source itself
    // and parse it at runtime during stdlib initialization

    // For now, we'll create a placeholder that indicates we need to parse at runtime
    // A better approach would be to extract the lexer/parser to a separate crate

    // Write the source as a constant that can be parsed at runtime
    let out_dir = env::var("OUT_DIR").unwrap();
    let output_path = PathBuf::from(out_dir).join("json_parser_source.txt");
    fs::write(&output_path, source).expect("Failed to write parser source");

    // Tell cargo to rerun if parser source changes
    println!("cargo:rerun-if-changed={}", parser_path.display());
}

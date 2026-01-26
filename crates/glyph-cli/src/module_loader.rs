use std::collections::HashMap;
use std::fs;
use std::path::Path;

use anyhow::{anyhow, Result};
use glyph_core::ast::Module;
use glyph_frontend::{lex, parse};
use walkdir::WalkDir;

pub fn module_id_from_path(root: &Path, path: &Path) -> Result<String> {
    let rel_path = path.strip_prefix(root).unwrap_or(path);
    let module_id = rel_path
        .with_extension("")
        .to_str()
        .ok_or_else(|| anyhow!("invalid path: {:?}", rel_path))?
        .replace(std::path::MAIN_SEPARATOR, "/");
    Ok(module_id)
}

/// Discover and parse all .glyph files in a directory tree
pub fn discover_and_parse_modules(root: &Path) -> Result<HashMap<String, Module>> {
    let mut modules = HashMap::new();

    for entry in WalkDir::new(root).follow_links(true) {
        let entry = entry?;
        let path = entry.path();

        // Skip non-.glyph files
        if path.extension().and_then(|s| s.to_str()) != Some("glyph") {
            continue;
        }

        // Read and parse the file
        let source = fs::read_to_string(path)?;
        let lex_out = lex(&source);
        if !lex_out.diagnostics.is_empty() {
            for diag in &lex_out.diagnostics {
                eprintln!("{:?} in {:?}", diag, path);
            }
            return Err(anyhow!("lex failed for {:?}", path));
        }

        let parse_out = parse(&lex_out.tokens, &source);
        if !parse_out.diagnostics.is_empty() {
            for diag in &parse_out.diagnostics {
                eprintln!("{:?} in {:?}", diag, path);
            }
            return Err(anyhow!("parse failed for {:?}", path));
        }

        let module_id = module_id_from_path(root, path)?;
        modules.insert(module_id, parse_out.module);
    }

    Ok(modules)
}

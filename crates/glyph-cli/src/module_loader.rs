use std::collections::HashMap;
use std::fs;
use std::path::Path;

use anyhow::{anyhow, Result};
use glyph_core::ast::Module;
use glyph_frontend::{lex, parse};
use walkdir::WalkDir;

use crate::diagnostics::SourceInfo;

pub fn module_id_from_path(root: &Path, path: &Path) -> Result<String> {
    let rel_path = path.strip_prefix(root).unwrap_or(path);
    let module_id = rel_path
        .with_extension("")
        .to_str()
        .ok_or_else(|| anyhow!("invalid path: {:?}", rel_path))?
        .replace(std::path::MAIN_SEPARATOR, "/");
    Ok(module_id)
}

pub struct ModuleLoadResult {
    pub modules: HashMap<String, Module>,
    pub sources: HashMap<String, SourceInfo>,
    pub diagnostics: Vec<glyph_core::diag::Diagnostic>,
}

/// Discover and parse all .glyph files in a directory tree
pub fn discover_and_parse_modules(root: &Path) -> Result<ModuleLoadResult> {
    let mut modules = HashMap::new();
    let mut sources = HashMap::new();
    let mut diagnostics = Vec::new();

    for entry in WalkDir::new(root).follow_links(true) {
        let entry = entry?;
        let path = entry.path();

        // Skip non-.glyph files
        if path.extension().and_then(|s| s.to_str()) != Some("glyph") {
            continue;
        }

        // Read and parse the file
        let source = fs::read_to_string(path)?;
        let module_id = module_id_from_path(root, path)?;
        sources.insert(
            module_id.clone(),
            SourceInfo::new(path.to_path_buf(), source.clone()),
        );
        let lex_out = lex(&source);
        if !lex_out.diagnostics.is_empty() {
            diagnostics.extend(lex_out.diagnostics.into_iter().map(|diag| {
                if diag.module_id.is_some() {
                    diag
                } else {
                    diag.with_module_id(module_id.clone())
                }
            }));
            return Ok(ModuleLoadResult {
                modules,
                sources,
                diagnostics,
            });
        }

        let parse_out = parse(&lex_out.tokens, &source);
        if !parse_out.diagnostics.is_empty() {
            diagnostics.extend(parse_out.diagnostics.into_iter().map(|diag| {
                if diag.module_id.is_some() {
                    diag
                } else {
                    diag.with_module_id(module_id.clone())
                }
            }));
            return Ok(ModuleLoadResult {
                modules,
                sources,
                diagnostics,
            });
        }
        modules.insert(module_id, parse_out.module);
    }

    Ok(ModuleLoadResult {
        modules,
        sources,
        diagnostics,
    })
}

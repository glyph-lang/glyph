use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Result, anyhow};
use clap::{Parser, Subcommand, ValueEnum};
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_core::ast::Module;
use glyph_frontend::{
    FrontendOptions, ResolverContext, compile_source, lex, parse, resolve_multi_module,
    resolve_types,
};
use walkdir::WalkDir;

#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;
#[cfg(feature = "codegen")]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[derive(Parser, Debug)]
#[command(name = "glyph", version, about = "Glyph language toolchain")]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Parse and type-check source
    Check { path: PathBuf },
    /// Build source and emit code
    Build {
        path: PathBuf,
        #[arg(long, value_enum, default_value_t = EmitTarget::Ll)]
        emit: EmitTarget,
        /// Link against a library (can be specified multiple times)
        #[arg(long = "link-lib")]
        link_lib: Vec<String>,
        /// Add a library search path (can be specified multiple times)
        #[arg(long = "link-search")]
        link_search: Vec<PathBuf>,
    },
    /// Build and execute (stub)
    Run { path: PathBuf },
}

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum EmitTarget {
    Ll,
    Obj,
    Exe,
}

impl From<EmitTarget> for EmitKind {
    fn from(value: EmitTarget) -> Self {
        match value {
            EmitTarget::Ll => EmitKind::LlvmIr,
            EmitTarget::Obj => EmitKind::Object,
            EmitTarget::Exe => EmitKind::Executable,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Check { path } => check(&path),
        Commands::Build {
            path,
            emit,
            link_lib,
            link_search,
        } => build(&path, emit, link_lib, link_search),
        Commands::Run { path } => run(&path),
    }
}

fn load_source(path: &PathBuf) -> Result<String> {
    Ok(fs::read_to_string(path)?)
}

/// Discover and parse all .glyph files in a directory tree
fn discover_and_parse_modules(root: &Path) -> Result<HashMap<String, Module>> {
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

        // Generate module ID from relative path
        // e.g., "src/math/geometry.glyph" -> "src/math/geometry"
        let rel_path = path.strip_prefix(root).unwrap_or(path);
        let module_id = rel_path
            .with_extension("")
            .to_str()
            .ok_or_else(|| anyhow!("invalid path: {:?}", rel_path))?
            .replace(std::path::MAIN_SEPARATOR, "/");

        modules.insert(module_id, parse_out.module);
    }

    Ok(modules)
}

fn check(path: &PathBuf) -> Result<()> {
    // Determine project root (parent of the input file)
    let project_root = path.parent().unwrap_or(Path::new("."));

    // Discover and parse all .glyph files in the project
    let modules = discover_and_parse_modules(project_root)?;

    if modules.is_empty() {
        return Err(anyhow!("no .glyph files found in project"));
    }

    // Multi-module resolution
    let (multi_ctx, compile_order) =
        resolve_multi_module(modules, project_root).map_err(|diags| {
            for diag in &diags {
                eprintln!("{:?}", diag);
            }
            anyhow!("multi-module resolution failed")
        })?;

    // Type-check each module in dependency order
    let mut all_diagnostics = Vec::new();

    for module_id in &compile_order {
        let module = &multi_ctx.modules[module_id];
        let import_scope = &multi_ctx.import_scopes[module_id];

        // Create resolver context with module info
        let mut resolver_ctx = ResolverContext::default();
        resolver_ctx.current_module = Some(module_id.clone());
        resolver_ctx.import_scope = Some(import_scope.clone());
        resolver_ctx.all_modules = Some(multi_ctx.clone());

        // Resolve types for this module
        let (_, diags) = resolve_types(module);
        all_diagnostics.extend(diags);
    }

    // Report results
    if all_diagnostics.is_empty() {
        println!("check ok: {} module(s), 0 diagnostics", compile_order.len());
        Ok(())
    } else {
        for diag in &all_diagnostics {
            eprintln!("{:?}", diag);
        }
        Err(anyhow!(
            "check failed with {} diagnostic(s)",
            all_diagnostics.len()
        ))
    }
}

fn build(
    path: &PathBuf,
    emit: EmitTarget,
    link_lib: Vec<String>,
    link_search: Vec<PathBuf>,
) -> Result<()> {
    let source = load_source(path)?;
    let output = compile_source(
        &source,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );
    if !output.diagnostics.is_empty() {
        for diag in output.diagnostics {
            eprintln!("{:?}", diag);
        }
        return Err(anyhow!("build failed"));
    }

    // Determine output file names based on input path
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow!("Invalid input file path"))?;

    match emit {
        EmitTarget::Ll => {
            // Just emit LLVM IR to stdout (existing behavior)
            #[cfg(feature = "codegen")]
            {
                let backend = LlvmBackend::default();
                let opts = CodegenOptions {
                    emit: EmitKind::LlvmIr,
                    link_libs: link_lib,
                    link_search_paths: link_search,
                    ..Default::default()
                };
                let artifact = backend.emit(&output.mir, &opts)?;
                if let Some(ir) = artifact.llvm_ir {
                    println!("{}", ir);
                }
            }
            #[cfg(not(feature = "codegen"))]
            {
                let backend = NullBackend::default();
                let opts = CodegenOptions::default();
                let artifact = backend.emit(&output.mir, &opts)?;
                if let Some(ir) = artifact.llvm_ir {
                    println!("{}", ir);
                }
            }
        }
        EmitTarget::Obj => {
            // Generate object file
            #[cfg(feature = "codegen")]
            {
                let obj_path = PathBuf::from(format!("{}.o", stem));
                let mut ctx = CodegenContext::new("glyph_module")?;
                ctx.codegen_module(&output.mir)?;
                ctx.emit_object_file(&obj_path)?;
                println!("Object file written to: {}", obj_path.display());
            }
            #[cfg(not(feature = "codegen"))]
            {
                return Err(anyhow!(
                    "Object file emission requires the 'codegen' feature"
                ));
            }
        }
        EmitTarget::Exe => {
            // Generate object file, then link to executable
            #[cfg(feature = "codegen")]
            {
                let obj_path = PathBuf::from(format!("{}.o", stem));
                let exe_path = PathBuf::from(stem);

                // Generate object file using LLVM
                let mut ctx = CodegenContext::new("glyph_module")?;
                ctx.codegen_module(&output.mir)?;
                ctx.emit_object_file(&obj_path)?;

                // Link to executable
                let linker = Linker::new();
                let runtime_lib = Linker::get_runtime_lib_path();

                let linker_opts = LinkerOptions {
                    output_path: exe_path.clone(),
                    object_files: vec![obj_path.clone()],
                    link_libs: link_lib,
                    link_search_paths: link_search,
                    runtime_lib_path: runtime_lib,
                };

                linker.link(&linker_opts)?;

                // Clean up intermediate object file
                let _ = std::fs::remove_file(obj_path);

                println!("Executable written to: {}", exe_path.display());
            }
            #[cfg(not(feature = "codegen"))]
            {
                return Err(anyhow!(
                    "Executable generation requires the 'codegen' feature"
                ));
            }
        }
    }

    Ok(())
}

fn run(path: &PathBuf) -> Result<()> {
    // Build the executable
    build(path, EmitTarget::Exe, Vec::new(), Vec::new())?;

    // Determine the executable name from the source file
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow!("Invalid input file path"))?;
    let exe_path = PathBuf::from(stem);

    // Execute the program
    let status = Command::new(format!("./{}", exe_path.display()))
        .status()
        .map_err(|e| anyhow!("Failed to execute program: {}", e))?;

    // Check exit status
    if !status.success() {
        let code = status.code().unwrap_or(-1);
        return Err(anyhow!("Program exited with status code: {}", code));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_help_succeeds() {
        let err = Cli::try_parse_from(["glyph", "--help"]).unwrap_err();
        assert_eq!(err.kind(), clap::error::ErrorKind::DisplayHelp);
    }

    #[test]
    fn emit_target_maps_to_emit_kind() {
        assert!(matches!(EmitKind::from(EmitTarget::Ll), EmitKind::LlvmIr));
        assert!(matches!(EmitKind::from(EmitTarget::Obj), EmitKind::Object));
        assert!(matches!(
            EmitKind::from(EmitTarget::Exe),
            EmitKind::Executable
        ));
    }

    #[test]
    fn discover_modules_finds_all_files() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create test files
        fs::write(root.join("main.glyph"), "fn main() -> i32 { ret 0 }").unwrap();
        fs::write(root.join("utils.glyph"), "fn helper() -> i32 { ret 1 }").unwrap();

        let math_dir = root.join("math");
        fs::create_dir(&math_dir).unwrap();
        fs::write(math_dir.join("geometry.glyph"), "struct Point { x: i32 }").unwrap();

        // Discover modules
        let modules = discover_and_parse_modules(root).unwrap();

        // Verify we found all 3 files
        assert_eq!(modules.len(), 3);
        assert!(modules.contains_key("main"));
        assert!(modules.contains_key("utils"));
        assert!(modules.contains_key("math/geometry"));
    }
}

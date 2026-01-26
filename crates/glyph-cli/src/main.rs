use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand, ValueEnum};
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{
    compile_modules, resolve_multi_module, resolve_types, FrontendOptions, ResolverContext,
};

mod module_loader;
use module_loader::{discover_and_parse_modules, module_id_from_path};

mod build_version;

#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;
#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[derive(Parser, Debug)]
#[command(
    name = "glyph",
    version = crate::build_version::BUILD_VERSION,
    about = "Glyph language toolchain"
)]
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

fn check(path: &PathBuf) -> Result<()> {
    // Determine project root (parent of the input file)
    let project_root = path.parent().unwrap_or(Path::new("."));
    let entry_module = module_id_from_path(project_root, path)?;

    // Discover and parse all .glyph files in the project
    let modules = discover_and_parse_modules(project_root)?;

    if modules.is_empty() {
        return Err(anyhow!("no .glyph files found in project"));
    }

    // Multi-module resolution
    let (multi_ctx, compile_order) = resolve_multi_module(modules, &entry_module, project_root)
        .map_err(|diags| {
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
    let project_root = path.parent().unwrap_or(Path::new("."));
    let entry_module = module_id_from_path(project_root, path)?;
    let modules = discover_and_parse_modules(project_root)?;
    let output = compile_modules(
        modules,
        &entry_module,
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
    let project_root = path.parent().unwrap_or(Path::new("."));
    let entry_module = module_id_from_path(project_root, path)?;
    let modules = discover_and_parse_modules(project_root)?;
    let output = compile_modules(
        modules,
        &entry_module,
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

    #[cfg(feature = "codegen")]
    {
        // Prefer JIT execution for `run` to avoid platform-specific AOT linker/toolchain
        // issues and to keep the feedback loop fast.
        let mut ctx = CodegenContext::new("glyph_module")?;
        ctx.codegen_module(&output.mir)?;

        // Provide runtime symbols that are normally supplied by the AOT runtime library.
        // When running via JIT, we need to make them available in-process.
        let mut symbols = HashMap::new();
        symbols.insert("glyph_byte_at".to_string(), glyph_byte_at as usize as u64);
        symbols.insert(
            "glyph_process_run".to_string(),
            glyph_process_run as usize as u64,
        );

        let exit = ctx.jit_execute_i32_with_symbols("main", &symbols)?;
        if exit != 0 {
            return Err(anyhow!("Program exited with status code: {}", exit));
        }
        Ok(())
    }

    #[cfg(not(feature = "codegen"))]
    {
        let _ = output;
        Err(anyhow!("running programs requires the 'codegen' feature"))
    }
}

// Runtime helper used by std/string::byte_at (link_name = "glyph_byte_at").
// This mirrors runtime/glyph_json.c so JIT execution can resolve the symbol.
#[cfg(feature = "codegen")]
#[unsafe(no_mangle)]
pub extern "C" fn glyph_byte_at(s: *const std::ffi::c_char, index: usize) -> u8 {
    if s.is_null() {
        return 0;
    }
    unsafe {
        let mut p = s as *const u8;
        let mut i: usize = 0;
        loop {
            let b = *p;
            if b == 0 {
                return 0;
            }
            if i == index {
                return b;
            }
            i += 1;
            p = p.add(1);
        }
    }
}

#[cfg(feature = "codegen")]
#[repr(C)]
pub struct GlyphVec {
    pub data: *mut std::ffi::c_void,
    pub len: i64,
    pub cap: i64,
}

// Runtime helper used by std/process::run (link_name = "glyph_process_run").
// The AOT path provides this via runtime/glyph_process.c; the JIT path needs an
// in-process implementation.
#[cfg(all(feature = "codegen", unix))]
#[unsafe(no_mangle)]
pub extern "C" fn glyph_process_run(cmd: *const std::ffi::c_char, args: GlyphVec) -> i32 {
    use std::ffi::CStr;
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::process::ExitStatusExt;
    use std::process::Command;

    if cmd.is_null() {
        return -(libc::EINVAL as i32);
    }
    if args.len < 0 {
        return -(libc::EINVAL as i32);
    }

    let cmd_bytes = unsafe { CStr::from_ptr(cmd) }.to_bytes();
    let cmd_os = std::ffi::OsStr::from_bytes(cmd_bytes);

    let mut child = Command::new(cmd_os);

    let arg_ptrs = args.data as *const *const std::ffi::c_char;
    for i in 0..args.len {
        let p = unsafe { *arg_ptrs.offset(i as isize) };
        if p.is_null() {
            continue;
        }
        let bytes = unsafe { CStr::from_ptr(p) }.to_bytes();
        child.arg(std::ffi::OsStr::from_bytes(bytes));
    }

    match child.status() {
        Ok(status) => {
            if let Some(code) = status.code() {
                code as i32
            } else if let Some(sig) = status.signal() {
                128 + sig
            } else {
                -(libc::EINVAL as i32)
            }
        }
        Err(e) => -(e.raw_os_error().unwrap_or(1) as i32),
    }
}

#[cfg(all(feature = "codegen", not(unix)))]
#[unsafe(no_mangle)]
pub extern "C" fn glyph_process_run(_cmd: *const std::ffi::c_char, _args: GlyphVec) -> i32 {
    -(libc::ENOSYS as i32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

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

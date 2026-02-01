use std::collections::HashSet;
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command as ProcessCommand;

use clap::{Parser, Subcommand};
use serde::Deserialize;

mod build_version;

#[cfg(feature = "codegen")]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};
use glyph_frontend::{compile_modules, FrontendOptions};

mod module_loader;
use module_loader::{discover_and_parse_modules, module_id_from_path};

mod diagnostics;
use diagnostics::format_diagnostic;

const EXIT_SUCCESS: i32 = 0;
const EXIT_USAGE: i32 = 1;
const EXIT_COMPILER: i32 = 2;
const EXIT_INTERNAL: i32 = 3;

#[derive(Parser, Debug)]
#[command(
    name = "glyph",
    version = crate::build_version::BUILD_VERSION,
    about = crate::build_version::WORKFLOW_ABOUT
)]
struct Cli {
    #[command(subcommand)]
    command: GlyphCommand,
}

#[derive(Subcommand, Debug)]
enum GlyphCommand {
    /// Initialize a new Glyph project
    Init {
        /// Project name (defaults to directory name)
        name: Option<String>,
    },
    /// Build binaries described in glyph.toml
    Build {
        /// Build the release profile
        #[arg(long)]
        release: bool,
        /// Build only this binary
        #[arg(long)]
        bin: Option<String>,
        /// Emit extra logging
        #[arg(short, long)]
        verbose: bool,
    },
    /// Build then run a binary with optional args
    Run {
        #[arg(long)]
        release: bool,
        #[arg(long)]
        bin: Option<String>,
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
        #[arg(short, long)]
        verbose: bool,
    },
}

#[derive(Debug)]
struct GlyphError {
    code: i32,
    message: String,
}

type GlyphResult<T> = Result<T, GlyphError>;

#[derive(Debug, Deserialize)]
struct PackageSection {
    name: String,
    version: String,
}

#[derive(Debug, Deserialize)]
struct BinTarget {
    name: String,
    path: String,
}

#[derive(Debug, Deserialize)]
struct Manifest {
    package: PackageSection,
    #[serde(default)]
    bin: Vec<BinTarget>,
    #[serde(default)]
    #[allow(dead_code)]
    dependencies: toml::value::Table,
}

fn main() {
    let exit = match run() {
        Ok(()) => EXIT_SUCCESS,
        Err(err) => {
            eprintln!("{}", err.message);
            err.code
        }
    };
    std::process::exit(exit);
}

fn run() -> GlyphResult<()> {
    let cli = Cli::parse();
    match cli.command {
        GlyphCommand::Init { name } => init_cmd(name),
        GlyphCommand::Build {
            release,
            bin,
            verbose,
        } => {
            let _ = build_cmd(release, bin, verbose)?;
            Ok(())
        }
        GlyphCommand::Run {
            release,
            bin,
            args,
            verbose,
        } => run_cmd(release, bin, args, verbose),
    }
}

fn init_cmd(name: Option<String>) -> GlyphResult<()> {
    let cwd = std::env::current_dir().map_err(internal_err("failed to read current dir"))?;
    let manifest_path = cwd.join("glyph.toml");

    // Check if glyph.toml already exists
    if manifest_path.exists() {
        return Err(usage_err("glyph.toml already exists in current directory"));
    }

    // Get project name from argument or directory name
    let project_name = if let Some(name) = name {
        name
    } else {
        cwd.file_name()
            .and_then(|n| n.to_str())
            .ok_or_else(|| usage_err("could not determine project name from directory"))?
            .to_string()
    };

    // Create glyph.toml
    let toml_content = format!(
        r#"[package]
name = "{}"
version = "0.1.0"

[dependencies]
# Add dependencies here

[[bin]]
name = "{}"
path = "src/main.glyph"
"#,
        project_name, project_name
    );

    fs::write(&manifest_path, toml_content).map_err(internal_err("failed to write glyph.toml"))?;

    println!("Created glyph.toml");

    // Create src directory
    let src_dir = cwd.join("src");
    fs::create_dir_all(&src_dir).map_err(internal_err("failed to create src/ directory"))?;

    // Create src/main.glyph
    let main_glyph_content = r#"from std import println

fn main() -> i32 {
  println("Hello, Glyph!")
  ret 0
}
"#;

    let main_path = src_dir.join("main.glyph");
    fs::write(&main_path, main_glyph_content)
        .map_err(internal_err("failed to write src/main.glyph"))?;

    println!("Created src/main.glyph");
    println!("\nProject '{}' initialized!", project_name);
    println!("\nNext steps:");
    println!("  glyph build");
    println!("  glyph run");

    Ok(())
}

fn build_cmd(release: bool, bin: Option<String>, verbose: bool) -> GlyphResult<PathBuf> {
    let cwd = std::env::current_dir().map_err(internal_err("failed to read current dir"))?;
    let manifest_path = locate_manifest(&cwd)?;
    let root = manifest_path
        .parent()
        .ok_or_else(|| usage_err("glyph.toml has no parent directory"))?;

    let manifest = load_manifest(&manifest_path, root)?;
    let target = select_bin(&manifest, bin.as_deref())?;
    let profile = if release { "release" } else { "debug" };

    let built_path = build_bin(root, &target, profile, verbose)?;
    println!(
        "Built {} ({}) -> {}",
        target.name,
        profile,
        built_path.display()
    );
    Ok(built_path)
}

fn run_cmd(
    release: bool,
    bin: Option<String>,
    args: Vec<String>,
    verbose: bool,
) -> GlyphResult<()> {
    let exe = build_cmd(release, bin, verbose)?;
    let status = ProcessCommand::new(&exe)
        .args(args)
        .status()
        .map_err(internal_err("failed to spawn binary"))?;
    if !status.success() {
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: format!("program exited with status {:?}", status.code()),
        });
    }
    Ok(())
}

fn locate_manifest(start: &Path) -> GlyphResult<PathBuf> {
    for ancestor in start.ancestors() {
        let candidate = ancestor.join("glyph.toml");
        if candidate.is_file() {
            return Ok(candidate);
        }
    }
    Err(GlyphError {
        code: EXIT_USAGE,
        message: "glyph.toml not found (searching current dir and parents)".to_string(),
    })
}

fn load_manifest(path: &Path, root: &Path) -> GlyphResult<Manifest> {
    let contents = fs::read_to_string(path)
        .map_err(|e| usage_err(format!("failed to read {}: {}", path.display(), e)))?;
    let manifest: Manifest =
        toml::from_str(&contents).map_err(|e| usage_err(format!("invalid glyph.toml: {}", e)))?;
    validate_manifest(&manifest, root)?;
    Ok(manifest)
}

fn validate_manifest(manifest: &Manifest, root: &Path) -> GlyphResult<()> {
    if manifest.package.name.trim().is_empty() {
        return Err(usage_err("[package].name is required"));
    }
    if manifest.package.version.trim().is_empty() {
        return Err(usage_err("[package].version is required"));
    }
    if manifest.bin.is_empty() {
        return Err(usage_err("at least one [[bin]] entry is required"));
    }

    let mut seen = HashSet::new();
    for bin in &manifest.bin {
        if bin.name.trim().is_empty() {
            return Err(usage_err("[[bin]].name is required"));
        }
        if !seen.insert(&bin.name) {
            return Err(usage_err(format!("duplicate [[bin]] name '{}'", bin.name)));
        }
        if bin.path.trim().is_empty() {
            return Err(usage_err(format!(
                "[[bin]] '{}' must specify path",
                bin.name
            )));
        }
        let path = Path::new(&bin.path);
        if path.is_absolute() {
            return Err(usage_err(format!(
                "[[bin]].path must be relative: '{}'",
                bin.path
            )));
        }
        if path.components().any(|c| matches!(c, Component::ParentDir)) {
            return Err(usage_err(format!(
                "[[bin]].path must not traverse parents ('{}')",
                bin.path
            )));
        }
        if path.extension().and_then(|s| s.to_str()) != Some("glyph") {
            return Err(usage_err(format!(
                "[[bin]].path must end with .glyph ('{}')",
                bin.path
            )));
        }

        let full = root.join(path);
        if !full.is_file() {
            return Err(usage_err(format!(
                "bin path does not exist: {}",
                full.display()
            )));
        }
    }

    Ok(())
}

fn select_bin<'a>(manifest: &'a Manifest, requested: Option<&str>) -> GlyphResult<&'a BinTarget> {
    if let Some(name) = requested {
        manifest.bin.iter().find(|b| b.name == name).ok_or_else(|| {
            let names: Vec<_> = manifest.bin.iter().map(|b| b.name.as_str()).collect();
            usage_err(format!(
                "bin '{}' not found. available: {}",
                name,
                names.join(", ")
            ))
        })
    } else if manifest.bin.len() == 1 {
        Ok(&manifest.bin[0])
    } else {
        let names: Vec<_> = manifest.bin.iter().map(|b| b.name.as_str()).collect();
        Err(usage_err(format!(
            "multiple bins defined; select one with --bin <name> (available: {})",
            names.join(", ")
        )))
    }
}

fn build_bin(root: &Path, bin: &BinTarget, profile: &str, verbose: bool) -> GlyphResult<PathBuf> {
    let source_path = root.join(&bin.path);
    let module_root = source_path.parent().unwrap_or(root);
    let output_dir = root.join("target").join(profile);
    fs::create_dir_all(&output_dir).map_err(internal_err(format!(
        "failed to create {}",
        output_dir.display()
    )))?;

    let entry_module = module_id_from_path(module_root, &source_path).map_err(|e| GlyphError {
        code: EXIT_COMPILER,
        message: format!("failed to determine entry module: {}", e),
    })?;

    let load = discover_and_parse_modules(module_root).map_err(|e| GlyphError {
        code: EXIT_COMPILER,
        message: format!("failed to load modules: {}", e),
    })?;
    if !load.diagnostics.is_empty() {
        for diag in &load.diagnostics {
            eprintln!("{}", format_diagnostic(diag, &load.sources));
        }
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: "build failed due to compiler diagnostics".to_string(),
        });
    }
    let modules = load.modules;
    let sources = load.sources;

    let frontend_output = compile_modules(
        modules,
        &entry_module,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );

    if !frontend_output.diagnostics.is_empty() {
        for diag in &frontend_output.diagnostics {
            eprintln!("{}", format_diagnostic(diag, &sources));
        }
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: "build failed due to compiler diagnostics".to_string(),
        });
    }

    #[cfg(not(feature = "codegen"))]
    {
        let _ = verbose;
        return Err(GlyphError {
            code: EXIT_INTERNAL,
            message: "building executables requires the 'codegen' feature".to_string(),
        });
    }

    #[cfg(feature = "codegen")]
    {
        let obj_path = output_dir.join(format!("{}.o", bin.name));
        let exe_path = output_dir.join(&bin.name);

        if verbose {
            eprintln!("[glyph] writing object: {}", obj_path.display());
        }

        let mut ctx = CodegenContext::new("glyph_module")
            .map_err(internal_err("failed to create codegen context"))?;
        ctx.codegen_module(&frontend_output.mir)
            .map_err(internal_err("codegen failed"))?;
        ctx.emit_object_file(&obj_path)
            .map_err(internal_err("failed to emit object file"))?;

        let linker = Linker::new();
        let runtime_lib = Linker::get_runtime_lib_path();
        let opts = LinkerOptions {
            output_path: exe_path.clone(),
            object_files: vec![obj_path.clone()],
            link_libs: Vec::new(),
            link_search_paths: Vec::new(),
            runtime_lib_path: runtime_lib,
        };

        if verbose {
            eprintln!("[glyph] linking to {}", exe_path.display());
        }

        linker.link(&opts).map_err(internal_err("linking failed"))?;
        let _ = fs::remove_file(&obj_path);
        Ok(exe_path)
    }
}

fn usage_err<T: Into<String>>(msg: T) -> GlyphError {
    GlyphError {
        code: EXIT_USAGE,
        message: msg.into(),
    }
}

fn internal_err<T: Into<String>, E: std::fmt::Display>(msg: T) -> impl FnOnce(E) -> GlyphError {
    let message = msg.into();
    move |e| GlyphError {
        code: EXIT_INTERNAL,
        message: format!("{}: {}", message, e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn select_bin_requires_flag_when_multiple() {
        let manifest = Manifest {
            package: PackageSection {
                name: "foo".into(),
                version: "0.1.0".into(),
            },
            bin: vec![
                BinTarget {
                    name: "a".into(),
                    path: "a.glyph".into(),
                },
                BinTarget {
                    name: "b".into(),
                    path: "b.glyph".into(),
                },
            ],
            dependencies: toml::value::Table::new(),
        };
        let err = select_bin(&manifest, None).unwrap_err();
        assert_eq!(err.code, EXIT_USAGE);
    }

    #[test]
    fn validate_manifest_rejects_parent_dirs() {
        let temp = TempDir::new().unwrap();
        let root = temp.path();
        let manifest = Manifest {
            package: PackageSection {
                name: "foo".into(),
                version: "0.1.0".into(),
            },
            bin: vec![BinTarget {
                name: "a".into(),
                path: "../a.glyph".into(),
            }],
            dependencies: toml::value::Table::new(),
        };
        // Need the file to exist to reach traversal check; create placeholder in root/.. not possible
        // Instead, expect traversal rejection before file existence is checked.
        let err = validate_manifest(&manifest, root).unwrap_err();
        assert_eq!(err.code, EXIT_USAGE);
    }
}

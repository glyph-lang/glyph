use std::fs;
use std::path::PathBuf;

use anyhow::{Result, anyhow};
use clap::{Parser, Subcommand, ValueEnum};
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;

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
        Commands::Build { path, emit } => build(&path, emit),
        Commands::Run { path } => run(&path),
    }
}

fn load_source(path: &PathBuf) -> Result<String> {
    Ok(fs::read_to_string(path)?)
}

fn check(path: &PathBuf) -> Result<()> {
    let source = load_source(path)?;
    let output = compile_source(&source, FrontendOptions { emit_mir: false });
    if output.diagnostics.is_empty() {
        println!("check ok: {} diagnostics", output.diagnostics.len());
        Ok(())
    } else {
        for diag in output.diagnostics {
            eprintln!("{:?}", diag);
        }
        Err(anyhow!("check failed"))
    }
}

fn build(path: &PathBuf, emit: EmitTarget) -> Result<()> {
    let source = load_source(path)?;
    let output = compile_source(&source, FrontendOptions { emit_mir: true });
    if !output.diagnostics.is_empty() {
        for diag in output.diagnostics {
            eprintln!("{:?}", diag);
        }
        return Err(anyhow!("build failed"));
    }

    #[cfg(feature = "codegen")]
    let backend = LlvmBackend::default();
    #[cfg(not(feature = "codegen"))]
    let backend = NullBackend::default();

    let opts = CodegenOptions {
        emit: emit.into(),
        ..Default::default()
    };
    let artifact = backend.emit(&output.mir, &opts)?;
    if let Some(ir) = artifact.llvm_ir {
        println!("{}", ir);
    }
    Ok(())
}

fn run(path: &PathBuf) -> Result<()> {
    build(path, EmitTarget::Exe)?;
    println!("run stub: execution not implemented yet");
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
}

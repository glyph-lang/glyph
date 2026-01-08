use anyhow::Result;
use glyph_core::mir::MirModule;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EmitKind {
    LlvmIr,
    Object,
    Executable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenOptions {
    pub emit: EmitKind,
    pub target: Option<String>,
    pub opt_level: u8,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            emit: EmitKind::LlvmIr,
            target: None,
            opt_level: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BackendArtifact {
    pub llvm_ir: Option<String>,
}

pub trait Backend {
    fn emit(&self, module: &MirModule, opts: &CodegenOptions) -> Result<BackendArtifact>;
}

#[derive(Debug, Default, Clone)]
pub struct NullBackend;

impl Backend for NullBackend {
    fn emit(&self, module: &MirModule, _opts: &CodegenOptions) -> Result<BackendArtifact> {
        Ok(BackendArtifact {
            llvm_ir: Some(format!(
                "; placeholder for {} functions",
                module.functions.len()
            )),
        })
    }
}

#[cfg(feature = "codegen")]
pub mod codegen;

#[cfg(feature = "codegen")]
pub mod llvm {
    use super::codegen::CodegenContext;
    use super::{Backend, BackendArtifact, CodegenOptions};
    use anyhow::Result;
    use glyph_core::mir::MirModule;

    #[derive(Debug, Default, Clone)]
    pub struct LlvmBackend;

    impl Backend for LlvmBackend {
        fn emit(&self, module: &MirModule, _opts: &CodegenOptions) -> Result<BackendArtifact> {
            let mut ctx = CodegenContext::new("glyph_module")?;
            ctx.codegen_module(module)?;
            let ir = ctx.dump_ir();
            Ok(BackendArtifact { llvm_ir: Some(ir) })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn null_backend_returns_ir_string() {
        let backend = NullBackend::default();
        let module = MirModule { functions: vec![] };
        let artifact = backend.emit(&module, &CodegenOptions::default()).unwrap();
        assert!(artifact.llvm_ir.unwrap().contains("placeholder"));
    }
}

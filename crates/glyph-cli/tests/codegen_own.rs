#![cfg(feature = "codegen")]

use std::fs;
use std::path::Path;

use glyph_backend::llvm::LlvmBackend;
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

fn load_fixture(name: &str) -> String {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests/fixtures/codegen");
    let path = root.join(name);
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("fixture read {}: {}", path.display(), e))
}

fn compile_ir(name: &str) -> String {
    let src = load_fixture(name);
    let out = compile_source(
        &src,
        FrontendOptions {
            emit_mir: true,
            include_std: false,
        },
    );
    assert!(
        out.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        out.diagnostics
    );

    let backend = LlvmBackend::default();
    let artifact = backend
        .emit(
            &out.mir,
            &CodegenOptions {
                emit: EmitKind::LlvmIr,
                ..Default::default()
            },
        )
        .expect("backend emit");
    artifact.llvm_ir.expect("llvm ir")
}

#[test]
fn codegen_own_basic() {
    let ir = compile_ir("own_basic.glyph");
    assert!(
        ir.contains("call ptr @malloc"),
        "IR missing malloc call\n{ir}"
    );
    assert!(ir.contains("call void @free"), "IR missing free call\n{ir}");
}

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
            include_std: true,
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
fn codegen_string_utils() {
    let ir = compile_ir("string_utils.glyph");
    assert!(ir.contains("strlen"), "IR missing strlen: {}", ir);
    assert!(ir.contains("memcpy"), "IR missing memcpy: {}", ir);
    assert!(ir.contains("memcmp"), "IR missing memcmp: {}", ir);
    assert!(ir.contains("strstr"), "IR missing strstr: {}", ir);
    assert!(ir.contains("isspace"), "IR missing isspace: {}", ir);
}

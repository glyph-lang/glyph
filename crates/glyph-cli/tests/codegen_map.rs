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
fn codegen_map_basic_ops() {
    let ir = compile_ir("map_basic.glyph");
    assert!(
        ir.contains("map.add.result"),
        "IR missing add result: {}",
        ir
    );
    assert!(
        ir.contains("map.get.result"),
        "IR missing get result: {}",
        ir
    );
    assert!(
        ir.contains("map.del.result"),
        "IR missing del result: {}",
        ir
    );
    assert!(ir.contains("map.keys.loop"), "IR missing keys loop: {}", ir);
    assert!(ir.contains("map.vals.loop"), "IR missing vals loop: {}", ir);
}

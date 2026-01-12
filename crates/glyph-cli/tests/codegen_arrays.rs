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

    if std::env::var("GLYPH_DEBUG_MIR").is_ok() {
        eprintln!("{:#?}", out.mir);
    }

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
fn codegen_array_literal_index() {
    let ir = compile_ir("array_basic.glyph");
    assert!(ir.contains("[3 x i32]"), "IR missing array type");
    assert!(
        ir.contains("getelementptr inbounds"),
        "IR missing GEP for array element"
    );
    assert!(
        ir.contains("call void @llvm.trap"),
        "IR missing trap intrinsic:\n{}",
        ir
    );
}

#[test]
fn codegen_array_loop_bounds_check() {
    let ir = compile_ir("array_loop.glyph");
    assert!(ir.contains("[4 x i32]"), "IR missing loop array type");
    assert!(
        ir.contains("getelementptr inbounds"),
        "IR missing loop element GEP"
    );
    assert!(
        ir.contains("call void @llvm.trap"),
        "IR missing trap intrinsic for bounds check:\n{}",
        ir
    );
}

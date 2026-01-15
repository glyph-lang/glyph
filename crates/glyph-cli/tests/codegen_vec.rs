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
fn codegen_vec_push_pop_len() {
    let ir = compile_ir("vec_push_pop.glyph");
    assert!(ir.contains("vec.len.load"), "IR missing len load: {}", ir);
    assert!(
        ir.contains("vec.cap.new") || ir.contains("vec.cap.double"),
        "IR missing growth path: {}",
        ir
    );
    assert!(
        ir.contains("vec.pop.phi"),
        "IR missing Option phi in pop: {}",
        ir
    );
}

#[test]
fn codegen_vec_bounds_trap() {
    let ir = compile_ir("vec_bounds.glyph");
    assert!(
        ir.contains("vec.elem.ptr"),
        "IR missing vec element GEP: {}",
        ir
    );
    assert!(
        ir.contains("llvm.trap"),
        "IR missing bounds trap for vec index: {}",
        ir
    );
}

#[test]
fn codegen_vec_drop_owns() {
    let ir = compile_ir("vec_drop_own.glyph");
    assert!(
        ir.contains("vec.drop"),
        "IR missing vec drop loop/block labels: {}",
        ir
    );
    assert!(
        ir.contains("free"),
        "IR missing buffer free in vec drop: {}",
        ir
    );
}

#[test]
fn codegen_vec_usize_len_cap() {
    let ir = compile_ir("vec_usize.glyph");
    assert!(ir.contains("vec.len.load"), "IR missing len load: {}", ir);
    assert!(ir.contains("vec.cap.load"), "IR missing cap load: {}", ir);
    assert!(ir.contains("i64"), "IR missing usize backend i64: {}", ir);
}

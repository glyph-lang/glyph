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
            include_std: true, // JSON requires stdlib
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
    let ir = artifact.llvm_ir.expect("llvm ir");
    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        eprintln!("{}", ir);
    }
    ir
}

#[test]
fn codegen_json_simple() {
    let ir = compile_ir("json_simple.glyph");

    // Verify the IR compiles successfully (JSON types infrastructure works)
    assert!(
        ir.contains("define"),
        "IR should contain function definitions"
    );
    assert!(ir.contains("main"), "IR should contain main function");
}

#[test]
fn codegen_json_primitives() {
    let ir = compile_ir("json_primitives.glyph");

    // Verify the IR compiles successfully with JSON primitive types
    assert!(
        ir.contains("define"),
        "IR should contain function definitions"
    );
    assert!(ir.contains("main"), "IR should contain main function");
}

#[test]
fn codegen_json_arrays() {
    let ir = compile_ir("json_arrays.glyph");

    // Verify the IR compiles successfully with JSON array types
    assert!(
        ir.contains("define"),
        "IR should contain function definitions"
    );
    assert!(ir.contains("main"), "IR should contain main function");
}

#[test]
fn codegen_json_objects() {
    let ir = compile_ir("json_objects.glyph");

    // Verify the IR compiles successfully with JSON object types
    assert!(
        ir.contains("define"),
        "IR should contain function definitions"
    );
    assert!(ir.contains("main"), "IR should contain main function");
}

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

#[test]
fn codegen_call_basic() {
    let src = load_fixture("call_basic.glyph");
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
    let ir = artifact.llvm_ir.expect("llvm ir");

    // Verify function definitions
    assert!(ir.contains("define i32 @add"));
    assert!(ir.contains("define i32 @main"));

    // Verify call instruction
    assert!(ir.contains("call i32 @add(i32 1, i32 2)"));
}

#[test]
fn codegen_call_recursive() {
    let src = load_fixture("factorial.glyph");
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
    let ir = artifact.llvm_ir.expect("llvm ir");

    // Verify factorial function
    assert!(ir.contains("define i32 @factorial"));

    // Verify recursive call
    assert!(ir.contains("call i32 @factorial"));

    // Verify main calls factorial
    assert!(ir.contains("call i32 @factorial(i32 5)"));
}

#[test]
fn codegen_call_struct_return() {
    let src = load_fixture("struct_return.glyph");
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
    let ir = artifact.llvm_ir.expect("llvm ir");

    // Verify struct type defined
    assert!(ir.contains("%Point = type { i32, i32 }"));

    // Verify function returns struct by value
    assert!(ir.contains("define %Point @make_point"));

    // Verify call returns struct
    assert!(ir.contains("call %Point @make_point(i32 10, i32 20)"));
}

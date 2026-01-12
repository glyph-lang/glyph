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
fn codegen_struct_basic() {
    let src = load_fixture("struct_basic.glyph");
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
    assert!(ir.contains("%Point = type { i32, i32 }"));
    assert!(ir.contains("getelementptr inbounds"));
}

#[test]
fn codegen_struct_field_access() {
    let src = load_fixture("struct_field_access.glyph");
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
    assert!(ir.contains("%Point = type { i32, i32 }"));
    assert!(ir.contains("getelementptr inbounds"));
    assert!(ir.contains("ret i32"));
}

#[test]
fn codegen_struct_field_sum() {
    let src = load_fixture("struct_field_sum.glyph");
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
    assert!(ir.contains("%Point = type { i32, i32 }"));
    assert!(ir.contains("getelementptr inbounds"));
    assert!(ir.contains("add"));
    assert!(ir.contains("ret i32"));
}

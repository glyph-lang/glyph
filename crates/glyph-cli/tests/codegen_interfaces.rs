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
fn codegen_interface_basic() {
    let src = load_fixture("interface_basic.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
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

    // Verify interface method definition with mangled name (LLVM quotes special chars)
    assert!(
        ir.contains(r#"define i32 @"Point::Drawable::draw""#),
        "Missing interface method definition"
    );

    // Verify main function exists
    assert!(ir.contains("define i32 @main"));

    // Verify interface method call
    assert!(
        ir.contains(r#"call i32 @"Point::Drawable::draw""#),
        "Missing interface method call"
    );

    // Verify receiver is passed as pointer (for &self)
    assert!(ir.contains("ptr %"), "Missing pointer receiver");
}

#[test]
fn codegen_interface_multi() {
    let src = load_fixture("interface_multi.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
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

    // Verify both interface method definitions (LLVM quotes special chars)
    assert!(
        ir.contains(r#"define i32 @"Point::Drawable::draw""#),
        "Missing Drawable::draw method"
    );
    assert!(
        ir.contains(r#"define i32 @"Point::Serializable::serialize""#),
        "Missing Serializable::serialize method"
    );

    // Verify both methods are called
    assert!(
        ir.contains(r#"call i32 @"Point::Drawable::draw""#),
        "Missing draw call"
    );
    assert!(
        ir.contains(r#"call i32 @"Point::Serializable::serialize""#),
        "Missing serialize call"
    );
}

#[test]
fn codegen_interface_mixed() {
    let src = load_fixture("interface_mixed.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
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

    // Verify inherent method (no interface name in mangling, LLVM quotes special chars)
    assert!(
        ir.contains(r#"define i32 @"Point::norm""#),
        "Missing inherent method Point::norm"
    );

    // Verify interface method (includes interface name)
    assert!(
        ir.contains(r#"define i32 @"Point::Drawable::draw""#),
        "Missing interface method Point::Drawable::draw"
    );

    // Verify both methods are called
    assert!(
        ir.contains(r#"call i32 @"Point::norm""#),
        "Missing norm call"
    );
    assert!(
        ir.contains(r#"call i32 @"Point::Drawable::draw""#),
        "Missing draw call"
    );
}

#[test]
fn codegen_interface_by_value() {
    let src = load_fixture("interface_by_value.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
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

    // Verify interface method definition (LLVM quotes special chars)
    assert!(
        ir.contains(r#"define %Point @"Point::Movable::move_by""#),
        "Missing interface method with by-value return"
    );

    // Verify struct type definition
    assert!(ir.contains("%Point = type { i32, i32 }"));

    // Verify method is called
    assert!(
        ir.contains(r#"call %Point @"Point::Movable::move_by""#),
        "Missing move_by call"
    );

    // Verify struct return by value
    assert!(
        ir.contains(r#"%Point @"Point::Movable::move_by""#),
        "Method should return struct by value"
    );
}

#[test]
fn codegen_interface_auto_borrow() {
    let src = load_fixture("interface_auto_borrow.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
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

    // Verify interface method definition (LLVM quotes special chars)
    assert!(
        ir.contains(r#"define i32 @"Point::Drawable::draw""#),
        "Missing interface method definition"
    );

    // Verify auto-borrowing: receiver should be passed as pointer
    assert!(
        ir.contains(r#"call i32 @"Point::Drawable::draw"(ptr"#),
        "Missing auto-borrow: receiver should be pointer"
    );

    // Verify Point is allocated on stack (alloca)
    assert!(
        ir.contains("alloca %Point"),
        "Point should be stack-allocated"
    );
}

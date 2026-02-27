#![cfg(feature = "codegen")]

use std::fs;
use std::path::Path;

use glyph_backend::llvm::LlvmBackend;
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{compile_source, FrontendOptions};

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

    let ir = artifact.llvm_ir.expect("llvm ir");
    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        eprintln!("{}", ir);
    }
    ir
}

// T1: Vec<String> drop IS emitted for direct Vec locals.
#[test]
fn ir_vec_string_drop_baseline() {
    let ir = compile_ir("vec_string_basic.glyph");
    assert!(
        ir.contains("vec.drop.body"),
        "Vec<String> local should emit vec.drop.body but doesn't: {}",
        ir
    );
    assert!(
        ir.contains("string.drop"),
        "Vec<String> drop loop should emit string.drop for elements but doesn't: {}",
        ir
    );
}

// T2: Struct String field is dropped via Named drop glue.
#[test]
fn ir_struct_with_string_no_drop() {
    let ir = compile_ir("struct_string_drop.glyph");
    // Struct's String field should be dropped via Named drop glue.
    assert!(
        ir.contains("string.drop"),
        "B1: struct Named {{ label: String, .. }} does not emit string.drop for its field: {}",
        ir
    );
}

// T3: Struct containing Vec<String> drops the Vec field.
#[test]
fn ir_struct_with_vec_field_no_drop() {
    let ir = compile_ir("struct_vec_field.glyph");
    let drop_count = ir.matches("vec.drop.body").count();
    assert!(
        drop_count >= 1,
        "B1: struct Container {{ items: Vec<String> }} has {} vec.drop.body occurrences (expected >= 1): {}",
        drop_count,
        ir
    );
}

// T4: Vec<Named> element drop recurses into struct fields.
#[test]
fn ir_vec_of_struct_elem_drop() {
    let ir = compile_ir("vec_struct_elem.glyph");
    assert!(
        ir.contains("vec.drop.body"),
        "IR missing vec.drop.body for Vec<Entry>: {}",
        ir
    );
    // Element drop loop should call string.drop for Entry.key.
    assert!(
        ir.contains("string.drop"),
        "B3: Vec<Entry> drop loop does not emit string.drop for Entry.key: {}",
        ir
    );
}

// T5: B2 — documents that field access uses LLVMBuildLoad2 (shallow copy).
// For scalars this is fine, but for composite types (Vec, String) it creates
// aliased data pointers — the root mechanism behind B2.
#[test]
fn ir_field_access_is_load() {
    let ir = compile_ir("struct_field_access.glyph");
    assert!(
        ir.contains("field.load"),
        "IR missing field.load for struct field access (B2 shallow copy mechanism): {}",
        ir
    );
}

// T6: Enum payload drop dispatch should be emitted when enum locals go out of scope.
#[test]
fn ir_enum_string_drop_dispatches() {
    let ir = compile_ir("enum_string_drop.glyph");
    assert!(
        ir.contains("enum.drop.variant"),
        "IR missing enum variant drop dispatch: {}",
        ir
    );
    assert!(
        ir.contains("string.drop"),
        "IR missing string.drop in enum payload drop path: {}",
        ir
    );
}

// T7: Map<String, String> drop should walk buckets and drop keys/values.
#[test]
fn ir_map_string_drop_emits_drop_path() {
    let ir = compile_ir("map_string_drop.glyph");
    assert!(
        ir.contains("map.drop.body"),
        "IR missing map.drop.body for Map drop glue: {}",
        ir
    );
    assert!(
        ir.contains("map.drop.inner.body"),
        "IR missing map bucket traversal drop loop: {}",
        ir
    );
    assert!(
        ir.contains("string.drop"),
        "IR missing string.drop for map key/value elements: {}",
        ir
    );
}

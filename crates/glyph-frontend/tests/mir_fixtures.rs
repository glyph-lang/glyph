use std::fs;
use std::path::Path;

use glyph_frontend::{FrontendOptions, compile_source};
use insta::assert_debug_snapshot;

fn run_fixture(name: &str) -> String {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests/fixtures/mir");
    let path = root.join(name);
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("fixture read {}: {}", path.display(), e))
}

#[test]
fn mir_ret_const() {
    let src = run_fixture("ret_const.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_let_ret() {
    let src = run_fixture("let_ret.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_binary_ret() {
    let src = run_fixture("binary_ret.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_if_else() {
    let src = run_fixture("if_else.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_if_no_else() {
    let src = run_fixture("if_no_else.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_if_nested() {
    let src = run_fixture("if_nested.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_call_placeholder() {
    let src = run_fixture("call_placeholder.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_compare_eq() {
    let src = run_fixture("compare_eq.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_logical_and() {
    let src = run_fixture("logical_and.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_logical_or() {
    let src = run_fixture("logical_or.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_fn_with_params() {
    let src = run_fixture("fn_with_params.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_implicit_return() {
    let src = run_fixture("implicit_return.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_struct_lit() {
    let src = run_fixture("struct_lit.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_field_access() {
    let src = run_fixture("field_access.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_struct_return() {
    let src = run_fixture("struct_return.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_ref_field_access() {
    let src = run_fixture("ref_field_access.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_ref_param_call() {
    let src = run_fixture("ref_param_call.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_call_basic() {
    let src = run_fixture("call_basic.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_call_recursive() {
    let src = run_fixture("call_recursive.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_while_simple() {
    let src = run_fixture("while_simple.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_while_break() {
    let src = run_fixture("while_break.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_while_continue() {
    let src = run_fixture("while_continue.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_while_nested() {
    let src = run_fixture("while_nested.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_while_nested_break() {
    let src = run_fixture("while_nested_break.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_break_outside_loop() {
    let src = run_fixture("break_outside_loop.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert!(!out.diagnostics.is_empty());
    assert!(out.diagnostics.iter().any(|d| d.message.contains("outside of loop")));
}

#[test]
fn mir_continue_outside_loop() {
    let src = run_fixture("continue_outside_loop.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert!(!out.diagnostics.is_empty());
    assert!(out.diagnostics.iter().any(|d| d.message.contains("outside of loop")));
}

#[test]
fn mir_while_infinite() {
    let src = run_fixture("while_infinite.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_for_simple() {
    let src = run_fixture("for_simple.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#[test]
fn mir_for_nested() {
    let src = run_fixture("for_nested.glyph");
    let out = compile_source(&src, FrontendOptions { emit_mir: true });
    assert_debug_snapshot!(out.mir);
}

#![cfg(feature = "codegen")]

use glyph_backend::llvm::LlvmBackend;
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

#[cfg(all(feature = "codegen", unix))]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};
#[cfg(all(feature = "codegen", unix))]
use std::os::unix::process::ExitStatusExt;
#[cfg(all(feature = "codegen", unix))]
use std::process::Command;
#[cfg(all(feature = "codegen", unix))]
use tempfile::TempDir;

fn compile_ir(source: &str) -> String {
    let out = compile_source(
        source,
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
fn ir_ui_session_guard_drop_calls_runtime_cleanup() {
    let source = r#"
        from std/term import Terminal
        from std/enums import Result

        fn main() -> i32 {
          let mut t = Terminal::stdout()
          let session_result = t.enter_ui_session()
          let session = match session_result {
            Ok(g) => g,
            Err(_e) => { ret 1 },
          }
          ret 0
        }
    "#;

    let ir = compile_ir(source);
    assert!(
        ir.contains("glyph_term_session_end"),
        "IR missing glyph_term_session_end runtime call: {}",
        ir
    );
    assert!(
        ir.contains("term.guard.drop.end"),
        "IR missing UiSessionGuard drop call site: {}",
        ir
    );
}

#[cfg(all(feature = "codegen", unix))]
fn build_and_run_exit_code(source: &str) -> i32 {
    if std::env::var("GLYPH_SKIP_RUN_MAIN").is_ok() {
        return 0;
    }

    let frontend_output = compile_source(
        source,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );

    assert!(
        frontend_output.diagnostics.is_empty(),
        "Compilation failed with diagnostics: {:?}",
        frontend_output.diagnostics
    );

    let temp = TempDir::new().unwrap();
    let obj_path = temp.path().join("test.o");
    let exe_path = temp.path().join("test_exe");

    let mut ctx = CodegenContext::new("glyph_module").unwrap();
    ctx.codegen_module(&frontend_output.mir).unwrap();

    if std::env::var("GLYPH_SKIP_RUN").is_ok() {
        return 0;
    }
    ctx.emit_object_file(&obj_path).unwrap();

    let linker = Linker::new();
    let opts = LinkerOptions {
        output_path: exe_path.clone(),
        object_files: vec![obj_path],
        link_libs: Vec::new(),
        link_search_paths: Vec::new(),
        runtime_lib_path: Linker::get_runtime_lib_path(),
    };
    linker.link(&opts).unwrap();

    let status = Command::new(&exe_path).status().unwrap();
    if let Some(code) = status.code() {
        code
    } else if let Some(sig) = status.signal() {
        -sig
    } else {
        -1
    }
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn ui_session_guard_cleanup_runs_on_early_return() {
    let source = r#"
        from std/term import Terminal
        from std/enums import Result

        fn run_once(early: bool) -> i32 {
          let mut t = Terminal::stdout()
          let session_result = t.enter_ui_session()
          let session = match session_result {
            Ok(g) => g,
            Err(_e) => { ret 9 },
          }
          if early { ret 0 }

          let _flush = t.flush()
          ret 0
        }

        fn main() -> i32 {
          let first = run_once(true)
          if first != 0 { ret 1 }

          let second = run_once(false)
          if second != 0 { ret 2 }

          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

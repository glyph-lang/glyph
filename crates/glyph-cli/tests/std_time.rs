use glyph_frontend::{compile_source, FrontendOptions};

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
fn std_time_human_readable_format() {
    let source = r#"
        from std/time import get_timestamp, Timestamp, Error
        from std/string import byte_at
        from std/enums import Result

        fn main() -> i32 {
          let ts = get_timestamp()
          let res: Result<String, Error> = ts.to_human_readable()
          ret match res {
            Ok(s) => {
              let s0: str = s
              if s0.len() != 19 { ret 10 }
              if byte_at(s0, 2) != 47 { ret 11 }
              if byte_at(s0, 5) != 47 { ret 12 }
              if byte_at(s0, 10) != 32 { ret 13 }
              if byte_at(s0, 13) != 58 { ret 14 }
              if byte_at(s0, 16) != 58 { ret 15 }
              ret 0
            },
            Err(_e) => 1,
          }
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

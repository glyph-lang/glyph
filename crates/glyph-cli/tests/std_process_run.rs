use glyph_frontend::{compile_source, FrontendOptions};

#[cfg(all(feature = "codegen", unix))]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[cfg(all(feature = "codegen", unix))]
use std::process::Command;

#[cfg(all(feature = "codegen", unix))]
use tempfile::TempDir;

#[cfg(all(feature = "codegen", unix))]
fn build_and_run_exit_code(source: &str) -> i32 {
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
    status.code().unwrap_or(-1)
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_true_returns_0() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("x"))
            let _ = args.pop()
            ret run("true", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_false_returns_1() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("x"))
            let _ = args.pop()
            ret run("false", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 1);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_with_args() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("echo hi"))
            ret run("sh", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

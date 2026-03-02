use glyph_frontend::{FrontendOptions, compile_source};

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

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_missing_command_returns_negative_errno() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let args: Vec<String> = Vec::new()
            let rc = run("__glyph_command_that_should_not_exist__", &args)
            ret if rc < 0 { 0 } else { 1 }
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_empty_command_returns_negative_errno() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let args: Vec<String> = Vec::new()
            let rc = run("", &args)
            ret if rc < 0 { 0 } else { 1 }
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_preserves_empty_and_spaced_arguments() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("test -z \"$1\" && test \"$2\" = \"a b\""))
            args.push(String::from_str("glyph"))
            args.push(String::from_str(""))
            args.push(String::from_str("a b"))
            ret run("sh", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_does_not_apply_shell_expansion_to_args() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("test \"$1\" = \"a b\" && test \"$2\" = \"x*y\" && test \"$3\" = \"\\$HOME\""))
            args.push(String::from_str("glyph"))
            args.push(String::from_str("a b"))
            args.push(String::from_str("x*y"))
            args.push(String::from_str("$HOME"))
            ret run("sh", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_many_args_round_trip_count() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("test \"$#\" -eq 64"))
            args.push(String::from_str("glyph"))

            let mut i: i32 = 0
            while i < 64 {
                args.push(String::from_str("x"))
                i = i + 1
            }

            ret run("sh", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_signal_exit_maps_to_128_plus_signal() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("kill -9 $$"))
            let rc = run("sh", &args)
            ret if rc == 137 { 0 } else { 1 }
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_process_run_absolute_path_command() {
    let source = r#"
        from std/process import run
        from std/vec import Vec

        fn main() -> i32 {
            let mut args: Vec<String> = Vec::new()
            args.push(String::from_str("-c"))
            args.push(String::from_str("exit 7"))
            ret run("/bin/sh", &args)
        }
    "#;
    assert_eq!(build_and_run_exit_code(source), 7);
}

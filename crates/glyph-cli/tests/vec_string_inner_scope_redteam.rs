/// Red-team tests for Vec<String> built inside inner (if-block) scopes.
///
/// Root cause: when a Vec<String> is populated inside an if-block, the
/// compiler may emit drop glue for the pushed Strings when the inner
/// scope exits, leaving the Vec holding dangling pointers. Accessing
/// the Vec after the if-block (or passing it to a function) then
/// crashes or returns garbage.
///
/// This is the same class of bug as the argv double-free fixed in
/// commit 1db7ab1 (skip_drop on shallow-copy locals). The canonical
/// production reproducer is a Vec<String> context built inside an if
/// block and then passed to `build_packet_from_task_file`.
///
/// Test numbering is local to this file (T1–T6).

#[cfg(all(feature = "codegen", unix))]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[cfg(all(feature = "codegen", unix))]
use glyph_frontend::{FrontendOptions, compile_source};

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

    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        let ir = ctx.dump_ir();
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("vec_string_inner_scope_debug.ll"), ir).unwrap();
        std::fs::write(
            root.join("vec_string_inner_scope_mir_debug.txt"),
            format!("{:#?}\n", frontend_output.mir),
        )
        .unwrap();
        if std::env::var("GLYPH_SKIP_RUN").is_ok() {
            return 0;
        }
    }

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

// ---------------------------------------------------------------------------
// T1: Baseline — Vec<String> pushed inside `if true` block, elements read
// after the block exits.
//
// BUG: If the if-scope emits drop glue for the pushed Strings on exit,
// v[0] / v[1] will point at freed memory → crash or garbage data.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_pushed_in_if_scope_read_outside() {
    let source = r#"
        from std/string import byte_at

        fn main() -> i32 {
          let mut v: Vec<String> = Vec::new()
          if true {
            v.push(String::from_str("hello"))
            v.push(String::from_str("world"))
          }
          let s0: str = v[0]
          let s1: str = v[1]
          if s0.len() != 5 { ret 1 }
          if s1.len() != 5 { ret 2 }
          if byte_at(s0, 0) != 104 { ret 3 }
          if byte_at(s1, 0) != 119 { ret 4 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

// ---------------------------------------------------------------------------
// T2: Vec<String> built in if-block, passed by value to a function.
//
// This is the direct analogue of the phase_implementation.glyph pattern:
// a Vec<String> context is constructed inside an if-block, then passed
// to `build_packet_from_task_file`. If the Strings are freed on if-scope
// exit, the function receives a Vec of dangling pointers → crash.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_built_in_if_scope_passed_to_fn() {
    let source = r#"
        from std/string import byte_at

        fn build_from_vec(v: Vec<String>) -> i32 {
          let s0: str = v[0]
          let s1: str = v[1]
          if s0.len() != 5 { ret 1 }
          if s1.len() != 5 { ret 2 }
          if byte_at(s0, 0) != 104 { ret 3 }
          if byte_at(s1, 0) != 119 { ret 4 }
          ret 0
        }

        fn main() -> i32 {
          let mut v: Vec<String> = Vec::new()
          if true {
            v.push(String::from_str("hello"))
            v.push(String::from_str("world"))
          }
          ret build_from_vec(v)
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

// ---------------------------------------------------------------------------
// T3: Multiple Strings pushed in if-block; verify every element.
//
// Exercises the same bug with more elements so that even partial corruption
// (e.g. only later elements freed) is detectable.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_if_scope_multiple_elements_all_verified() {
    let source = r#"
        from std/string import byte_at

        fn main() -> i32 {
          let mut v: Vec<String> = Vec::new()
          if true {
            v.push(String::from_str("alpha"))
            v.push(String::from_str("beta"))
            v.push(String::from_str("gamma"))
            v.push(String::from_str("delta"))
          }
          let n: usize = v.len()
          if n != 4 { ret 1 }
          let s0: str = v[0]
          let s1: str = v[1]
          let s2: str = v[2]
          let s3: str = v[3]
          if s0.len() != 5 { ret 2 }
          if s1.len() != 4 { ret 3 }
          if s2.len() != 5 { ret 4 }
          if s3.len() != 5 { ret 5 }
          if byte_at(s0, 0) != 97  { ret 6 }
          if byte_at(s1, 0) != 98  { ret 7 }
          if byte_at(s2, 0) != 103 { ret 8 }
          if byte_at(s3, 0) != 100 { ret 9 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

// ---------------------------------------------------------------------------
// T4: Runtime-conditional push — the if-condition depends on a variable so
// the compiler cannot constant-fold the branch away.
//
// This ensures the test is not trivially bypassed by branch elimination:
// the Vec is populated only when `flag == 1`, which is always true at
// runtime but opaque at compile time.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_runtime_conditional_if_scope_passed_to_fn() {
    let source = r#"
        from std/string import byte_at

        fn consume(v: Vec<String>) -> i32 {
          if v.len() != 2 { ret 1 }
          let s0: str = v[0]
          let s1: str = v[1]
          if s0.len() != 3 { ret 2 }
          if s1.len() != 3 { ret 3 }
          if byte_at(s0, 0) != 102 { ret 4 }
          if byte_at(s1, 0) != 98  { ret 5 }
          ret 0
        }

        fn main() -> i32 {
          let flag: i32 = 1
          let mut v: Vec<String> = Vec::new()
          if flag == 1 {
            v.push(String::from_str("foo"))
            v.push(String::from_str("bar"))
          }
          ret consume(v)
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

// ---------------------------------------------------------------------------
// T5: Nested if-scopes — outer if pushes one string, inner if pushes another.
//
// Tests whether drop glue from multiple nested scope exits each corrupt an
// element. If the inner scope frees its string and the outer scope frees its
// string, v[0] and v[1] are both dangling when passed to the function.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_nested_if_scopes_passed_to_fn() {
    let source = r#"
        from std/string import byte_at

        fn verify(v: Vec<String>) -> i32 {
          if v.len() != 2 { ret 1 }
          let s0: str = v[0]
          let s1: str = v[1]
          if s0.len() != 5 { ret 2 }
          if s1.len() != 5 { ret 3 }
          if byte_at(s0, 0) != 111 { ret 4 }
          if byte_at(s1, 0) != 105 { ret 5 }
          ret 0
        }

        fn main() -> i32 {
          let mut v: Vec<String> = Vec::new()
          if true {
            v.push(String::from_str("outer"))
            if true {
              v.push(String::from_str("inner"))
            }
          }
          ret verify(v)
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

// ---------------------------------------------------------------------------
// T6: Loop body builds Vec<String> inside an if-block on each iteration,
// calls a function with it, then the Vec goes out of scope.
//
// 10 iterations: the Vec is rebuilt each time. If the inner-scope bug exists,
// every iteration produces a Vec of dangling pointers passed to the function.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn vec_string_if_scope_in_loop_passed_to_fn() {
    let source = r#"
        from std/string import byte_at

        fn check(v: Vec<String>, expected_first_byte: i32) -> i32 {
          if v.len() != 1 { ret 1 }
          let s: str = v[0]
          if s.len() != 4 { ret 2 }
          if byte_at(s, 0) != expected_first_byte { ret 3 }
          ret 0
        }

        fn main() -> i32 {
          let mut i: i32 = 0
          while i < 10 {
            let mut v: Vec<String> = Vec::new()
            if true {
              v.push(String::from_str("item"))
            }
            let code = check(v, 105)
            if code != 0 { ret code }
            i = i + 1
          }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

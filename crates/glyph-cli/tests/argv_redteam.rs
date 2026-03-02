/// Systematic argv handling tests for the Glyph compiler.
///
/// These tests exercise argv building and cleanup at escalating complexity,
/// modeled after `vec_string_redteam.rs`. The goal is to expose the
/// malloc double-free bug where a raw `str` reference is freed as if
/// it were an owned `String` during argv cleanup.

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

/// Compile Glyph source, link, and run with the given args.
/// Returns the process exit code (or negative signal number on crash).
#[cfg(all(feature = "codegen", unix))]
fn build_and_run_with_args(source: &str, args: &[&str]) -> i32 {
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
        std::fs::write(root.join("argv_redteam_debug.ll"), ir).unwrap();
        std::fs::write(
            root.join("argv_redteam_mir_debug.txt"),
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

    let status = Command::new(&exe_path).args(args).status().unwrap();
    if let Some(code) = status.code() {
        code
    } else if let Some(sig) = status.signal() {
        -sig
    } else {
        -1
    }
}

// ---------------------------------------------------------------------------
// T1: Baseline — argv build + cleanup with only program name (no extra args)
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_no_args() {
    let source = r#"
        from std/sys import argv

        fn main() -> i32 {
          let args = argv
          ret 0
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &[]), 0);
}

// ---------------------------------------------------------------------------
// T2: Access .len() without touching any element
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_len_only() {
    let source = r#"
        from std/sys import argv

        fn main() -> i32 {
          let args = argv
          let n: usize = args.len()
          if n < 2 { ret 1 }
          ret 0
        }
    "#;

    // With one extra arg, argc == 2 (program name + "hello")
    assert_eq!(build_and_run_with_args(source, &["hello"]), 0);
}

// ---------------------------------------------------------------------------
// T3: Safe .get(0) + match + .clone() — single arg access
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_get_clone() {
    let source = r#"
        from std/sys import argv
        from std/enums import Option
        from std/string import byte_at

        fn main() -> i32 {
          let args = argv
          let opt = args.get(1)
          let val = match opt {
            Some(s) => s.clone(),
            None => { ret 1 },
          }
          let r: str = val
          if r.len() != 5 { ret 2 }
          if byte_at(r, 0) != 104 { ret 3 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &["hello"]), 0);
}

// ---------------------------------------------------------------------------
// T4: Safe .get() + .clone() on multiple args
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_get_clone_multiple() {
    let source = r#"
        from std/sys import argv
        from std/enums import Option
        from std/string import byte_at

        fn main() -> i32 {
          let args = argv
          let opt0 = args.get(1)
          let v0 = match opt0 {
            Some(s) => s.clone(),
            None => { ret 1 },
          }
          let opt1 = args.get(2)
          let v1 = match opt1 {
            Some(s) => s.clone(),
            None => { ret 2 },
          }
          let opt2 = args.get(3)
          let v2 = match opt2 {
            Some(s) => s.clone(),
            None => { ret 3 },
          }

          let r0: str = v0
          let r1: str = v1
          let r2: str = v2

          if r0.len() != 5 { ret 10 }
          if byte_at(r0, 0) != 104 { ret 11 }
          if r1.len() != 5 { ret 20 }
          if byte_at(r1, 0) != 119 { ret 21 }
          if r2.len() != 4 { ret 30 }
          if byte_at(r2, 0) != 116 { ret 31 }
          ret 0
        }
    "#;

    assert_eq!(
        build_and_run_with_args(source, &["hello", "world", "test"]),
        0
    );
}

// ---------------------------------------------------------------------------
// T5: Direct index with str ref — `let s: str = args[1]`
// This is the pattern that crashes in scribe (double-free of argv string).
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_direct_index_str_ref() {
    let source = r#"
        from std/sys import argv

        fn main() -> i32 {
          let args = argv
          let s: str = args[1]
          if s.len() != 5 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &["snake"]), 0);
}

// ---------------------------------------------------------------------------
// T6: Direct index as String move — `let arg = args[1]`
// Tests if move-from-vec causes double-free on cleanup.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_direct_index_string_move() {
    let source = r#"
        from std/sys import argv

        fn main() -> i32 {
          let args = argv
          let arg = args[1]
          let s: str = arg
          if s.len() != 5 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &["snake"]), 0);
}

// ---------------------------------------------------------------------------
// T7: Safe loop — iterate all args with .get(i) + match + .clone()
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_loop_get() {
    let source = r#"
        from std/sys import argv
        from std/enums import Option
        from std/string import byte_at

        fn main() -> i32 {
          let args = argv
          let n: usize = args.len()
          let mut total: usize = 0
          let mut i: usize = 1
          while i < n {
            let opt = args.get(i)
            let val = match opt {
              Some(s) => s.clone(),
              None => { ret 1 },
            }
            let r: str = val
            total = total + r.len()
            i = i + 1
          }
          if total != 15 { ret 2 }
          ret 0
        }
    "#;

    // "hello" (5) + "world" (5) + "test!" (5) = 15
    assert_eq!(
        build_and_run_with_args(source, &["hello", "world", "test!"]),
        0
    );
}

// ---------------------------------------------------------------------------
// T8: Unsafe loop — iterate all args with direct index args[i]
// This is the pattern from test_argv.glyph that crashes.
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_loop_direct_index() {
    let source = r#"
        from std/sys import argv

        fn main() -> i32 {
          let args = argv
          let n: usize = args.len()
          let mut total: usize = 0
          let mut i: usize = 0
          while i < n {
            let arg = args[i]
            let r: str = arg
            total = total + r.len()
            i = i + 1
          }
          if total == 0 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &["hello", "world"]), 0);
}

// ---------------------------------------------------------------------------
// T9: Build a new String from an argv arg — mimics scribe's path_join usage
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_string_concat_from_arg() {
    let source = r#"
        from std/sys import argv
        from std/enums import Option
        from std import String
        from std/string import byte_at

        fn main() -> i32 {
          let args = argv
          let opt = args.get(1)
          let val = match opt {
            Some(s) => s.clone(),
            None => { ret 1 },
          }
          let r: str = val
          let path = String::from_str(r) + ".json"
          let path_ref: str = path
          if path_ref.len() != 10 { ret 2 }
          if byte_at(path_ref, 5) != 46 { ret 3 }
          ret 0
        }
    "#;

    // "hello" + ".json" = "hello.json" (10 chars), byte_at(5) = '.' = 46
    assert_eq!(build_and_run_with_args(source, &["hello"]), 0);
}

// ---------------------------------------------------------------------------
// T10: Clone an arg and pass it to a function — ownership transfer across calls
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn argv_pass_to_function() {
    let source = r#"
        from std/sys import argv
        from std/enums import Option
        from std/string import byte_at

        fn check_arg(s: String) -> i32 {
          let r: str = s
          if r.len() != 5 { ret 1 }
          if byte_at(r, 0) != 104 { ret 2 }
          ret 0
        }

        fn main() -> i32 {
          let args = argv
          let opt = args.get(1)
          let val = match opt {
            Some(s) => s.clone(),
            None => { ret 10 },
          }
          ret check_arg(val)
        }
    "#;

    assert_eq!(build_and_run_with_args(source, &["hello"]), 0);
}

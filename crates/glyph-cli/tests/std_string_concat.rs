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

    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        let ir = ctx.dump_ir();
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("std_string_concat_debug.ll"), ir).unwrap();
        std::fs::write(
            root.join("std_string_concat_mir_debug.txt"),
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

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_string_concat_basic() {
    let source = r#"
        from std/string import byte_at

        fn main() -> i32 {
          let a = String::from_str("ab")
          let b = String::from_str("cd")
          let c = a.concat(b)
          let s0: str = c
          if s0.len() != 4 { ret 10 }
          if byte_at(s0, 0) != 97 { ret 11 }
          if byte_at(s0, 3) != 100 { ret 12 }

          let base = String::from_str("")
          let d = base + String::from_str("x")
          let s1: str = d
          if s1.len() != 1 { ret 20 }
          if byte_at(s1, 0) != 120 { ret 21 }

          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_string_concat_loop() {
    let source = r#"
        from std/string import byte_at

        fn main() -> i32 {
          let mut s = String::from_str("")
          let mut i: usize = 0
          while i < 2048 {
            s = s.concat(String::from_str("a"))
            i = i + 1
          }
          let s0: str = s
          if s0.len() != 2048 { ret 1 }
          if byte_at(s0, 0) != 97 { ret 2 }
          if byte_at(s0, 2047) != 97 { ret 3 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_string_concat_lengths() {
    let source = r#"
        from std/string import byte_at

        fn check_len_str(target: usize, code: i32) -> i32 {
          let mut s = String::from_str("")
          let mut i: usize = 0
          while i < target {
            s = s.concat(String::from_str("a"))
            i = i + 1
          }
          let s0: str = s
          if s0.len() != target { ret code }
          if byte_at(s0, 0) != 97 { ret code + 1 }
          if byte_at(s0, target - 1) != 97 { ret code + 2 }
          ret 0
        }

        fn check_len_ref(target: usize, code: i32) -> i32 {
          let mut s = String::from_str("")
          let mut i: usize = 0
          while i < target {
            s = s.concat(String::from_str("a"))
            i = i + 1
          }
          let s_ref = &s
          if s_ref.len() != target { ret code }
          ret 0
        }

        fn main() -> i32 {
          let r0 = check_len_ref(64, 1)
          if r0 != 0 { ret r0 }
          let r1 = check_len_str(64, 10)
          if r1 != 0 { ret r1 }
          let r2 = check_len_str(256, 20)
          if r2 != 0 { ret r2 }
          let r3 = check_len_str(512, 30)
          if r3 != 0 { ret r3 }
          let r4 = check_len_str(1024, 40)
          if r4 != 0 { ret r4 }
          let r5 = check_len_str(2048, 50)
          if r5 != 0 { ret r5 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_string_concat_loop_bucket() {
    let source = r#"
        fn main() -> i32 {
          let mut s = String::from_str("")
          let mut i: usize = 0
          while i < 2048 {
            s = s.concat(String::from_str("a"))
            let s_ref = &s
            let want = i + 1
            if s_ref.len() != want {
              let mut bucket: i32 = 0
              let mut limit: usize = 0
              while limit < want {
                limit = limit + 64
                bucket = bucket + 1
              }
              ret bucket
            }
            i = i + 1
          }
          ret 0
        }
    "#;

    let code = build_and_run_exit_code(source);
    assert_eq!(code, 0, "concat length drifted in bucket {}", code);
}

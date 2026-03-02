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
fn for_range_break_and_bounds() {
    let source = r#"
        fn main() -> i32 {
          let mut acc: i32 = 0
          for i in 0..10 {
            if i > 7 { break }
            if i % 2 == 1 {
              acc = acc + i
            }
          }
          if acc != 16 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn for_range_continue_runs_increment_path() {
    let source = r#"
        fn main() -> i32 {
          let mut acc: i32 = 0
          for i in 0..8 {
            if i % 2 == 0 { cont }
            acc = acc + i
          }
          if acc != 16 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn for_in_vec_struct_unpacks_fields() {
    let source = r#"
        struct Pair { left: i32, right: i32 }

        fn main() -> i32 {
          let mut pairs: Vec<Pair> = Vec::new()
          pairs.push(Pair { left: 1, right: 2 })
          pairs.push(Pair { left: 3, right: 4 })
          pairs.push(Pair { left: 5, right: 6 })

          let mut sum: i32 = 0
          for pair in pairs {
            sum = sum + pair.left
            sum = sum + pair.right
          }

          if sum != 21 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn for_in_vec_struct_continue_runs_index_increment() {
    let source = r#"
        struct Pair { left: i32, right: i32 }

        fn main() -> i32 {
          let mut pairs: Vec<Pair> = Vec::new()
          pairs.push(Pair { left: 0, right: 10 })
          pairs.push(Pair { left: 1, right: 11 })
          pairs.push(Pair { left: 2, right: 12 })
          pairs.push(Pair { left: 3, right: 13 })

          let mut sum: i32 = 0
          for pair in pairs {
            if pair.left % 2 == 0 { cont }
            sum = sum + pair.right
          }

          if sum != 24 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn for_in_vec_enum_unpacks_match_payloads() {
    let source = r#"
        struct Pair { left: i32, right: i32 }

        enum Item {
          Scalar(i32)
          PairValue(Pair)
          Skip
        }

        fn main() -> i32 {
          let mut items: Vec<Item> = Vec::new()
          items.push(Scalar(2))
          items.push(PairValue(Pair { left: 3, right: 4 }))
          items.push(Skip())
          items.push(Scalar(5))

          let mut sum: i32 = 0
          for item in items {
            let value = match item {
              Scalar(v) => v,
              PairValue(p) => p.left + p.right,
              Skip => 0,
            }
            sum = sum + value
          }

          if sum != 14 { ret 1 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

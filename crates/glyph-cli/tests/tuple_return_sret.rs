#[cfg(all(feature = "codegen", unix))]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[cfg(all(feature = "codegen", unix))]
use glyph_frontend::{compile_source, FrontendOptions};

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
        std::fs::write(root.join("tuple_return_sret_debug.ll"), ir).unwrap();
        std::fs::write(
            root.join("tuple_return_sret_mir_debug.txt"),
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
fn tuple_return_scalars() {
    let source = r#"
        fn make_pair() -> (i32, i32) {
          ret (9, 11)
        }

        fn main() -> i32 {
          let pair = make_pair()
          if pair.0 != 9 { ret 1 }
          if pair.1 != 11 { ret 2 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn tuple_return_string_second() {
    let source = r#"
        from std import String
        from std/string import byte_at

        fn make_pair() -> (i32, String) {
          ret (7, String::from_str("phase"))
        }

        fn wrap_pair() -> (i32, String) {
          let p = make_pair()
          ret p
        }

        fn main() -> i32 {
          let pair = wrap_pair()
          if pair.0 != 7 { ret 1 }
          let s: str = pair.1
          if s.len() != 5 { ret 2 }
          if byte_at(s, 0) != 112 { ret 3 }
          if byte_at(s, 4) != 101 { ret 4 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn tuple_return_string_first() {
    let source = r#"
        from std import String
        from std/string import byte_at

        fn make_pair() -> (String, i32) {
          ret (String::from_str("alpha"), 42)
        }

        fn main() -> i32 {
          let pair = make_pair()
          let s: str = pair.0
          if s.len() != 5 { ret 1 }
          if byte_at(s, 0) != 97 { ret 2 }
          if byte_at(s, 4) != 97 { ret 3 }
          if pair.1 != 42 { ret 4 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn tuple_return_string_middle() {
    let source = r#"
        from std import String
        from std/string import byte_at

        fn make_triple() -> (i32, String, i32) {
          ret (1, String::from_str("middle"), 3)
        }

        fn main() -> i32 {
          let triple = make_triple()
          if triple.0 != 1 { ret 1 }
          let s: str = triple.1
          if s.len() != 6 { ret 2 }
          if byte_at(s, 0) != 109 { ret 3 }
          if byte_at(s, 5) != 101 { ret 4 }
          if triple.2 != 3 { ret 5 }
          ret 0
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn tuple_return_nested() {
    let source = r#"
        from std import String
        from std/string import byte_at

        fn make_nested() -> ((i32, String), i32) {
          ret ((3, String::from_str("nest")), 5)
        }

        fn check_nested(t: ((i32, String), i32)) -> i32 {
          if t.1 != 5 { ret 5 }
          if (t.0).0 != 3 { ret 1 }
          let s: str = (t.0).1
          if s.len() != 4 { ret 2 }
          if byte_at(s, 0) != 110 { ret 3 }
          if byte_at(s, 3) != 116 { ret 4 }
          ret 0
        }

        fn main() -> i32 {
          let nested = make_nested()
          ret check_nested(nested)
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

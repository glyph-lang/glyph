use glyph_frontend::{compile_source, FrontendOptions};

#[cfg(all(feature = "codegen", unix))]
use glyph_backend::{
    codegen::CodegenContext,
    linker::{Linker, LinkerOptions},
};

#[cfg(all(feature = "codegen", unix))]
use std::process::Command;

#[cfg(all(feature = "codegen", unix))]
use std::os::unix::process::ExitStatusExt;

#[cfg(all(feature = "codegen", unix))]
use tempfile::TempDir;

#[cfg(all(feature = "codegen", unix))]
fn build_and_run_exit_code(source: &str) -> i32 {
    if std::env::var("GLYPH_SKIP_RUN_MAIN").is_ok() {
        return 0;
    }

    if std::env::var("GLYPH_SKIP_RUN").is_ok() || std::env::var("GLYPH_DEBUG_IR").is_ok() {
        println!("[build_and_run] compile start");
    }

    let frontend_output = compile_source(
        source,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );

    if std::env::var("GLYPH_SKIP_RUN").is_ok() || std::env::var("GLYPH_DEBUG_IR").is_ok() {
        println!("[build_and_run] compile done");
    }

    assert!(
        frontend_output.diagnostics.is_empty(),
        "Compilation failed with diagnostics: {:?}",
        frontend_output.diagnostics
    );

    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
        std::fs::create_dir_all(&root).unwrap();
        let mir_path = root.join("std_json_parser_mir_debug.txt");
        std::fs::write(&mir_path, format!("{:#?}\n", frontend_output.mir)).unwrap();
        println!("[build_and_run] wrote MIR to {:?}", mir_path);
    }

    // Optional shortcut for debugging codegen without running the produced binary.
    let skip_run = std::env::var("GLYPH_SKIP_RUN").is_ok();

    let temp = TempDir::new().unwrap();
    let obj_path = temp.path().join("test.o");
    let exe_path = temp.path().join("test_exe");

    let mut ctx = CodegenContext::new("glyph_module").unwrap();
    if std::env::var("GLYPH_SKIP_RUN").is_ok() || std::env::var("GLYPH_DEBUG_IR").is_ok() {
        println!("[build_and_run] codegen start");
    }
    ctx.codegen_module(&frontend_output.mir).unwrap();
    if std::env::var("GLYPH_SKIP_RUN").is_ok() || std::env::var("GLYPH_DEBUG_IR").is_ok() {
        println!("[build_and_run] codegen done");
    }

    if std::env::var("GLYPH_DEBUG_IR").is_ok() {
        let ir = ctx.dump_ir();
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../target");
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("std_json_parser_debug.ll"), ir).unwrap();
        if skip_run {
            return 0;
        }
    }

    if skip_run {
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
fn map_string_basic() {
    let source = r#"
        from std/map import Map
        from std/enums import Option, Result

        fn main() -> i32 {
          let mut m: Map<String, i32> = Map::new()
          let add_res = m.add(String::from_str("a"), 1)
          let add_code = match add_res {
            Ok(_) => 0,
            Err(_e) => 10,
          }
          if add_code != 0 { ret add_code }
          let g = m.get(String::from_str("a"))
          ret match g {
            Some(v) => if v == 1 { 0 } else { 11 },
            None => 12,
          }
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

#[cfg(all(feature = "codegen", unix))]
#[test]
fn std_json_parser_nested_and_trailing() {
    let source = r#"
        from std/enums import Option
        from std/json import JsonValue, ParseResult
        from std/map import Map
        from std/vec import Vec

        from std/json/parser import parse

        fn check_null() -> i32 {
          let r = parse("null")
          ret match r {
            Ok(v) => match v {
              Null => 0,
              _ => 1,
            },
            Err(_e) => 1,
          }
        }

        fn check_nested_array() -> i32 {
          let r = parse("[[1, 2], [3, 4]]")
          ret match r {
            Ok(v) => match v {
              Array(arr) => check_outer_array(arr),
              _ => 1,
            },
            Err(_e) => 1,
          }
        }

        fn check_outer_array(arr0: Vec<JsonValue>) -> i32 {
          let mut arr = arr0
          let right = arr.pop()
          let left = arr.pop()
          let extra = arr.pop()

          ret match extra {
            Some(_v) => 1,
            None => match left {
              Some(lv) => match lv {
                Array(inner) => match right {
                  Some(rv) => match rv {
                    Array(inner2) => {
                      let s0 = check_pair_numbers(inner)
                      let s1 = check_pair_numbers(inner2)
                      if s0 == 0 { s1 } else { 1 }
                    }
                    _ => 1,
                  },
                  None => 1,
                },
                _ => 1,
              },
              None => 1,
            },
          }
        }

        fn check_pair_numbers(arr0: Vec<JsonValue>) -> i32 {
          let mut arr = arr0
          let b = arr.pop()
          let a = arr.pop()
          let extra = arr.pop()

          ret match extra {
            Some(_v) => 1,
            None => match a {
              Some(av) => match av {
                Number(_) => match b {
                  Some(bv) => match bv {
                    Number(_) => 0,
                    _ => 1,
                  },
                  None => 1,
                },
                _ => 1,
              },
              None => 1,
            },
          }
        }

        fn check_nested_object() -> i32 {
          let r = parse("{\"a\": {\"b\": 3}}")
          ret match r {
            Ok(v) => match v {
              Object(obj) => check_object_a_b(obj),
              _ => 11,
            },
            Err(_e) => 101,
          }
        }

        fn check_object_a_b(obj: Map<String, JsonValue>) -> i32 {
          let a_opt = obj.get(String::from_str("a"))
          ret match a_opt {
            Some(a_val) => match a_val {
              Object(inner) => {
                let b_opt = inner.get(String::from_str("b"))
                match b_opt {
                  Some(b_val) => match b_val {
                    Number(_) => 0,
                    _ => 13,
                  },
                  None => 12,
                }
              }
              _ => 14,
            },
            None => 15,
          }
        }

        fn check_trailing_rejected() -> i32 {
          let r = parse("null x")
          ret match r {
            Ok(_v) => 1,
            Err(_e) => 0,
          }
        }

        fn main() -> i32 {
          let r = check_nested_object()
          ret r
        }
    "#;

    assert_eq!(build_and_run_exit_code(source), 0);
}

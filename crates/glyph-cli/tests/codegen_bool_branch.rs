/// Tests for bare boolean branch conditions (`if flag` without `== true`).
///
/// Prior to the fix, `if bool_var` crashed LLVM with "Branch condition is not i1"
/// because untyped locals default to i32 storage, and LLVMBuildCondBr requires i1.
/// The fix truncates the loaded value to i1 before branching.

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
fn build_and_run(source: &str) -> (i32, String) {
    if std::env::var("GLYPH_SKIP_RUN_MAIN").is_ok() {
        return (0, String::new());
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
        return (0, String::new());
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

    let output = Command::new(&exe_path).output().unwrap();
    let code = if let Some(c) = output.status.code() {
        c
    } else if let Some(sig) = output.status.signal() {
        -sig
    } else {
        -1
    };
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    (code, stdout)
}

// ---------------------------------------------------------------------------
// Basic: `if true_literal` — should take the then-branch
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_literal_true() {
    let source = r#"
        fn main() -> i32 {
          if true {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "if true should take then-branch");
}

// ---------------------------------------------------------------------------
// Basic: `if false` — should take the else-branch
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_literal_false() {
    let source = r#"
        fn main() -> i32 {
          if false {
            ret 1
          }
          ret 0
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "if false should skip then-branch");
}

// ---------------------------------------------------------------------------
// Core bug: `let flag = true; if flag { ... }` — the original crash site
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_variable_true() {
    let source = r#"
        fn main() -> i32 {
          let flag = true
          if flag {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "if flag (true) should take then-branch");
}

// ---------------------------------------------------------------------------
// Core bug: `let flag = false; if flag { ... } else { ... }`
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_variable_false_else() {
    let source = r#"
        fn main() -> i32 {
          let flag = false
          if flag {
            ret 1
          } else {
            ret 0
          }
          ret 2
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "if flag (false) should take else-branch");
}

// ---------------------------------------------------------------------------
// Negation: `if !flag`
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_negated() {
    let source = r#"
        fn main() -> i32 {
          let flag = false
          if !flag {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "if !flag (flag=false) should take then-branch");
}

// ---------------------------------------------------------------------------
// Bool from comparison used bare: `let flag = (x > 0); if flag { ... }`
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_from_comparison() {
    let source = r#"
        fn main() -> i32 {
          let x: i32 = 42
          let flag = x > 0
          if flag {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "bool from comparison should work bare in if");
}

// ---------------------------------------------------------------------------
// Bool parameter passed to function, used bare in if
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_function_param() {
    let source = r#"
        fn check(b: bool) -> i32 {
          if b {
            ret 1
          }
          ret 0
        }

        fn main() -> i32 {
          let r1 = check(true)
          let r2 = check(false)
          if r1 == 1 {
            if r2 == 0 {
              ret 0
            }
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "bool param should work bare in if");
}

// ---------------------------------------------------------------------------
// Bool return value used bare
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_return_value() {
    let source = r#"
        fn is_positive(x: i32) -> bool {
          ret x > 0
        }

        fn main() -> i32 {
          let result = is_positive(5)
          if result {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "bool return value should work bare in if");
}

// ---------------------------------------------------------------------------
// Bool in while loop condition
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_in_while() {
    let source = r#"
        fn main() -> i32 {
          let mut running = true
          let mut count: i32 = 0
          while running {
            count = count + 1
            if count == 5 {
              running = false
            }
          }
          if count == 5 {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "bare bool in while loop should work");
}

// ---------------------------------------------------------------------------
// Bool mutation: set true then false, branch should reflect final value
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_mutation() {
    let source = r#"
        fn main() -> i32 {
          let mut flag = true
          flag = false
          if flag {
            ret 1
          }
          ret 0
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "mutated bool (now false) should skip then-branch");
}

// ---------------------------------------------------------------------------
// Chained bare-bool ifs
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn bare_bool_chained_ifs() {
    let source = r#"
        fn main() -> i32 {
          let a = true
          let b = false
          let c = true
          if a {
            if b {
              ret 1
            } else {
              if c {
                ret 0
              }
            }
          }
          ret 2
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "chained bare bool ifs should evaluate correctly");
}

// ---------------------------------------------------------------------------
// Regression: `if flag == true` should still work after the fix
// ---------------------------------------------------------------------------
#[cfg(all(feature = "codegen", unix))]
#[test]
fn explicit_bool_comparison_still_works() {
    let source = r#"
        fn main() -> i32 {
          let flag = true
          if flag == true {
            ret 0
          }
          ret 1
        }
    "#;
    let (code, _) = build_and_run(source);
    assert_eq!(code, 0, "explicit == true should still work");
}

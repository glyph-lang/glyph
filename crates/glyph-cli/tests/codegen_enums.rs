use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;

#[test]
fn enum_constructor_codegen() {
    let source = r#"
enum Option {
  None
  Some(i32)
}

fn main() -> i32 {
  let x = Some(5);
  ret 0
}
"#;

    let output = compile_source(
        source,
        FrontendOptions {
            emit_mir: true,
            include_std: false,
        },
    );
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:?}",
        output.diagnostics
    );

    #[cfg(feature = "codegen")]
    {
        let backend = LlvmBackend::default();
        let artifact = backend
            .emit(
                &output.mir,
                &CodegenOptions {
                    emit: EmitKind::LlvmIr,
                    ..Default::default()
                },
            )
            .unwrap();
        let ir = artifact.llvm_ir.unwrap();
        assert!(
            ir.contains("%Option = type"),
            "expected enum type definition in IR\n{}",
            ir
        );
        assert!(
            ir.contains("enum.tag"),
            "expected tag store for enum constructor\n{}",
            ir
        );
        assert!(
            ir.contains("enum.payload"),
            "expected payload store for enum constructor\n{}",
            ir
        );
    }
}

#[test]
fn match_on_enum_generates_branches() {
    let source = r#"
import std

fn main() -> i32 {
  let x = Some(5);
  let y = match x {
    Some(v) => v,
    None => 0,
  };
  ret y
}
"#;

    let output = compile_source(
        source,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );
    assert!(
        output.diagnostics.is_empty(),
        "diagnostics: {:?}",
        output.diagnostics
    );

    #[cfg(feature = "codegen")]
    {
        let backend = LlvmBackend::default();
        let artifact = backend
            .emit(
                &output.mir,
                &CodegenOptions {
                    emit: EmitKind::LlvmIr,
                    ..Default::default()
                },
            )
            .unwrap();
        let ir = artifact.llvm_ir.unwrap();
        assert!(ir.contains("enum.tag"), "expected tag access in IR\n{}", ir);
        assert!(
            ir.contains("br i1"),
            "expected conditional branch in match lowering\n{}",
            ir
        );
    }
}

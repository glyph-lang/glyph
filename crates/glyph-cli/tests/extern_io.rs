use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;

#[test]
fn std_file_write_codegen() {
    let source = r#"import std

fn main() -> u32 {
    let file = std::io::fopen("out.txt", "w");
    let s = String::from_str("hi");
    std::io::write(s, 1, 2, file);
    ret 0
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
        let opts = CodegenOptions {
            emit: EmitKind::LlvmIr,
            ..Default::default()
        };
        let artifact = backend.emit(&output.mir, &opts).unwrap();
        let ir = artifact.llvm_ir.unwrap();

        assert!(
            ir.contains("declare"),
            "expected IR to contain declarations, got:\n{}",
            ir
        );
        assert!(
            ir.contains("@fopen"),
            "LLVM IR should declare fopen\n{}",
            ir
        );
        assert!(
            ir.contains("@fwrite"),
            "LLVM IR should declare fwrite via std::io::write\n{}",
            ir
        );
        assert!(
            ir.contains("@strdup"),
            "LLVM IR should declare strdup for String::from\n{}",
            ir
        );
        assert!(
            ir.contains("call") && ir.contains("fwrite"),
            "LLVM IR should call fwrite\n{}",
            ir
        );
    }
}

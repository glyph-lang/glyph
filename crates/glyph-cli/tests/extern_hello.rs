use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_frontend::{FrontendOptions, compile_source};

#[cfg(not(feature = "codegen"))]
use glyph_backend::NullBackend;
#[cfg(feature = "codegen")]
use glyph_backend::llvm::LlvmBackend;

#[test]
fn extern_putchar_compiles() {
    let source = r#"
        extern "C" fn putchar(c: i32) -> i32;

        fn print_h() -> i32 {
            putchar(72)
        }
    "#;

    let output = compile_source(source, FrontendOptions { emit_mir: true });

    // Should compile without errors
    assert!(
        output.diagnostics.is_empty(),
        "Compilation failed with diagnostics: {:?}",
        output.diagnostics
    );

    // Verify the extern function is in MIR
    assert_eq!(output.mir.extern_functions.len(), 1);
    assert_eq!(output.mir.extern_functions[0].name, "putchar");

    #[cfg(feature = "codegen")]
    {
        let backend = LlvmBackend::default();
        let opts = CodegenOptions {
            emit: EmitKind::LlvmIr,
            ..Default::default()
        };
        let artifact = backend.emit(&output.mir, &opts).unwrap();
        let ir = artifact.llvm_ir.unwrap();

        // Verify extern declaration in LLVM IR
        assert!(
            ir.contains("declare i32 @putchar(i32)"),
            "LLVM IR should contain extern declaration for putchar"
        );

        // Verify the function calls putchar
        assert!(
            ir.contains("call i32 @putchar"),
            "LLVM IR should contain call to putchar"
        );
    }
}

#[test]
fn extern_print_hello_compiles() {
    let source = r#"
        extern "C" fn putchar(c: i32) -> i32;

        fn print_hello() -> i32 {
            putchar(72);
            putchar(101);
            putchar(108);
            putchar(108);
            putchar(111);
            putchar(10);
            ret 0
        }
    "#;

    let output = compile_source(source, FrontendOptions { emit_mir: true });

    // Should compile without errors
    assert!(
        output.diagnostics.is_empty(),
        "Compilation failed with diagnostics: {:?}",
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

        // Verify multiple calls to putchar
        let putchar_calls = ir.matches("call i32 @putchar").count();
        assert!(
            putchar_calls >= 6,
            "Should have at least 6 calls to putchar, found {}",
            putchar_calls
        );
    }
}

#[test]
fn extern_puts_with_string_literal_codegen() {
    let source = r#"
        extern "C" fn puts(msg: str) -> i32;

        fn main() -> i32 {
            ret puts("Hello World!")
        }
    "#;

    let output = compile_source(source, FrontendOptions { emit_mir: true });
    assert!(
        output.diagnostics.is_empty(),
        "Compilation failed with diagnostics: {:?}",
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

        let decl_ok = ir.contains("declare i32 @puts(i8*)") || ir.contains("declare i32 @puts(ptr)");
        assert!(decl_ok, "LLVM IR should declare puts with pointer parameter\n{}", ir);
        assert!(ir.contains("call i32 @puts"), "LLVM IR should call puts");
        assert!(
            ir.contains(".str.main.0"),
            "LLVM IR should contain string literal global"
        );
    }
}

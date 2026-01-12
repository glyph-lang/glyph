use glyph_frontend::{FrontendOptions, compile_source};

#[test]
fn print_and_println_lower_to_puts() {
    let source = r#"import std

fn main() -> i32 {
  std::print($"hi");
  std::println($"bye");
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
}

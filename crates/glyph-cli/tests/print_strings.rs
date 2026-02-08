use glyph_frontend::{compile_source, FrontendOptions};

#[test]
fn print_accepts_runtime_strings() {
    let source = r#"import std

fn main() -> i32 {
  let s = String::from_str("hello")
  let t: str = "world"
  std::println(s)
  std::println(t)
  std::println($"s={s}")
  std::println(s.as_str())
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

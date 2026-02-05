use glyph_frontend::{compile_source, FrontendOptions};

#[test]
fn vec_get_returns_ref_option() {
    let src = r#"
from std/vec import Vec
from std/enums import Option

fn main() {
  let mut v: Vec<i32> = Vec::new()
  let maybe: Option<&i32> = v.get(0)
}
"#;

    let out = compile_source(
        src,
        FrontendOptions {
            emit_mir: true,
            include_std: true,
        },
    );

    assert!(
        out.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        out.diagnostics
    );
}

use std::collections::HashMap;

use glyph_core::ast::Module;
use glyph_frontend::{compile_modules, lex, parse, FrontendOptions};

fn parse_module(source: &str, module_id: &str) -> Module {
    let lex_out = lex(source);
    assert!(
        lex_out.diagnostics.is_empty(),
        "lex errors in {}: {:?}",
        module_id,
        lex_out.diagnostics
    );
    let parse_out = parse(&lex_out.tokens, source);
    assert!(
        parse_out.diagnostics.is_empty(),
        "parse errors in {}: {:?}",
        module_id,
        parse_out.diagnostics
    );
    parse_out.module
}

#[test]
fn imported_nested_field_access() {
    let models_src = r#"
struct Inner {
  value: i32
}

struct Outer {
  inner: Inner
}

fn make_outer() -> Outer {
  ret Outer { inner: Inner { value: 1 } }
}
"#;

    let main_src = r#"
from models import Outer, make_outer

fn main() -> i32 {
  let mut obj = make_outer()
  obj.inner.value = 3
  let v = obj.inner.value
  let mut obj2 = make_outer()
  let mut inner = obj2.inner
  inner.value = v
  ret inner.value
}
"#;

    let mut modules = HashMap::new();
    modules.insert("models".to_string(), parse_module(models_src, "models"));
    modules.insert("main".to_string(), parse_module(main_src, "main"));

    let out = compile_modules(
        modules,
        "main",
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

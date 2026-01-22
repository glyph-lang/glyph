// Parser regression tests
// Automatically load all fixtures under tests/fixtures/parse/*.glyph and ensure they parse.

use std::fs;
use std::path::Path;

#[test]
fn parse_fixtures() {
    let fixtures_dir = Path::new("tests/fixtures/parse");
    for entry in fs::read_dir(fixtures_dir).expect("read fixtures") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.extension().map(|s| s == "glyph").unwrap_or(false) {
            let src = fs::read_to_string(&path).expect("read fixture");
            let lex_out = glyph_frontend::lex(&src);
            assert!(
                lex_out.diagnostics.is_empty(),
                "lex errors in {:?}: {:?}",
                path,
                lex_out.diagnostics
            );
            let parse_out = glyph_frontend::parse(&lex_out.tokens, &src);
            assert!(
                parse_out.diagnostics.is_empty(),
                "parse errors in {:?}: {:?}",
                path,
                parse_out.diagnostics
            );
        }
    }
}

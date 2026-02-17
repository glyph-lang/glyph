use std::fs;
use std::path::Path;
use std::process::Command;

use assert_cmd::cargo::cargo_bin_cmd;
use tempfile::TempDir;

fn write_file(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

#[test]
fn build_with_local_path_dependency_succeeds() {
    let temp = TempDir::new().unwrap();
    let workspace = temp.path();
    let lib_dir = workspace.join("my_lib");
    let app_dir = workspace.join("my_app");

    fs::create_dir_all(&lib_dir).unwrap();
    fs::create_dir_all(&app_dir).unwrap();

    write_file(
        &lib_dir.join("glyph.toml"),
        r#"[package]
name = "my_lib"
version = "0.1.0"

[lib]
path = "src/lib.glyph"
"#,
    );
    write_file(
        &lib_dir.join("src/lib.glyph"),
        r#"from utils import helper

fn greet() -> i32 {
  ret helper()
}
"#,
    );
    write_file(
        &lib_dir.join("src/utils.glyph"),
        r#"fn helper() -> i32 {
  ret 7
}
"#,
    );

    write_file(
        &app_dir.join("glyph.toml"),
        r#"[package]
name = "my_app"
version = "0.1.0"

[dependencies]
my_lib = { path = "../my_lib" }

[[bin]]
name = "my_app"
path = "src/main.glyph"
"#,
    );
    write_file(
        &app_dir.join("src/main.glyph"),
        r#"from std import println
from my_lib import greet

fn main() -> i32 {
  greet()
  println("hi")
  ret 0
}
"#,
    );

    let mut cmd = cargo_bin_cmd!("glyph");
    cmd.current_dir(&app_dir).arg("build").assert().success();

    let exe = app_dir.join("target").join("debug").join("my_app");
    let output = Command::new(&exe).output().unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hi"));
}

use std::process::Command;

fn git(args: &[&str]) -> Option<String> {
    let out = Command::new("git").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8(out.stdout).ok()?;
    let s = s.trim().to_string();
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

fn is_dirty() -> bool {
    // Best-effort: if git isn't available, assume clean.
    let diff = Command::new("git").args(["diff", "--quiet"]).status();
    let diff_cached = Command::new("git")
        .args(["diff", "--cached", "--quiet"])
        .status();
    match (diff, diff_cached) {
        (Ok(a), Ok(b)) => !(a.success() && b.success()),
        _ => false,
    }
}

fn main() {
    // Rebuild if the git state changes.
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/index");

    let pkg_version = std::env::var("CARGO_PKG_VERSION").unwrap_or_else(|_| "0.0.0".to_string());

    let id = git(&["describe", "--tags", "--exact-match"])
        .or_else(|| git(&["rev-parse", "--short=7", "HEAD"]));

    let mut full = pkg_version.clone();
    if let Some(mut id) = id {
        if is_dirty() {
            id.push_str("-dirty");
        }
        full = format!("{} ({})", pkg_version, id);
    }

    println!("cargo:rustc-env=GLYPH_BUILD_VERSION={}", full);
}

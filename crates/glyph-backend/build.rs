use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Get the output directory where cargo builds artifacts
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR not set"));

    // Path to the runtime C source file (relative to glyph-backend crate root)
    let runtime_src = PathBuf::from("../../runtime/glyph_fmt.c");

    // Output paths for compiled object and static library
    let runtime_obj = out_dir.join("glyph_fmt.o");
    let runtime_lib = out_dir.join("libglyph_runtime.a");

    // Check if runtime source exists
    if !runtime_src.exists() {
        panic!("Runtime library source not found at: {:?}", runtime_src);
    }

    // Compile runtime C code to object file using cc
    println!(
        "cargo:warning=Compiling runtime library from {:?}",
        runtime_src
    );
    let status = Command::new("cc")
        .args(&[
            "-c",    // Compile only, don't link
            "-O2",   // Optimize
            "-fPIC", // Position-independent code for shared libraries
            "-Wall", // Enable warnings
        ])
        .arg(&runtime_src)
        .arg("-o")
        .arg(&runtime_obj)
        .status()
        .expect("Failed to execute cc compiler");

    if !status.success() {
        panic!("Failed to compile runtime library. Make sure cc (clang/gcc) is installed.");
    }

    // Create static library archive from object file using ar
    println!("cargo:warning=Creating static library at {:?}", runtime_lib);
    let status = Command::new("ar")
        .args(&["rcs"]) // r=insert, c=create, s=index
        .arg(&runtime_lib)
        .arg(&runtime_obj)
        .status()
        .expect("Failed to execute ar archiver");

    if !status.success() {
        panic!("Failed to create static library. Make sure ar is installed.");
    }

    // Tell cargo where to find the runtime library
    println!("cargo:rustc-link-search=native={}", out_dir.display());
    println!("cargo:rustc-link-lib=static=glyph_runtime");

    // Rerun build script if runtime source changes
    println!("cargo:rerun-if-changed=../../runtime/glyph_fmt.c");

    println!(
        "cargo:warning=Runtime library built successfully at {}",
        runtime_lib.display()
    );
}

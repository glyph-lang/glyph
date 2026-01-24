use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Get the output directory where cargo builds artifacts
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR not set"));

    // Paths to runtime C source files (relative to glyph-backend crate root)
    let glyph_fmt_src = PathBuf::from("../../runtime/glyph_fmt.c");
    let glyph_json_src = PathBuf::from("../../runtime/glyph_json.c");
    let glyph_process_src = PathBuf::from("../../runtime/glyph_process.c");

    // Output paths for compiled objects and static library
    let glyph_fmt_obj = out_dir.join("glyph_fmt.o");
    let glyph_json_obj = out_dir.join("glyph_json.o");
    let glyph_process_obj = out_dir.join("glyph_process.o");
    let runtime_lib = out_dir.join("libglyph_runtime.a");

    // Check if runtime sources exist
    if !glyph_fmt_src.exists() {
        panic!("Runtime library source not found at: {:?}", glyph_fmt_src);
    }
    if !glyph_json_src.exists() {
        panic!("Runtime library source not found at: {:?}", glyph_json_src);
    }
    if !glyph_process_src.exists() {
        panic!(
            "Runtime library source not found at: {:?}",
            glyph_process_src
        );
    }

    // Compile glyph_fmt.c to object file
    println!(
        "cargo:warning=Compiling runtime library from {:?}",
        glyph_fmt_src
    );
    let status = Command::new("cc")
        .args(&[
            "-c",    // Compile only, don't link
            "-O2",   // Optimize
            "-fPIC", // Position-independent code for shared libraries
            "-Wall", // Enable warnings
        ])
        .arg(&glyph_fmt_src)
        .arg("-o")
        .arg(&glyph_fmt_obj)
        .status()
        .expect("Failed to execute cc compiler");

    if !status.success() {
        panic!("Failed to compile glyph_fmt.c. Make sure cc (clang/gcc) is installed.");
    }

    // Compile glyph_json.c to object file
    println!(
        "cargo:warning=Compiling runtime library from {:?}",
        glyph_json_src
    );
    let status = Command::new("cc")
        .args(&[
            "-c",    // Compile only, don't link
            "-O2",   // Optimize
            "-fPIC", // Position-independent code for shared libraries
            "-Wall", // Enable warnings
        ])
        .arg(&glyph_json_src)
        .arg("-o")
        .arg(&glyph_json_obj)
        .status()
        .expect("Failed to execute cc compiler");

    if !status.success() {
        panic!("Failed to compile glyph_json.c. Make sure cc (clang/gcc) is installed.");
    }

    // Compile glyph_process.c to object file
    println!(
        "cargo:warning=Compiling runtime library from {:?}",
        glyph_process_src
    );
    let status = Command::new("cc")
        .args(&[
            "-c",    // Compile only, don't link
            "-O2",   // Optimize
            "-fPIC", // Position-independent code for shared libraries
            "-Wall", // Enable warnings
        ])
        .arg(&glyph_process_src)
        .arg("-o")
        .arg(&glyph_process_obj)
        .status()
        .expect("Failed to execute cc compiler");

    if !status.success() {
        panic!("Failed to compile glyph_process.c. Make sure cc (clang/gcc) is installed.");
    }

    // Create static library archive from both object files using ar
    println!("cargo:warning=Creating static library at {:?}", runtime_lib);
    let status = Command::new("ar")
        .args(&["rcs"]) // r=insert, c=create, s=index
        .arg(&runtime_lib)
        .arg(&glyph_fmt_obj)
        .arg(&glyph_json_obj)
        .arg(&glyph_process_obj)
        .status()
        .expect("Failed to execute ar archiver");

    if !status.success() {
        panic!("Failed to create static library. Make sure ar is installed.");
    }

    // Tell cargo where to find the runtime library
    println!("cargo:rustc-link-search=native={}", out_dir.display());
    println!("cargo:rustc-link-lib=static=glyph_runtime");

    // Rerun build script if runtime sources change
    println!("cargo:rerun-if-changed=../../runtime/glyph_fmt.c");
    println!("cargo:rerun-if-changed=../../runtime/glyph_json.c");
    println!("cargo:rerun-if-changed=../../runtime/glyph_process.c");

    println!(
        "cargo:warning=Runtime library built successfully at {}",
        runtime_lib.display()
    );
}

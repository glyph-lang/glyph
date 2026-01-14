#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

heading() {
  printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%s\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n" "$1"
}

status() {
  printf "[%s] %s\n" "$1" "$2"
}

# Detect LLVM for llvm-sys (override with existing env)
detect_llvm_prefix() {
  if [[ -n "${LLVM_SYS_201_PREFIX:-}" ]]; then
    echo "$LLVM_SYS_201_PREFIX"
    return 0
  fi
  if command -v llvm-config >/dev/null 2>&1; then
    llvm-config --prefix && return 0
  fi
  for prefix in /opt/homebrew/opt/llvm /usr/local/opt/llvm; do
    if [[ -d "$prefix" ]]; then
      echo "$prefix"
      return 0
    fi
  done
  return 1
}

if prefix=$(detect_llvm_prefix); then
  export LLVM_SYS_201_PREFIX="$prefix"
  status "info" "LLVM_SYS_201_PREFIX set to $prefix"
else
  status "warn" "Could not auto-detect LLVM; if build fails, set LLVM_SYS_201_PREFIX manually"
fi

heading "Building glyph-cli (release, codegen enabled)"
cargo build --release --bin glyph-cli --features codegen

heading "Demo 1: Check + type-resolve a stdlib hello"
cat examples/std_hello/hello.glyph
cargo run --quiet --bin glyph-cli -- check examples/std_hello/hello.glyph

heading "Demo 2: Build + run stdlib hello"
cargo run --quiet --bin glyph-cli --features codegen -- run examples/std_hello/hello.glyph

heading "Demo 3: Emit LLVM IR for a tiny function"
cargo run --quiet --bin glyph-cli --features codegen -- build tests/fixtures/codegen/simple_ret.glyph

heading "Demo 4: Native executable (puts-based hello)"
cargo run --quiet --bin glyph-cli --features codegen -- build examples/puts_hello/hello.glyph --emit exe
./examples/puts_hello/hello

heading "Done"
status "info" "Demos completed"

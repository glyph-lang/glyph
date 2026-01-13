#!/bin/bash
# Build script for the Glyph stdlib println example

set -e  # Exit on error

echo "Building stdlib println hello world example..."

# Build the executable
cargo run --bin glyph-cli --features codegen -- build hello.glyph --emit exe

echo "Build complete!"
echo ""
echo "Run the program with: ./hello"
echo "Or use: cargo run --bin glyph-cli --features codegen -- run hello.glyph"

#!/bin/bash
# Demo script for Glyph compiler

set -e

# Set LLVM path
export LLVM_SYS_201_PREFIX=/opt/homebrew/Cellar/llvm/20.1.8

echo "╔════════════════════════════════════════════════════════╗"
echo "║       Glyph Compiler Demo - Working Prototype!        ║"
echo "╚════════════════════════════════════════════════════════╝"
echo

# Build if needed
if [ ! -f "target/release/glyph-cli" ]; then
    echo "Building Glyph compiler..."
    cargo build --release --quiet
    echo "✓ Build complete"
    echo
fi

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Demo 1: Simple Function Returning a Constant"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Source code (simple_ret.glyph):"
echo "────────────────────────────────"
cat tests/fixtures/codegen/simple_ret.glyph
echo
echo "Generated LLVM IR:"
echo "────────────────────────────────"
./target/release/glyph-cli build tests/fixtures/codegen/simple_ret.glyph
echo

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Demo 2: Function with Parameters and Arithmetic"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Source code (add_function.glyph):"
echo "────────────────────────────────"
cat tests/fixtures/codegen/add_function.glyph
echo
echo "Generated LLVM IR:"
echo "────────────────────────────────"
./target/release/glyph-cli build tests/fixtures/codegen/add_function.glyph
echo

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Demo 3: Control Flow (If/Else)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Source code (if_else.glyph):"
echo "────────────────────────────────"
cat tests/fixtures/mir/if_else.glyph
echo
echo "Generated LLVM IR:"
echo "────────────────────────────────"
./target/release/glyph-cli build tests/fixtures/mir/if_else.glyph
echo

echo "╔════════════════════════════════════════════════════════╗"
echo "║                    ✓ Demo Complete!                   ║"
echo "╚════════════════════════════════════════════════════════╝"
echo
echo "What's Working:"
echo "  ✓ Lexing and parsing"
echo "  ✓ MIR lowering (SSA-like IR)"
echo "  ✓ LLVM IR code generation"
echo "  ✓ JIT execution for testing"
echo "  ✓ Function parameters and return types"
echo "  ✓ Arithmetic and comparison operations"
echo "  ✓ Control flow (if/else)"
echo
echo "Next Steps:"
echo "  → Struct definitions and literals"
echo "  → Field access"
echo "  → Full type checking"
echo "  → Native executable generation"
echo

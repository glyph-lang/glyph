# Hello World in Glyph

Glyph now has `extern "C"` support, enabling you to call C library functions!

## Current State: Character-by-Character Output

Since Glyph doesn't have string literals yet, you can print "Hello World" by calling `putchar` for each character:

```glyph
extern "C" fn putchar(c: i32) -> i32;

fn main() -> i32 {
    putchar(72);  // 'H'
    putchar(101); // 'e'
    putchar(108); // 'l'
    putchar(108); // 'l'
    putchar(111); // 'o'
    putchar(32);  // ' '
    putchar(87);  // 'W'
    putchar(111); // 'o'
    putchar(114); // 'r'
    putchar(108); // 'l'
    putchar(100); // 'd'
    putchar(33);  // '!'
    putchar(10);  // '\n'
    ret 0
}
```

## How to Compile

```bash
# Build to LLVM IR (default)
cargo run --features codegen -- build tests/fixtures/extern_putchar.glyph

# Build to object file (requires linking - not yet implemented)
cargo run --features codegen -- build tests/fixtures/extern_putchar.glyph --emit obj
```

## JIT Execution (Testing)

For testing, you can execute extern functions in the JIT by providing symbol mappings:

```rust
// In Rust test code:
let mut symbols = HashMap::new();
symbols.insert("putchar".to_string(), libc::putchar as u64);
ctx.jit_execute_i32_with_symbols("main", &symbols)?;
```

See `crates/glyph-backend/src/codegen.rs::jit_hello_world_with_putchar` for a working example.

## String Support Design (v0)

### Design Decisions

**String Literal Type: `&str`**
- String literals have type `&str` (fat pointer with ptr + length)
- Static strings are stored in `.rodata` as null-terminated byte arrays
- Length is tracked for safety and bounds checking

**Memory Layout:**
```
&str = { ptr: RawPtr<u8>, len: usize }
```

**FFI Conversion:**
- Automatic conversion to `RawPtr<u8>` when passing `&str` to `extern "C"` functions
- Compiler inserts `.as_ptr()` call implicitly for C interop

**String Types:**
1. `&str` - Immutable string reference (string literals, slices)
2. `String` - Owned, heap-allocated string type (like Rust's String)

### Implementation Roadmap

#### Phase 1: String Literals (`&str`)

**Goal:** Enable `print("Hello World!")` with string literals

**1.1 AST Extensions (glyph-core/src/lib.rs):**
```rust
pub enum Expr {
    // ... existing variants
    StringLit(String, Span),  // The actual string content
}

// String slice type representation
pub enum Type {
    // ... existing variants
    Str,  // Represents &str (fat pointer: ptr + len)
}
```

**1.2 Parser (glyph-frontend/src/parser.rs):**
```rust
fn parse_primary_expression(&mut self) -> Result<Expr, ParseError> {
    match self.current_token() {
        TokenKind::Str => {
            let content = self.current_token_text().to_string();
            let span = self.current_span();
            self.advance();
            Ok(Expr::StringLit(content, span))
        }
        // ... existing cases
    }
}
```

**1.3 MIR Extensions (glyph-core/src/mir.rs):**
```rust
pub enum Rvalue {
    // ... existing variants
    StringLit {
        content: String,      // UTF-8 string content
        global_name: String,  // Generated global symbol name
    },
}
```

**1.4 MIR Lowering (glyph-frontend/src/mir_lower.rs):**
```rust
fn lower_expr(&mut self, expr: &Expr) -> Result<MirValue, Diagnostic> {
    match expr {
        Expr::StringLit(s, _) => {
            let global_name = format!(".str.{}", self.string_counter);
            self.string_counter += 1;

            let temp = self.new_temp_local(Type::Str);
            self.emit_assign(temp, Rvalue::StringLit {
                content: s.clone(),
                global_name,
            });
            Ok(MirValue::Local(temp))
        }
        // ... existing cases
    }
}
```

**1.5 Codegen - Global Strings (glyph-backend/src/codegen.rs):**
```rust
// Add to CodegenContext struct:
string_globals: HashMap<String, LLVMValueRef>,

// Codegen for string literals
fn codegen_string_literal(&mut self, content: &str, global_name: &str) -> Result<LLVMValueRef> {
    // Check if already generated
    if let Some(&global) = self.string_globals.get(global_name) {
        return Ok(global);
    }

    unsafe {
        // Create null-terminated string constant
        let bytes: Vec<u8> = content.bytes().chain(std::iter::once(0)).collect();
        let str_ty = LLVMArrayType(LLVMInt8TypeInContext(self.context), bytes.len() as u32);

        // Create global constant in .rodata
        let name_c = CString::new(global_name)?;
        let global = LLVMAddGlobal(self.module, str_ty, name_c.as_ptr());
        LLVMSetGlobalConstant(global, 1);
        LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);

        // Initialize with string data
        let mut const_bytes: Vec<LLVMValueRef> = bytes.iter()
            .map(|&b| LLVMConstInt(LLVMInt8TypeInContext(self.context), b as u64, 0))
            .collect();
        let init_val = LLVMConstArray(
            LLVMInt8TypeInContext(self.context),
            const_bytes.as_mut_ptr(),
            bytes.len() as u32,
        );
        LLVMSetInitializer(global, init_val);

        // Store for reuse
        self.string_globals.insert(global_name.to_string(), global);

        // Return fat pointer { ptr, len }
        // For now, return just the pointer (GEP to first element)
        // Full &str fat pointer support requires struct return
        let i32_ty = LLVMInt32TypeInContext(self.context);
        let zero = LLVMConstInt(i32_ty, 0, 0);
        let mut indices = vec![zero, zero];
        let ptr = LLVMConstGEP2(str_ty, global, indices.as_mut_ptr(), 2);

        Ok(ptr)
    }
}
```

**1.6 FFI Auto-Conversion:**
When calling `extern "C"` functions, automatically convert `&str` to `RawPtr<u8>`:

```rust
// In codegen_rvalue for Rvalue::Call
fn codegen_call_with_ffi_conversion(&mut self, ...) -> Result<LLVMValueRef> {
    for (i, (arg, param_ty)) in args.iter().zip(param_types.iter()).enumerate() {
        let arg_val = self.codegen_value(arg, func, local_map)?;
        let arg_ty = self.infer_value_type(arg);

        // Auto-convert &str to RawPtr<u8> for extern C
        let converted = if matches!(arg_ty, Type::Str) &&
                          matches!(param_ty, Type::RawPtr(_)) &&
                          is_extern_c {
            // Extract pointer from &str fat pointer
            arg_val  // Already a pointer from codegen_string_literal
        } else {
            arg_val
        };

        llvm_args.push(converted);
    }
    // ... rest of call codegen
}
```

#### Phase 2: Owned Strings (`String`)

**Goal:** Enable heap-allocated mutable strings

**2.1 Type System:**
```rust
pub enum Type {
    // ... existing
    Str,      // &str - immutable reference
    String,   // String - owned, heap-allocated
}

// Or reuse existing Own type:
// Own<[u8]> for owned byte buffers
```

**2.2 String Methods:**
```glyph
// String creation
fn String::new() -> String;
fn String::from(s: &str) -> String;

// Conversion
fn str_to_string(s: &str) -> String;
fn string_as_str(s: &String) -> &str;

// Methods
impl String {
    fn push_str(&mut self, s: &str);
    fn len(&self) -> i32;
    fn as_str(&self) -> &str;
}
```

**2.3 Memory Management:**
- Use existing `Own<T>` infrastructure for heap allocation
- String = `Own<[u8]>` internally with length tracking
- Drop implementation calls `free()` like other `Own` types

### Example: Final Syntax

```glyph
extern "C" fn puts(s: RawPtr<u8>) -> i32;

fn main() -> i32 {
    // String literal (automatic conversion to RawPtr<u8>)
    puts("Hello World!");

    // Explicit &str usage
    let greeting: &str = "Hello";
    puts(greeting);  // Auto-converts to RawPtr<u8>

    // Owned String (Phase 2)
    let mut s = String::from("Hello");
    s.push_str(" World!");
    puts(s.as_str());  // Convert String -> &str -> RawPtr<u8>

    ret 0
}
```

### Implementation Order

1. ✅ **Extern C support** - Complete (Phase 8)
2. 🔄 **String literals (`&str`)** - Next priority
   - Parser: `Expr::StringLit`
   - MIR: `Rvalue::StringLit`
   - Codegen: Global strings + fat pointer
   - FFI: Auto-conversion to `RawPtr<u8>`
3. ⏸️ **Owned strings (`String`)** - After literals work
   - Reuse `Own<[u8]>` or create dedicated `String` type
   - String methods and conversions
   - Integration with existing memory management

## Working Examples

See:
- `tests/fixtures/extern_putchar.glyph` - Character output example
- `crates/glyph-cli/tests/extern_hello.rs` - Compilation tests
- `crates/glyph-backend/src/codegen.rs::jit_hello_world_with_putchar` - JIT test

## CLI Flags for Linking (Phase 8 - Partial)

CLI flags are implemented but actual linking is not:

```bash
# These flags are parsed but not yet used for linking:
cargo run -- build main.glyph --link-lib c --link-search /usr/lib
```

**TODO:** Invoke system linker (ld/clang) to link object files with specified libraries.

# Multi-Module Compilation - Remaining Implementation Tasks

## Overview
This document provides step-by-step instructions for completing Phases 4-7 of the import system implementation. Phases 0-3 are already complete (tokens, parser, dependency graph).

**Current Status:**
- ✅ Phase 0: Foundation (Tokens + AST)
- ✅ Phase 1: Parser Implementation
- ✅ Phase 2: Multi-file Discovery
- ✅ Phase 3: Dependency Graph & Cycle Detection
- ⏳ Phase 4: Import Scope Resolution
- ⏳ Phase 5: Multi-Module Type Resolution
- ⏳ Phase 6: MIR Lowering Updates
- ⏳ Phase 7: End-to-End Integration

---

## Phase 4: Import Scope Resolution & Collision Detection

### Goal
Build import scopes for each module and detect name collisions when multiple selective imports bring the same symbol into scope.

### Step 4.1: Add Import Scope Types to `module_resolver.rs`

**File:** `crates/glyph-frontend/src/module_resolver.rs`

**Location:** After the `build_dependency_graph()` function (around line 160)

**Add these type definitions:**

```rust
use glyph_core::ast::{Item, Ident};

/// Import scope tracks which symbols are accessible in a module
#[derive(Debug, Clone)]
pub struct ImportScope {
    /// Selective imports: local_name -> (source_module, original_name)
    /// Example: "import Point as P from geometry" -> "P" -> ("geometry", "Point")
    pub direct_symbols: HashMap<String, (String, String)>,

    /// Wildcard imports: module names that can be used for qualified access
    /// Example: "import math" -> "math" in this set
    pub wildcard_modules: HashSet<String>,
}

/// Multi-module context containing all parsed modules and their scopes
#[derive(Debug, Clone)]
pub struct MultiModuleContext {
    /// All parsed modules: module_id -> Module AST
    pub modules: HashMap<String, Module>,

    /// Import scopes: module_id -> ImportScope
    pub import_scopes: HashMap<String, ImportScope>,

    /// Module symbols: module_id -> ModuleSymbols
    pub module_symbols: HashMap<String, ModuleSymbols>,
}

/// Symbols exported by a module
#[derive(Debug, Clone)]
pub struct ModuleSymbols {
    pub structs: HashSet<String>,
    pub interfaces: HashSet<String>,
    pub functions: HashSet<String>,
}

impl ModuleSymbols {
    pub fn new() -> Self {
        Self {
            structs: HashSet::new(),
            interfaces: HashSet::new(),
            functions: HashSet::new(),
        }
    }
}
```

### Step 4.2: Implement `collect_module_symbols()`

**Location:** After the type definitions you just added

**Add this function:**

```rust
/// Collect all symbols (structs, interfaces, functions) from each module
fn collect_module_symbols(modules: &HashMap<String, Module>) -> HashMap<String, ModuleSymbols> {
    let mut symbols_map = HashMap::new();

    for (module_id, module) in modules {
        let mut symbols = ModuleSymbols::new();

        for item in &module.items {
            match item {
                Item::Struct(s) => {
                    symbols.structs.insert(s.name.0.clone());
                }
                Item::Interface(i) => {
                    symbols.interfaces.insert(i.name.0.clone());
                }
                Item::Function(f) => {
                    symbols.functions.insert(f.name.0.clone());
                }
                Item::Impl(_) => {
                    // Methods are handled separately in resolver
                }
            }
        }

        symbols_map.insert(module_id.clone(), symbols);
    }

    symbols_map
}
```

### Step 4.3: Implement `build_import_scopes()`

**Location:** After `collect_module_symbols()`

**Add this function:**

```rust
use glyph_core::ast::ImportKind;

/// Build import scopes for each module and detect collisions
fn build_import_scopes(
    modules: &HashMap<String, Module>,
    module_symbols: &HashMap<String, ModuleSymbols>,
) -> Result<HashMap<String, ImportScope>, Vec<Diagnostic>> {
    let mut scopes = HashMap::new();
    let mut diagnostics = Vec::new();

    for (module_id, module) in modules {
        let mut scope = ImportScope {
            direct_symbols: HashMap::new(),
            wildcard_modules: HashSet::new(),
        };

        for import in &module.imports {
            let source_module = import.path.segments.join("/");

            match &import.kind {
                ImportKind::Wildcard => {
                    // Wildcard import: just track the module name for qualified access
                    scope.wildcard_modules.insert(source_module.clone());
                }
                ImportKind::Selective { items } => {
                    // Selective import: bring specific symbols into scope
                    for item in items {
                        let local_name = item.alias.as_ref()
                            .unwrap_or(&item.name)
                            .0.clone();
                        let original_name = item.name.0.clone();

                        // Check if symbol exists in source module
                        if let Some(symbols) = module_symbols.get(&source_module) {
                            let exists = symbols.structs.contains(&original_name)
                                || symbols.interfaces.contains(&original_name)
                                || symbols.functions.contains(&original_name);

                            if !exists {
                                diagnostics.push(Diagnostic::error(
                                    format!(
                                        "symbol '{}' not found in module '{}'",
                                        original_name, source_module
                                    ),
                                    Some(item.span),
                                ));
                                continue;
                            }
                        }

                        // Check for collision with existing imports
                        if let Some((existing_module, _)) = scope.direct_symbols.get(&local_name) {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "symbol '{}' conflicts: imported from both '{}' and '{}'. Use aliasing to resolve: 'import {} as {} from {}'",
                                    local_name, existing_module, source_module,
                                    local_name, format!("{}_{}", source_module.replace("/", "_"), local_name), source_module
                                ),
                                Some(item.span),
                            ));
                        } else {
                            scope.direct_symbols.insert(
                                local_name,
                                (source_module.clone(), original_name),
                            );
                        }
                    }
                }
            }
        }

        scopes.insert(module_id.clone(), scope);
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    Ok(scopes)
}
```

### Step 4.4: Implement `resolve_multi_module()`

**Location:** After `build_import_scopes()`

**Add this main entry point function:**

```rust
use std::path::Path;

/// Main entry point for multi-module resolution
/// Returns the multi-module context and compilation order
pub fn resolve_multi_module(
    modules: HashMap<String, Module>,
    _root_dir: &Path, // Reserved for future use
) -> Result<(MultiModuleContext, Vec<String>), Vec<Diagnostic>> {
    // Step 1: Build dependency graph
    let dep_graph = build_dependency_graph(&modules)?;

    // Step 2: Detect circular imports
    if let Some(cycle) = dep_graph.find_cycle() {
        return Err(vec![Diagnostic::error(
            format!("circular import detected: {}", cycle.join(" → ")),
            None,
        )]);
    }

    // Step 3: Topological sort for compilation order
    let compile_order = dep_graph.topological_sort()
        .map_err(|cycle| {
            vec![Diagnostic::error(
                format!("circular import: {}", cycle.join(" → ")),
                None,
            )]
        })?;

    // Step 4: Collect symbols from all modules
    let module_symbols = collect_module_symbols(&modules);

    // Step 5: Build import scopes (with collision detection)
    let import_scopes = build_import_scopes(&modules, &module_symbols)?;

    let context = MultiModuleContext {
        modules,
        import_scopes,
        module_symbols,
    };

    Ok((context, compile_order))
}
```

### Step 4.5: Export New Types

**File:** `crates/glyph-frontend/src/lib.rs`

**Location:** Line 17 (in the `pub use` section)

**Update the export:**

```rust
pub use module_resolver::{
    DependencyGraph,
    build_dependency_graph,
    resolve_multi_module,      // NEW
    MultiModuleContext,        // NEW
    ImportScope,               // NEW
    ModuleSymbols,             // NEW
};
```

### Step 4.6: Add Tests

**File:** `crates/glyph-frontend/src/module_resolver.rs`

**Location:** At the end of the `tests` module (before the closing `}`)

**Add these tests:**

```rust
#[test]
fn collect_symbols_from_module() {
    use glyph_core::ast::{StructDef, Function, Param, Block};
    use glyph_core::span::Span;

    let mut modules = HashMap::new();
    modules.insert(
        "geometry".to_string(),
        Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: vec![],
                    methods: vec![],
                    inline_impls: vec![],
                    span: Span::new(0, 0),
                }),
                Item::Function(Function {
                    name: Ident("distance".into()),
                    params: vec![],
                    ret_type: None,
                    body: Block { stmts: vec![], span: Span::new(0, 0) },
                    span: Span::new(0, 0),
                }),
            ],
        },
    );

    let symbols = collect_module_symbols(&modules);
    let geom_symbols = symbols.get("geometry").unwrap();

    assert!(geom_symbols.structs.contains("Point"));
    assert!(geom_symbols.functions.contains("distance"));
}

#[test]
fn build_scopes_detects_collision() {
    use glyph_core::ast::{ImportItem, StructDef};
    use glyph_core::span::Span;

    let mut modules = HashMap::new();

    // Module with two structs
    modules.insert(
        "geo".to_string(),
        Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: vec![],
                    methods: vec![],
                    inline_impls: vec![],
                    span: Span::new(0, 0),
                }),
            ],
        },
    );

    modules.insert(
        "graphics".to_string(),
        Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: vec![],
                    methods: vec![],
                    inline_impls: vec![],
                    span: Span::new(0, 0),
                }),
            ],
        },
    );

    // Module importing Point from both (collision!)
    modules.insert(
        "main".to_string(),
        Module {
            imports: vec![
                Import {
                    kind: ImportKind::Selective {
                        items: vec![ImportItem {
                            name: Ident("Point".into()),
                            alias: None,
                            span: Span::new(0, 5),
                        }],
                    },
                    path: ImportPath {
                        segments: vec!["geo".to_string()],
                        span: Span::new(0, 0),
                    },
                    span: Span::new(0, 0),
                },
                Import {
                    kind: ImportKind::Selective {
                        items: vec![ImportItem {
                            name: Ident("Point".into()),
                            alias: None,
                            span: Span::new(10, 15),
                        }],
                    },
                    path: ImportPath {
                        segments: vec!["graphics".to_string()],
                        span: Span::new(0, 0),
                    },
                    span: Span::new(0, 0),
                },
            ],
            items: vec![],
        },
    );

    let symbols = collect_module_symbols(&modules);
    let result = build_import_scopes(&modules, &symbols);

    assert!(result.is_err());
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.iter().any(|d| d.message.contains("conflicts")));
}

#[test]
fn resolve_multi_module_success() {
    use glyph_core::ast::StructDef;
    use glyph_core::span::Span;
    use std::path::Path;

    let mut modules = HashMap::new();

    modules.insert(
        "utils".to_string(),
        Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Helper".into()),
                    fields: vec![],
                    interfaces: vec![],
                    methods: vec![],
                    inline_impls: vec![],
                    span: Span::new(0, 0),
                }),
            ],
        },
    );

    modules.insert(
        "main".to_string(),
        Module {
            imports: vec![Import {
                kind: ImportKind::Wildcard,
                path: ImportPath {
                    segments: vec!["utils".to_string()],
                    span: Span::new(0, 0),
                },
                span: Span::new(0, 0),
            }],
            items: vec![],
        },
    );

    let result = resolve_multi_module(modules, Path::new("."));
    assert!(result.is_ok());

    let (context, order) = result.unwrap();

    // Verify compilation order: utils before main
    let utils_pos = order.iter().position(|m| m == "utils").unwrap();
    let main_pos = order.iter().position(|m| m == "main").unwrap();
    assert!(utils_pos < main_pos);

    // Verify import scope
    let main_scope = context.import_scopes.get("main").unwrap();
    assert!(main_scope.wildcard_modules.contains("utils"));
}
```

### Step 4.7: Verify Phase 4

**Run tests:**
```bash
cargo test -p glyph-frontend module_resolver
```

**Expected output:**
```
test result: ok. 11 passed; 0 failed; 0 ignored
```

**Manual verification:**
```bash
# Create test files
mkdir -p /tmp/phase4_test
cat > /tmp/phase4_test/geo.glyph << 'EOF'
struct Point { x: i32, y: i32 }
EOF

cat > /tmp/phase4_test/graphics.glyph << 'EOF'
struct Point { x: f32, y: f32 }
EOF

cat > /tmp/phase4_test/main.glyph << 'EOF'
import Point from geo
import Point from graphics

fn main() -> i32 { ret 0 }
EOF

# This should fail with collision error (Phase 7 integration)
cargo run -p glyph-cli check /tmp/phase4_test/main.glyph
```

---

## Phase 5: Multi-Module Type Resolution

### Goal
Extend the `ResolverContext` to support resolving symbols across modules using import scopes.

### Step 5.1: Add Multi-Module Fields to `ResolverContext`

**File:** `crates/glyph-frontend/src/resolver.rs`

**Location:** Find the `ResolverContext` struct (around line 12)

**Add these fields at the end of the struct:**

```rust
pub struct ResolverContext {
    // ... existing fields ...
    pub interface_impls: HashMap<String, HashMap<String, HashMap<String, MethodInfo>>>,

    // NEW: Multi-module support
    pub current_module: Option<String>,
    pub import_scope: Option<ImportScope>,
    pub all_modules: Option<MultiModuleContext>,
}
```

**Update the `Default` implementation:**

**Location:** Find `impl Default for ResolverContext` (around line 30)

**Add the new fields:**

```rust
impl Default for ResolverContext {
    fn default() -> Self {
        Self {
            struct_types: HashMap::new(),
            inherent_methods: HashMap::new(),
            interfaces: HashMap::new(),
            interface_impls: HashMap::new(),
            current_module: None,      // NEW
            import_scope: None,         // NEW
            all_modules: None,          // NEW
        }
    }
}
```

### Step 5.2: Add Import for Types

**File:** `crates/glyph-frontend/src/resolver.rs`

**Location:** At the top of the file (around line 1)

**Add these imports:**

```rust
use crate::module_resolver::{ImportScope, MultiModuleContext};
```

### Step 5.3: Add Symbol Resolution Enum

**File:** `crates/glyph-frontend/src/resolver.rs`

**Location:** After the `MethodInfo` struct (around line 100)

**Add this enum:**

```rust
/// Result of resolving a symbol (possibly from another module)
#[derive(Debug, Clone)]
pub enum ResolvedSymbol {
    Struct(String, String),      // (module_id, struct_name)
    Interface(String, String),   // (module_id, interface_name)
    Function(String, String),    // (module_id, function_name)
}
```

### Step 5.4: Implement `resolve_symbol()` Method

**File:** `crates/glyph-frontend/src/resolver.rs`

**Location:** In the `impl ResolverContext` block, after existing methods (around line 200)

**Add these methods:**

```rust
impl ResolverContext {
    // ... existing methods ...

    /// Resolve a symbol name (handles qualified names and imports)
    pub fn resolve_symbol(&self, name: &str) -> Option<ResolvedSymbol> {
        // Case 1: Qualified name (module::symbol)
        if name.contains("::") {
            return self.resolve_qualified_symbol(name);
        }

        // Case 2: Check direct imports (selective imports)
        if let Some(scope) = &self.import_scope {
            if let Some((source_module, original_name)) = scope.direct_symbols.get(name) {
                return self.resolve_in_module(source_module, original_name);
            }
        }

        // Case 3: Local module symbol
        if let Some(module_id) = &self.current_module {
            if let Some(symbol) = self.resolve_in_module(module_id, name) {
                return Some(symbol);
            }
        }

        None
    }

    fn resolve_qualified_symbol(&self, qualified: &str) -> Option<ResolvedSymbol> {
        let parts: Vec<&str> = qualified.split("::").collect();
        if parts.len() != 2 {
            return None; // Only support module::symbol for now
        }

        let (module_name, symbol_name) = (parts[0], parts[1]);

        // Check if this module is in wildcard imports
        if let Some(scope) = &self.import_scope {
            if scope.wildcard_modules.contains(module_name) {
                return self.resolve_in_module(module_name, symbol_name);
            }
        }

        None
    }

    fn resolve_in_module(&self, module_id: &str, symbol_name: &str) -> Option<ResolvedSymbol> {
        let all_modules = self.all_modules.as_ref()?;
        let module_symbols = all_modules.module_symbols.get(module_id)?;

        if module_symbols.structs.contains(symbol_name) {
            Some(ResolvedSymbol::Struct(module_id.to_string(), symbol_name.to_string()))
        } else if module_symbols.interfaces.contains(symbol_name) {
            Some(ResolvedSymbol::Interface(module_id.to_string(), symbol_name.to_string()))
        } else if module_symbols.functions.contains(symbol_name) {
            Some(ResolvedSymbol::Function(module_id.to_string(), symbol_name.to_string()))
        } else {
            None
        }
    }
}
```

### Step 5.5: Verify Phase 5 Compiles

**Run check:**
```bash
cargo check -p glyph-frontend
```

**Expected:** No errors (warnings about unused code are OK)

---

## Phase 6: MIR Lowering Updates

### Goal
Update the MIR lowering to use the new symbol resolution for cross-module references.

### Step 6.1: Update Struct Type Resolution

**File:** `crates/glyph-frontend/src/mir_lower.rs`

**Location:** Find the `resolve_type_name` function (search for `fn resolve_type_name`)

**This function currently looks like:**
```rust
fn resolve_type_name(name: &str, resolver: &ResolverContext) -> Option<Type> {
    // ... existing logic for primitives ...

    if resolver.struct_types.contains_key(name) {
        return Some(Type::Named(name.to_string()));
    }

    None
}
```

**Replace with:**
```rust
fn resolve_type_name(name: &str, resolver: &ResolverContext) -> Option<Type> {
    // Primitive types
    match name {
        "i32" | "i64" | "f32" | "f64" | "bool" | "str" => {
            return Some(Type::Named(name.to_string()));
        }
        _ => {}
    }

    // Try to resolve through imports
    if let Some(resolved) = resolver.resolve_symbol(name) {
        match resolved {
            crate::resolver::ResolvedSymbol::Struct(module_id, struct_name) => {
                // Use fully qualified name if from another module
                if resolver.current_module.as_ref() == Some(&module_id) {
                    return Some(Type::Named(struct_name));
                } else {
                    return Some(Type::Named(format!("{}::{}", module_id, struct_name)));
                }
            }
            _ => {}
        }
    }

    // Fallback to local lookup
    if resolver.struct_types.contains_key(name) {
        return Some(Type::Named(name.to_string()));
    }

    None
}
```

### Step 6.2: Update Function Call Resolution

**File:** `crates/glyph-frontend/src/mir_lower.rs`

**Location:** Find the `lower_call` function (search for `fn lower_call`)

**Find this section (around line 780):**
```rust
let Expr::Ident(name, _) = callee else { return None; };

let Some(sig) = ctx.fn_sigs.get(&name.0) else {
    ctx.error(format!("unknown function '{}'", name.0));
    return None;
};
```

**Replace with:**
```rust
let Expr::Ident(name, _) = callee else { return None; };

// Try to resolve function name (may be qualified or imported)
let resolved_name = if let Some(resolved) = ctx.resolver.resolve_symbol(&name.0) {
    match resolved {
        crate::resolver::ResolvedSymbol::Function(module_id, func_name) => {
            // Use fully qualified name for cross-module calls
            if ctx.resolver.current_module.as_ref() == Some(&module_id) {
                func_name
            } else {
                format!("{}::{}", module_id, func_name)
            }
        }
        _ => name.0.clone(),
    }
} else {
    name.0.clone()
};

let Some(sig) = ctx.fn_sigs.get(&resolved_name) else {
    ctx.error(format!("unknown function '{}'", name.0));
    return None;
};
```

**IMPORTANT:** After the function lookup, when creating the `Rvalue::Call`, update to use `resolved_name`:

**Find:**
```rust
Some(Rvalue::Call {
    func: name.0.clone(),
    args: arg_vals,
})
```

**Replace with:**
```rust
Some(Rvalue::Call {
    func: resolved_name,
    args: arg_vals,
})
```

### Step 6.3: Verify Phase 6 Compiles

**Run check:**
```bash
cargo check -p glyph-frontend
```

**Expected:** Compiles successfully

---

## Phase 7: End-to-End Integration

### Goal
Wire everything together in the CLI to enable actual multi-file compilation.

### Step 7.1: Update `check()` Function

**File:** `crates/glyph-cli/src/main.rs`

**Location:** Find the `check()` function (around line 108)

**Replace the entire function with:**

```rust
fn check(path: &PathBuf) -> Result<()> {
    // Determine project root (parent of the input file)
    let project_root = path.parent().unwrap_or(Path::new("."));

    // Discover and parse all .glyph files in the project
    let modules = discover_and_parse_modules(project_root)?;

    if modules.is_empty() {
        return Err(anyhow!("no .glyph files found in project"));
    }

    // Multi-module resolution
    let (multi_ctx, compile_order) = glyph_frontend::resolve_multi_module(modules, project_root)
        .map_err(|diags| {
            for diag in &diags {
                eprintln!("{:?}", diag);
            }
            anyhow!("multi-module resolution failed")
        })?;

    // Type-check each module in dependency order
    let mut all_diagnostics = Vec::new();

    for module_id in &compile_order {
        let module = &multi_ctx.modules[module_id];
        let import_scope = &multi_ctx.import_scopes[module_id];

        // Create resolver context with module info
        let mut resolver_ctx = glyph_frontend::ResolverContext::default();
        resolver_ctx.current_module = Some(module_id.clone());
        resolver_ctx.import_scope = Some(import_scope.clone());
        resolver_ctx.all_modules = Some(multi_ctx.clone());

        // Resolve types for this module
        let (_, diags) = glyph_frontend::resolve_types(module);
        all_diagnostics.extend(diags);
    }

    // Report results
    if all_diagnostics.is_empty() {
        println!("check ok: {} module(s), 0 diagnostics", compile_order.len());
        Ok(())
    } else {
        for diag in &all_diagnostics {
            eprintln!("{:?}", diag);
        }
        Err(anyhow!("check failed with {} diagnostic(s)", all_diagnostics.len()))
    }
}
```

### Step 7.2: Add Required Imports

**File:** `crates/glyph-cli/src/main.rs`

**Location:** At the top of the file (check if already present, add if missing)

**Ensure these are present:**
```rust
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Result, anyhow};
use clap::{Parser, Subcommand, ValueEnum};
use glyph_backend::{Backend, CodegenOptions, EmitKind};
use glyph_core::ast::Module;
use glyph_frontend::{FrontendOptions, compile_source};
use walkdir::WalkDir;
```

### Step 7.3: Test Multi-File Compilation

**Create test project:**
```bash
mkdir -p /tmp/multifile_test
cat > /tmp/multifile_test/utils.glyph << 'EOF'
struct Helper { value: i32 }

fn get_value(h: Helper) -> i32 {
  ret h.value
}
EOF

cat > /tmp/multifile_test/main.glyph << 'EOF'
import Helper, get_value from utils

fn main() -> i32 {
  let h = Helper { value: 42 }
  ret get_value(h)
}
EOF
```

**Run check:**
```bash
cargo run -p glyph-cli check /tmp/multifile_test/main.glyph
```

**Expected output:**
```
check ok: 2 module(s), 0 diagnostics
```

### Step 7.4: Test Circular Import Detection

**Create circular import test:**
```bash
mkdir -p /tmp/circular_test
cat > /tmp/circular_test/a.glyph << 'EOF'
import b

fn foo() -> i32 { ret 0 }
EOF

cat > /tmp/circular_test/b.glyph << 'EOF'
import a

fn bar() -> i32 { ret 0 }
EOF
```

**Run check:**
```bash
cargo run -p glyph-cli check /tmp/circular_test/a.glyph
```

**Expected output:**
```
Diagnostic { message: "circular import detected: a → b → a", ... }
Error: multi-module resolution failed
```

### Step 7.5: Test Name Collision Detection

**Create collision test:**
```bash
mkdir -p /tmp/collision_test
cat > /tmp/collision_test/geo.glyph << 'EOF'
struct Point { x: i32, y: i32 }
EOF

cat > /tmp/collision_test/graphics.glyph << 'EOF'
struct Point { r: f32, g: f32, b: f32 }
EOF

cat > /tmp/collision_test/main.glyph << 'EOF'
import Point from geo
import Point from graphics

fn main() -> i32 { ret 0 }
EOF
```

**Run check:**
```bash
cargo run -p glyph-cli check /tmp/collision_test/main.glyph
```

**Expected output:**
```
Diagnostic { message: "symbol 'Point' conflicts: imported from both 'geo' and 'graphics'...", ... }
Error: multi-module resolution failed
```

### Step 7.6: Test Wildcard Import with Qualified Access

**Create wildcard test:**
```bash
mkdir -p /tmp/wildcard_test
cat > /tmp/wildcard_test/math.glyph << 'EOF'
struct Vector { x: i32, y: i32 }

fn add(a: Vector, b: Vector) -> Vector {
  ret Vector { x: a.x, y: b.y }
}
EOF

cat > /tmp/wildcard_test/main.glyph << 'EOF'
import math

fn main() -> i32 {
  let v = math::Vector { x: 1, y: 2 }
  ret v.x
}
EOF
```

**Run check:**
```bash
cargo run -p glyph-cli check /tmp/wildcard_test/main.glyph
```

**Expected output:**
```
check ok: 2 module(s), 0 diagnostics
```

### Step 7.7: Test Aliased Imports

**Create alias test:**
```bash
mkdir -p /tmp/alias_test
cat > /tmp/alias_test/geo.glyph << 'EOF'
struct Point { x: i32, y: i32 }
EOF

cat > /tmp/alias_test/graphics.glyph << 'EOF'
struct Point { r: f32, g: f32, b: f32 }
EOF

cat > /tmp/alias_test/main.glyph << 'EOF'
import Point from geo
import Point as GPoint from graphics

fn main() -> i32 { ret 0 }
EOF
```

**Run check:**
```bash
cargo run -p glyph-cli check /tmp/alias_test/main.glyph
```

**Expected output:**
```
check ok: 3 module(s), 0 diagnostics
```

---

## Verification Checklist

After completing all phases, verify:

- [x] **Phase 4 Tests:** `cargo test -p glyph-frontend module_resolver` → 11+ tests pass
- [x] **Compilation:** `cargo check` → No errors
- [x] **Multi-file check:** Basic imports work
- [x] **Circular imports:** Detected and reported
- [x] **Name collisions:** Detected with helpful error message
- [x] **Wildcard imports:** Qualified access works (`module::Symbol`)
- [x] **Aliased imports:** Resolve name conflicts
- [x] **Subdirectories:** `import math/linear` works

---

## Common Issues and Solutions

### Issue: "unknown function" errors in MIR lowering

**Cause:** Function signatures not being collected with qualified names.

**Solution:** In `mir_lower.rs`, update `collect_function_signatures()` to use qualified names for imported functions.

### Issue: "struct type not found" errors

**Cause:** Type resolution not checking imports.

**Solution:** Ensure `resolve_type_name()` calls `resolver.resolve_symbol()` before local lookup.

### Issue: Compilation order seems wrong

**Cause:** Topological sort might be reversed.

**Solution:** Check `DependencyGraph::topological_sort()` - result should be reversed at the end.

### Issue: Tests failing with "missing field `imports`"

**Cause:** Old test code creating `Module` without the `imports` field.

**Solution:** Add `imports: vec![],` to all `Module { ... }` initializers in tests.

---

## Notes for Implementation

1. **Work incrementally:** Complete each phase fully before moving to the next
2. **Test frequently:** Run `cargo test` and `cargo check` after each step
3. **Read error messages carefully:** Rust's compiler errors are very helpful
4. **Use the plan file:** Reference `/Users/svenkratz/.claude/plans/harmonic-sparking-panda.md` for design details
5. **Add debug output:** Use `eprintln!("DEBUG: {:?}", variable)` liberally while debugging

---

## Success Criteria

The import system is complete when:

1. ✅ All tests in `module_resolver` pass
2. ✅ Multi-file projects compile without errors
3. ✅ Circular imports are detected and reported
4. ✅ Name collisions are detected with helpful errors
5. ✅ Wildcard imports enable qualified access
6. ✅ Selective imports bring symbols into scope
7. ✅ Aliasing resolves name conflicts
8. ✅ Subdirectory imports work (`import subdir/module`)

Good luck! 🚀

use std::collections::{HashMap, HashSet};

use crate::method_symbols::{inherent_method_symbol, interface_method_symbol};
use crate::module_resolver::{ImportScope, MultiModuleContext};
use glyph_core::{
    ast::{Ident, Item, Module, StructDef, TypeExpr},
    diag::Diagnostic,
    types::{EnumType, StructType, Type},
};

/// Context containing resolved type information
#[derive(Debug, Clone)]
pub struct ResolverContext {
    pub struct_types: HashMap<String, StructType>,
    pub enum_types: HashMap<String, EnumType>,
    pub inherent_methods: HashMap<String, HashMap<String, MethodInfo>>,
    pub interfaces: HashMap<String, InterfaceType>,
    pub interface_impls: HashMap<String, HashMap<String, HashMap<String, MethodInfo>>>,
    // struct_name -> interface_name -> method_name -> MethodInfo
    pub extern_functions: HashMap<String, (Vec<Type>, Option<Type>)>,
    pub current_module: Option<String>,
    pub import_scope: Option<ImportScope>,
    pub all_modules: Option<MultiModuleContext>,
}

impl Default for ResolverContext {
    fn default() -> Self {
        Self {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            inherent_methods: HashMap::new(),
            interfaces: HashMap::new(),
            interface_impls: HashMap::new(),
            extern_functions: HashMap::new(),
            current_module: None,
            import_scope: None,
            all_modules: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfKind {
    ByValue, // self: Point
    Ref,     // self: &Point
    MutRef,  // self: &mut Point
}

fn type_expr_to_string(ty: &TypeExpr) -> String {
    match ty {
        TypeExpr::Path { segments, .. } => segments.join("::"),
        TypeExpr::App { base, args, .. } => {
            let mut s = type_expr_to_string(base);
            let rendered_args: Vec<String> = args.iter().map(type_expr_to_string).collect();
            s.push('<');
            s.push_str(&rendered_args.join(", "));
            s.push('>');
            s
        }
        TypeExpr::Ref {
            mutability, inner, ..
        } => {
            let mut s = String::from("&");
            if matches!(mutability, glyph_core::types::Mutability::Mutable) {
                s.push_str("mut ");
            }
            s.push_str(&type_expr_to_string(inner));
            s
        }
        TypeExpr::Array { elem, size, .. } => format!("[{}; {}]", type_expr_to_string(elem), size),
        TypeExpr::Tuple { elements, .. } => {
            let elem_strs: Vec<String> = elements.iter().map(type_expr_to_string).collect();
            format!("({})", elem_strs.join(", "))
        }
    }
}

pub fn resolve_type_expr_to_type(ty: &TypeExpr, ctx: &ResolverContext) -> Option<Type> {
    match ty {
        TypeExpr::Path { segments, .. } => {
            let ty_str = segments.join("::");
            if let Some(builtin) = Type::from_name(&ty_str) {
                return Some(builtin);
            }
            if ctx.enum_types.contains_key(&ty_str) {
                return Some(Type::Enum(ty_str));
            }
            if ctx.struct_types.contains_key(&ty_str) {
                return Some(Type::Named(ty_str));
            }
            Some(Type::Named(ty_str))
        }
        TypeExpr::App { base, args, .. } => {
            let base_ty = resolve_type_expr_to_type(base, ctx)?;
            let mut rendered_args: Vec<Type> = Vec::new();
            for a in args {
                if let Some(resolved) = resolve_type_expr_to_type(a, ctx) {
                    rendered_args.push(resolved);
                } else {
                    // Unknown arg type; bail to Named
                    return Some(Type::Named(type_expr_to_string(ty)));
                }
            }

            if let Type::Named(name) = &base_ty {
                if name == "RawPtr" && rendered_args.len() == 1 {
                    return Some(Type::RawPtr(Box::new(rendered_args[0].clone())));
                }
                if name == "Own" && rendered_args.len() == 1 {
                    return Some(Type::Own(Box::new(rendered_args[0].clone())));
                }
                if name == "Shared" && rendered_args.len() == 1 {
                    return Some(Type::Shared(Box::new(rendered_args[0].clone())));
                }
            }

            match base_ty {
                Type::Named(name) | Type::Enum(name) => Some(Type::App {
                    base: name,
                    args: rendered_args,
                }),
                _ => Some(Type::Named(type_expr_to_string(ty))),
            }
        }
        TypeExpr::Ref {
            mutability, inner, ..
        } => resolve_type_expr_to_type(inner, ctx)
            .map(|inner_ty| Type::Ref(Box::new(inner_ty), *mutability)),
        TypeExpr::Array { elem, size, .. } => resolve_type_expr_to_type(elem, ctx)
            .map(|elem_ty| Type::Array(Box::new(elem_ty), *size)),
        TypeExpr::Tuple { elements, .. } => {
            let mut elem_types = Vec::new();
            for elem in elements {
                if let Some(resolved) = resolve_type_expr_to_type(elem, ctx) {
                    elem_types.push(resolved);
                } else {
                    return None;
                }
            }
            Some(Type::Tuple(elem_types))
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub function_name: String,
    pub self_kind: SelfKind,
}

/// Result of resolving a symbol (possibly from another module)
#[derive(Debug, Clone)]
pub enum ResolvedSymbol {
    Struct(String, String),    // (module_id, struct_name)
    Interface(String, String), // (module_id, interface_name)
    Function(String, String),  // (module_id, function_name)
    Enum(String, String),      // (module_id, enum_name)
}

#[derive(Debug, Clone)]
pub struct InterfaceType {
    pub name: String,
    pub methods: Vec<InterfaceMethodSig>,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodSig {
    pub name: String,
    pub params: Vec<Option<Type>>,
    pub raw_param_types: Vec<Option<String>>,
    pub ret_type: Option<Type>,
    pub raw_ret_type: Option<String>,
}

/// Resolves types in a module, building a registry of struct types
/// Returns the resolver context and any diagnostics encountered
pub fn resolve_types(module: &Module) -> (ResolverContext, Vec<Diagnostic>) {
    let mut ctx = ResolverContext {
        struct_types: HashMap::new(),
        enum_types: HashMap::new(),
        inherent_methods: HashMap::new(),
        interfaces: HashMap::new(),
        interface_impls: HashMap::new(),
        extern_functions: HashMap::new(),
        current_module: None,
        import_scope: None,
        all_modules: None,
    };
    let mut diagnostics = Vec::new();

    // Collect enum definitions
    for item in &module.items {
        if let Item::Enum(e) = item {
            let enum_name = e.name.0.clone();

            if ctx.enum_types.contains_key(&enum_name) || ctx.struct_types.contains_key(&enum_name)
            {
                diagnostics.push(Diagnostic::error(
                    format!("type '{}' is defined multiple times", enum_name),
                    Some(e.span),
                ));
                continue;
            }

            let mut seen_variants = std::collections::HashSet::new();
            let mut variants = Vec::new();
            for variant in &e.variants {
                if !seen_variants.insert(variant.name.0.clone()) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "variant '{}' is defined multiple times in enum '{}'",
                            variant.name.0, enum_name
                        ),
                        Some(variant.span),
                    ));
                    continue;
                }

                let payload_ty = match &variant.payload {
                    Some(payload_expr) => resolve_type_expr_to_type(payload_expr, &ctx),
                    None => None,
                };

                variants.push(glyph_core::types::EnumVariant {
                    name: variant.name.0.clone(),
                    payload: payload_ty,
                });
            }

            ctx.enum_types.insert(
                enum_name.clone(),
                EnumType {
                    name: enum_name,
                    variants,
                },
            );
        }
    }

    // First pass: collect all interface definitions
    for item in &module.items {
        if let Item::Interface(iface) = item {
            let iface_name = &iface.name.0;

            // Check for duplicate interface definitions
            if ctx.interfaces.contains_key(iface_name) {
                diagnostics.push(Diagnostic::error(
                    format!("interface '{}' is defined multiple times", iface_name),
                    Some(iface.span),
                ));
                continue;
            }

            let mut methods = Vec::new();
            for method in &iface.methods {
                // Resolve parameter types
                let mut param_types = Vec::new();
                let mut raw_param_types = Vec::new();
                for param in &method.params {
                    let param_ty = match &param.ty {
                        Some(ty_expr) => {
                            let rendered = type_expr_to_string(ty_expr);
                            raw_param_types.push(Some(rendered.clone()));
                            resolve_type_expr_to_type(ty_expr, &ctx)
                        }
                        None => {
                            raw_param_types.push(None);
                            None
                        }
                    };
                    param_types.push(param_ty);
                }

                // Resolve return type
                let (ret_type, raw_ret_type) = match &method.ret_type {
                    Some(ty) => {
                        let rendered = type_expr_to_string(ty);
                        (resolve_type_expr_to_type(ty, &ctx), Some(rendered))
                    }
                    None => (None, None),
                };

                methods.push(InterfaceMethodSig {
                    name: method.name.0.clone(),
                    params: param_types,
                    raw_param_types,
                    ret_type,
                    raw_ret_type,
                });
            }

            ctx.interfaces.insert(
                iface_name.clone(),
                InterfaceType {
                    name: iface_name.clone(),
                    methods,
                },
            );
        }
    }

    // Second pass: collect all struct definitions
    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = s.name.0.clone();

            // Check for duplicate struct definitions
            if ctx.struct_types.contains_key(&struct_name)
                || ctx.enum_types.contains_key(&struct_name)
            {
                diagnostics.push(Diagnostic::error(
                    format!("struct '{}' is defined multiple times", struct_name),
                    Some(s.span),
                ));
                continue;
            }

            let mut fields = Vec::new();
            let mut field_names = std::collections::HashSet::new();

            for field in &s.fields {
                let field_name = field.name.0.clone();
                let rendered = type_expr_to_string(&field.ty);

                // Check for duplicate field names
                if !field_names.insert(field_name.clone()) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "field '{}' is defined multiple times in struct '{}'",
                            field_name, struct_name
                        ),
                        Some(field.span),
                    ));
                    continue;
                }

                // Resolve field type
                let field_type = resolve_type_expr_to_type(&field.ty, &ctx)
                    .unwrap_or(Type::Named(rendered.clone()));

                fields.push((field_name, field_type));
            }

            ctx.struct_types.insert(
                struct_name.clone(),
                StructType {
                    name: struct_name,
                    fields,
                },
            );
        }
    }

    // Third pass: collect extern function signatures (FFI subset)
    for item in &module.items {
        if let Item::ExternFunction(f) = item {
            let is_sys_argv = f.name.0 == "argv" && f.params.is_empty() && f.abi.is_none();

            let mut params = Vec::new();
            let mut has_error = false;
            if !is_sys_argv {
                for param in &f.params {
                    let Some(ty_ident) = param.ty.as_ref() else {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "extern function '{}' parameter '{}' must have a type",
                                f.name.0, param.name.0
                            ),
                            Some(param.span),
                        ));
                        has_error = true;
                        continue;
                    };

                    let ty_rendered = type_expr_to_string(ty_ident);
                    if let Some(resolved) = resolve_ffi_type(&ty_rendered) {
                        params.push(resolved);
                    } else {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "unsupported FFI parameter type '{}' in extern function '{}'",
                                ty_rendered, f.name.0
                            ),
                            Some(param.span),
                        ));
                        has_error = true;
                    }
                }
            }

            let ret_type = match &f.ret_type {
                Some(ty) => {
                    if is_sys_argv {
                        resolve_type_expr_to_type(ty, &ctx)
                    } else {
                        let ty_rendered = type_expr_to_string(ty);
                        if let Some(resolved) = resolve_ffi_type(&ty_rendered) {
                            Some(resolved)
                        } else {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "unsupported FFI return type '{}' in extern function '{}'",
                                    ty_rendered, f.name.0
                                ),
                                Some(f.span),
                            ));
                            has_error = true;
                            None
                        }
                    }
                }
                None => None,
            };

            if is_sys_argv && ret_type.is_none() {
                diagnostics.push(Diagnostic::error(
                    "argv() must return Vec<String>".to_string(),
                    Some(f.span),
                ));
                has_error = true;
            }

            if !has_error {
                if ctx.extern_functions.contains_key(&f.name.0)
                    || ctx.struct_types.contains_key(&f.name.0)
                {
                    diagnostics.push(Diagnostic::error(
                        format!("function '{}' is defined multiple times", f.name.0),
                        Some(f.span),
                    ));
                } else {
                    ctx.extern_functions
                        .insert(f.name.0.clone(), (params, ret_type));
                }
            }
        }
    }

    // Fourth pass: collect inherent methods from structs
    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = s.name.0.clone();

            // Collect inherent methods
            for method in &s.methods {
                // Generate mangled name using existing infrastructure
                let mangled_name = inherent_method_symbol(&struct_name, &method.name.0);

                // Detect self kind from first parameter
                let self_kind = if let Some(first_param) = method.params.first() {
                    detect_self_kind(&first_param.ty)
                } else {
                    // No parameters - should have been caught by parser, but handle it
                    SelfKind::ByValue
                };

                let method_info = MethodInfo {
                    function_name: mangled_name,
                    self_kind,
                };

                ctx.inherent_methods
                    .entry(struct_name.clone())
                    .or_insert_with(HashMap::new)
                    .insert(method.name.0.clone(), method_info);
            }
        }
    }

    // Local struct name set for interface validation.
    let struct_names: std::collections::HashSet<_> = ctx.struct_types.keys().cloned().collect();

    validate_interface_signatures(&ctx, &struct_names, &mut diagnostics);
    collect_interface_impls(module, &mut ctx, &mut diagnostics);

    (ctx, diagnostics)
}

/// Validate that all Named types referenced in struct fields / enum payloads
/// exist in the resolver context.
///
/// Note: this must run *after* `populate_imported_types()` for multi-module
/// compilation, otherwise references to imported types will be flagged as
/// undefined.
pub fn validate_named_types(
    module: &Module,
    ctx: &ResolverContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let struct_names: std::collections::HashSet<_> = ctx.struct_types.keys().cloned().collect();
    let enum_names: std::collections::HashSet<_> = ctx.enum_types.keys().cloned().collect();

    for item in &module.items {
        if let Item::Struct(s) = item {
            let struct_name = &s.name.0;
            let generic_params: std::collections::HashSet<String> =
                s.generic_params.iter().map(|p| p.0.clone()).collect();

            if let Some(struct_type) = ctx.struct_types.get(struct_name) {
                for (field_name, field_type) in &struct_type.fields {
                    if let Type::Named(type_name) = field_type {
                        let is_generic = generic_params.contains(type_name);
                        let is_known =
                            struct_names.contains(type_name) || enum_names.contains(type_name);
                        if !is_generic && !is_known {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "undefined type '{}' used in field '{}' of struct '{}'",
                                    type_name, field_name, struct_name
                                ),
                                Some(s.span),
                            ));
                        }
                    }
                }
            }
        }
    }

    for item in &module.items {
        if let Item::Enum(e) = item {
            if let Some(enum_type) = ctx.enum_types.get(&e.name.0) {
                let generic_params: std::collections::HashSet<String> =
                    e.generic_params.iter().map(|p| p.0.clone()).collect();
                for variant in &enum_type.variants {
                    if let Some(Type::Named(type_name)) = &variant.payload {
                        let is_generic = generic_params.contains(type_name);
                        let is_known =
                            struct_names.contains(type_name) || enum_names.contains(type_name);
                        if !is_generic && !is_known {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "undefined type '{}' used in variant '{}' of enum '{}'",
                                    type_name, variant.name, e.name.0
                                ),
                                Some(e.span),
                            ));
                        }
                    }
                }
            }
        }
    }
}

const HASH_INTERFACE: &str = "Hash";

pub fn validate_map_usage(
    module: &Module,
    ctx: &ResolverContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for item in &module.items {
        match item {
            Item::Struct(def) => {
                let generics: HashSet<String> =
                    def.generic_params.iter().map(|p| p.0.clone()).collect();
                for field in &def.fields {
                    validate_map_type_expr(&field.ty, ctx, &generics, module, diagnostics);
                }
                for method in &def.methods {
                    validate_map_function(method, ctx, &generics, module, diagnostics);
                }
                for inline in &def.inline_impls {
                    for method in &inline.methods {
                        validate_map_function(method, ctx, &generics, module, diagnostics);
                    }
                }
            }
            Item::Enum(def) => {
                let generics: HashSet<String> =
                    def.generic_params.iter().map(|p| p.0.clone()).collect();
                for variant in &def.variants {
                    if let Some(payload) = &variant.payload {
                        validate_map_type_expr(payload, ctx, &generics, module, diagnostics);
                    }
                }
            }
            Item::Interface(def) => {
                let generics = HashSet::new();
                for method in &def.methods {
                    for param in &method.params {
                        if let Some(ty) = &param.ty {
                            validate_map_type_expr(ty, ctx, &generics, module, diagnostics);
                        }
                    }
                    if let Some(ret) = &method.ret_type {
                        validate_map_type_expr(ret, ctx, &generics, module, diagnostics);
                    }
                }
            }
            Item::Impl(block) => {
                let generics = struct_generic_params(block.target.0.as_str(), module, ctx);
                for method in &block.methods {
                    validate_map_function(method, ctx, &generics, module, diagnostics);
                }
            }
            Item::Function(func) => {
                let generics = HashSet::new();
                validate_map_function(func, ctx, &generics, module, diagnostics);
            }
            Item::ExternFunction(func) => {
                let generics = HashSet::new();
                for param in &func.params {
                    if let Some(ty) = &param.ty {
                        validate_map_type_expr(ty, ctx, &generics, module, diagnostics);
                    }
                }
                if let Some(ret) = &func.ret_type {
                    validate_map_type_expr(ret, ctx, &generics, module, diagnostics);
                }
            }
        }
    }
}

fn validate_map_function(
    func: &glyph_core::ast::Function,
    ctx: &ResolverContext,
    generics: &HashSet<String>,
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for param in &func.params {
        if let Some(ty) = &param.ty {
            validate_map_type_expr(ty, ctx, generics, module, diagnostics);
        }
    }
    if let Some(ret) = &func.ret_type {
        validate_map_type_expr(ret, ctx, generics, module, diagnostics);
    }
    validate_map_block(&func.body, ctx, generics, module, diagnostics);
}

fn validate_map_block(
    block: &glyph_core::ast::Block,
    ctx: &ResolverContext,
    generics: &HashSet<String>,
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in &block.stmts {
        if let glyph_core::ast::Stmt::Let { ty: Some(ty), .. } = stmt {
            validate_map_type_expr(ty, ctx, generics, module, diagnostics);
        }
    }
}

fn validate_map_type_expr(
    ty: &TypeExpr,
    ctx: &ResolverContext,
    generics: &HashSet<String>,
    module: &Module,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match ty {
        TypeExpr::App { base, args, span } => {
            if let TypeExpr::Path { segments, .. } = base.as_ref() {
                let base_name = segments.join("::");
                if base_name == "Map" {
                    if args.len() != 2 {
                        diagnostics.push(Diagnostic::error(
                            format!("Map expects 2 type arguments but got {}", args.len()),
                            Some(*span),
                        ));
                    } else if !map_key_is_hashable(&args[0], ctx, generics, module) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "Map key type '{}' must implement Hash",
                                type_expr_to_string(&args[0])
                            ),
                            Some(*span),
                        ));
                    }
                }
            }
            for arg in args {
                validate_map_type_expr(arg, ctx, generics, module, diagnostics);
            }
        }
        TypeExpr::Ref { inner, .. } => {
            validate_map_type_expr(inner, ctx, generics, module, diagnostics);
        }
        TypeExpr::Array { elem, .. } => {
            validate_map_type_expr(elem, ctx, generics, module, diagnostics);
        }
        TypeExpr::Tuple { elements, .. } => {
            for elem in elements {
                validate_map_type_expr(elem, ctx, generics, module, diagnostics);
            }
        }
        TypeExpr::Path { .. } => {}
    }
}

fn map_key_is_hashable(
    key: &TypeExpr,
    ctx: &ResolverContext,
    generics: &HashSet<String>,
    module: &Module,
) -> bool {
    if let TypeExpr::Path { segments, .. } = key {
        if segments.len() == 1 && generics.contains(&segments[0]) {
            return true;
        }
    }

    let key_type = resolve_type_expr_to_type(key, ctx)
        .unwrap_or_else(|| Type::Named(type_expr_to_string(key)));
    if is_hashable_scalar(&key_type) {
        return true;
    }

    match key_type {
        Type::Named(name) => struct_supports_hash(name.as_str(), module, ctx),
        Type::Param(_) => true,
        _ => false,
    }
}

fn is_hashable_scalar(ty: &Type) -> bool {
    matches!(
        ty,
        Type::I8
            | Type::I32
            | Type::I64
            | Type::U8
            | Type::U32
            | Type::U64
            | Type::Usize
            | Type::Bool
            | Type::Char
            | Type::RawPtr(_)
            | Type::Ref(_, _)
            | Type::String
            | Type::Str
    )
}

fn struct_supports_hash(struct_name: &str, module: &Module, ctx: &ResolverContext) -> bool {
    if let Some(impls) = ctx.interface_impls.get(struct_name) {
        if impls.contains_key(HASH_INTERFACE) {
            return true;
        }
    }

    if module_struct_has_hash(module, struct_name) {
        return true;
    }

    if let Some(all_modules) = ctx.all_modules.as_ref() {
        if let Some(ResolvedSymbol::Struct(module_id, resolved_name)) =
            ctx.resolve_symbol(struct_name)
        {
            if let Some(target) = all_modules.modules.get(&module_id) {
                return module_struct_has_hash(target, &resolved_name);
            }
        }
    }

    false
}

fn module_struct_has_hash(module: &Module, struct_name: &str) -> bool {
    for item in &module.items {
        match item {
            Item::Struct(def) if def.name.0 == struct_name => {
                if def.interfaces.iter().any(|iface| iface.0 == HASH_INTERFACE) {
                    return true;
                }
                if def
                    .inline_impls
                    .iter()
                    .any(|inline| inline.interface.0 == HASH_INTERFACE)
                {
                    return true;
                }
            }
            Item::Impl(block)
                if block.target.0 == struct_name && block.interface.0 == HASH_INTERFACE =>
            {
                return true;
            }
            _ => {}
        }
    }
    false
}

fn struct_generic_params(
    struct_name: &str,
    module: &Module,
    ctx: &ResolverContext,
) -> HashSet<String> {
    if let Some(def) = find_struct_def(module, struct_name) {
        return def.generic_params.iter().map(|p| p.0.clone()).collect();
    }

    if let Some(all_modules) = ctx.all_modules.as_ref() {
        if let Some(ResolvedSymbol::Struct(module_id, resolved_name)) =
            ctx.resolve_symbol(struct_name)
        {
            if let Some(target) = all_modules.modules.get(&module_id) {
                if let Some(def) = find_struct_def(target, resolved_name.as_str()) {
                    return def.generic_params.iter().map(|p| p.0.clone()).collect();
                }
            }
        }
    }

    HashSet::new()
}

fn find_struct_def<'a>(module: &'a Module, struct_name: &str) -> Option<&'a StructDef> {
    module.items.iter().find_map(|item| match item {
        Item::Struct(def) if def.name.0 == struct_name => Some(def),
        _ => None,
    })
}

impl ResolverContext {
    /// Look up a struct type by name
    pub fn get_struct(&self, name: &str) -> Option<&StructType> {
        self.struct_types.get(name)
    }

    pub fn get_enum(&self, name: &str) -> Option<&EnumType> {
        self.enum_types.get(name)
    }

    /// Get the type and index of a field in a struct
    pub fn get_field(&self, struct_name: &str, field_name: &str) -> Option<(Type, usize)> {
        // Handle tuple structs specially (e.g., "__Tuple2_i32_i32")
        if struct_name.starts_with("__Tuple") {
            // Parse the field index from field_name (should be "0", "1", "2", etc.)
            if let Ok(idx) = field_name.parse::<usize>() {
                // For now, we can't easily extract the exact element type from the struct name
                // during resolution, so we'll just return i32 as a placeholder.
                // The actual type will be properly handled during MIR lowering.
                return Some((Type::I32, idx));
            }
            return None;
        }

        let struct_type = self.get_struct(struct_name)?;

        for (i, (name, ty)) in struct_type.fields.iter().enumerate() {
            if name == field_name {
                return Some((ty.clone(), i));
            }
        }

        None
    }

    /// Look up inherent method by struct type and method name
    /// Returns mangled function name and method info
    pub fn get_inherent_method(&self, struct_name: &str, method_name: &str) -> Option<&MethodInfo> {
        self.inherent_methods.get(struct_name)?.get(method_name)
    }

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
            // Extern function in current module
            if let Some((_, _)) = self.extern_functions.get(name) {
                return Some(ResolvedSymbol::Function(
                    module_id.clone(),
                    name.to_string(),
                ));
            }
        }

        None
    }

    fn resolve_qualified_symbol(&self, qualified: &str) -> Option<ResolvedSymbol> {
        let parts: Vec<&str> = qualified.split("::").collect();
        if parts.len() < 2 {
            return None;
        }

        // Everything except the last segment is the module path
        let module_path = parts[..parts.len() - 1].join("/");
        let symbol_name = parts.last().unwrap().to_string();

        // Allow fully-qualified references to the current module without needing imports.
        if self.current_module.as_deref() == Some(&module_path) {
            if let Some(resolved) = self.resolve_in_module(&module_path, &symbol_name) {
                return Some(resolved);
            }
        }

        // Check if this module (or a prefix) is available via wildcard imports
        if let Some(scope) = &self.import_scope {
            if scope.wildcard_modules.contains(&module_path)
                || scope
                    .wildcard_modules
                    .iter()
                    .any(|m| module_path.starts_with(&format!("{}/", m)))
            {
                if let Some(resolved) = self.resolve_in_module(&module_path, &symbol_name) {
                    return Some(resolved);
                }
            }
        }

        // Fallback: if the module exists in the program graph, allow absolute qualified access.
        if let Some(all) = &self.all_modules {
            if all.module_symbols.contains_key(&module_path) {
                return self.resolve_in_module(&module_path, &symbol_name);
            }
        }

        None
    }

    fn resolve_in_module(&self, module_id: &str, symbol_name: &str) -> Option<ResolvedSymbol> {
        let all_modules = self.all_modules.as_ref()?;
        let module_symbols = all_modules.module_symbols.get(module_id)?;

        if module_symbols.structs.contains(symbol_name) {
            Some(ResolvedSymbol::Struct(
                module_id.to_string(),
                symbol_name.to_string(),
            ))
        } else if module_symbols.enums.contains(symbol_name) {
            Some(ResolvedSymbol::Enum(
                module_id.to_string(),
                symbol_name.to_string(),
            ))
        } else if module_symbols.interfaces.contains(symbol_name) {
            Some(ResolvedSymbol::Interface(
                module_id.to_string(),
                symbol_name.to_string(),
            ))
        } else if module_symbols.functions.contains(symbol_name) {
            Some(ResolvedSymbol::Function(
                module_id.to_string(),
                symbol_name.to_string(),
            ))
        } else {
            // fallback: if current module, check externs map
            if self.current_module.as_deref() == Some(module_id) {
                if self.extern_functions.contains_key(symbol_name) {
                    return Some(ResolvedSymbol::Function(
                        module_id.to_string(),
                        symbol_name.to_string(),
                    ));
                }
            }
            None
        }
    }
}

/// Populate the resolver context with struct/enum types made available through imports.
/// This enables cross-module type usage during lowering without requiring explicit
/// duplication in the source module.
pub fn populate_imported_types(ctx: &mut ResolverContext) {
    let (Some(scope), Some(all_modules)) = (&ctx.import_scope, &ctx.all_modules) else {
        return;
    };

    for (local_name, (source_module, original_name)) in &scope.direct_symbols {
        if ctx.struct_types.contains_key(local_name) || ctx.enum_types.contains_key(local_name) {
            continue;
        }
        if let Some(module) = all_modules.modules.get(source_module) {
            for item in &module.items {
                match item {
                    Item::Struct(s) if s.name.0 == *original_name => {
                        // Pre-register to support self/recursive references during resolution.
                        ctx.struct_types.insert(
                            local_name.clone(),
                            StructType {
                                name: local_name.clone(),
                                fields: Vec::new(),
                            },
                        );
                        let mut st = struct_type_from_def(s, ctx);
                        st.name = local_name.clone();
                        ctx.struct_types.insert(local_name.clone(), st);
                        break;
                    }
                    Item::Enum(e) if e.name.0 == *original_name => {
                        // Pre-register to support self/recursive references during resolution.
                        ctx.enum_types.insert(
                            local_name.clone(),
                            EnumType {
                                name: local_name.clone(),
                                variants: Vec::new(),
                            },
                        );
                        let mut et = enum_type_from_def(e, ctx);
                        et.name = local_name.clone();
                        ctx.enum_types.insert(local_name.clone(), et);
                        break;
                    }
                    _ => {}
                }
            }
        }
    }

    for wildcard in &scope.wildcard_modules {
        for (_module_id, module) in all_modules
            .modules
            .iter()
            .filter(|(id, _)| *id == wildcard || id.starts_with(&format!("{}/", wildcard)))
        {
            for item in &module.items {
                match item {
                    Item::Struct(s) => {
                        if ctx.struct_types.contains_key(&s.name.0) {
                            continue;
                        }
                        // Pre-register to support self/recursive references during resolution.
                        ctx.struct_types.insert(
                            s.name.0.clone(),
                            StructType {
                                name: s.name.0.clone(),
                                fields: Vec::new(),
                            },
                        );
                        let st = struct_type_from_def(s, ctx);
                        ctx.struct_types.insert(st.name.clone(), st);
                    }
                    Item::Enum(e) => {
                        if ctx.enum_types.contains_key(&e.name.0) {
                            continue;
                        }
                        // Pre-register to support self/recursive references during resolution.
                        ctx.enum_types.insert(
                            e.name.0.clone(),
                            EnumType {
                                name: e.name.0.clone(),
                                variants: Vec::new(),
                            },
                        );
                        let et = enum_type_from_def(e, ctx);
                        ctx.enum_types.insert(et.name.clone(), et);
                    }
                    _ => {}
                }
            }
        }
    }
}

fn struct_type_from_def(def: &glyph_core::ast::StructDef, ctx: &ResolverContext) -> StructType {
    let generics: std::collections::HashSet<String> =
        def.generic_params.iter().map(|p| p.0.clone()).collect();
    let mut fields = Vec::new();
    for field in &def.fields {
        let mut field_type = resolve_type_expr_to_type(&field.ty, ctx)
            .unwrap_or_else(|| Type::Named(type_expr_to_string(&field.ty)));
        if let Type::Named(n) = &field_type {
            if generics.contains(n) {
                field_type = Type::Param(n.clone());
            }
        }
        fields.push((field.name.0.clone(), field_type));
    }

    StructType {
        name: def.name.0.clone(),
        fields,
    }
}

fn enum_type_from_def(def: &glyph_core::ast::EnumDef, ctx: &ResolverContext) -> EnumType {
    let generics: std::collections::HashSet<String> =
        def.generic_params.iter().map(|p| p.0.clone()).collect();
    let mut variants = Vec::new();
    for variant in &def.variants {
        let mut payload_ty = match &variant.payload {
            Some(payload_ident) => Some(
                resolve_type_expr_to_type(payload_ident, ctx)
                    .unwrap_or_else(|| Type::Named(type_expr_to_string(payload_ident))),
            ),
            None => None,
        };

        if let Some(Type::Named(n)) = &payload_ty {
            if generics.contains(n) {
                payload_ty = Some(Type::Param(n.clone()));
            }
        }

        variants.push(glyph_core::types::EnumVariant {
            name: variant.name.0.clone(),
            payload: payload_ty,
        });
    }

    EnumType {
        name: def.name.0.clone(),
        variants,
    }
}

fn resolve_ffi_type(name: &str) -> Option<Type> {
    let trimmed = name.trim();

    if trimmed == "&str" {
        return Some(Type::Str);
    }

    if trimmed == "String" {
        return Some(Type::String);
    }

    if trimmed == "String" {
        return Some(Type::String);
    }

    if let Some(builtin) = Type::from_name(trimmed) {
        return Some(builtin);
    }

    if trimmed == "void" {
        return Some(Type::Void);
    }

    // Allow RawPtr<T> for any T (validated recursively)
    if trimmed.starts_with("RawPtr<") && trimmed.ends_with('>') {
        let inner = trimmed.trim_start_matches("RawPtr<").trim_end_matches('>');
        let inner_ty = resolve_ffi_type(inner).unwrap_or(Type::Named(inner.to_string()));
        return Some(Type::RawPtr(Box::new(inner_ty)));
    }

    None
}

/// Detect the self kind from a parameter type
fn detect_self_kind(param_ty: &Option<TypeExpr>) -> SelfKind {
    match param_ty {
        Some(ty_expr) => {
            let ty_str = type_expr_to_string(ty_expr).replace(' ', "");
            if ty_str.starts_with("&mut") {
                SelfKind::MutRef
            } else if ty_str.starts_with('&') {
                SelfKind::Ref
            } else {
                SelfKind::ByValue
            }
        }
        None => SelfKind::ByValue,
    }
}

#[derive(Default)]
struct InterfaceImplState {
    methods: HashMap<String, MethodInfo>,
    has_error: bool,
}

fn normalize_type_name(raw: &str) -> String {
    raw.split_whitespace().collect()
}

fn self_param_matches_struct(param: &glyph_core::ast::Param, struct_name: &str) -> bool {
    match &param.ty {
        Some(ty) => {
            let rendered = type_expr_to_string(ty);
            let norm = normalize_type_name(&rendered);
            norm == struct_name
                || norm == format!("&{}", struct_name)
                || norm == format!("&mut{}", struct_name)
        }
        None => true,
    }
}

fn validate_interface_signatures(
    ctx: &ResolverContext,
    struct_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for iface in ctx.interfaces.values() {
        for method in &iface.methods {
            for (idx, param_ty) in method.params.iter().enumerate() {
                if let Some(Type::Named(name)) = param_ty {
                    if !struct_names.contains(name) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "interface '{}' references unknown type '{}' in parameter {}",
                                iface.name,
                                name,
                                idx + 1
                            ),
                            None,
                        ));
                    }
                }
            }

            if let Some(Type::Named(name)) = &method.ret_type {
                if !struct_names.contains(name) {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "interface '{}' references unknown return type '{}'",
                            iface.name, name
                        ),
                        None,
                    ));
                }
            }
        }
    }
}

fn record_interface_impl(
    struct_name: &Ident,
    interface: &Ident,
    methods: &[glyph_core::ast::Function],
    span: glyph_core::span::Span,
    ctx: &ResolverContext,
    impls: &mut HashMap<String, HashMap<String, InterfaceImplState>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let iface_name = &interface.0;
    let Some(iface_def) = ctx.interfaces.get(iface_name) else {
        diagnostics.push(Diagnostic::error(
            format!("impl refers to unknown interface '{}'", iface_name),
            Some(span),
        ));
        return;
    };

    let struct_entry = impls
        .entry(struct_name.0.clone())
        .or_insert_with(HashMap::new);
    let state = struct_entry
        .entry(iface_name.clone())
        .or_insert_with(InterfaceImplState::default);

    for method in methods {
        if state.methods.contains_key(&method.name.0) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' implemented multiple times for interface '{}' on struct '{}'",
                    method.name.0, iface_name, struct_name.0
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        if method.params.is_empty() || method.params[0].name.0 != "self" {
            diagnostics.push(Diagnostic::error(
                "interface methods must take `self` as the first parameter",
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        if !self_param_matches_struct(&method.params[0], &struct_name.0) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "self parameter type must reference struct '{}' (by value, '&', or '&mut')",
                    struct_name.0
                ),
                Some(method.params[0].span),
            ));
            state.has_error = true;
            continue;
        }

        let Some(iface_sig) = iface_def.methods.iter().find(|m| m.name == method.name.0) else {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' is not declared in interface '{}'",
                    method.name.0, iface_name
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        };

        if iface_sig.params.len() != method.params.len() {
            diagnostics.push(Diagnostic::error(
                format!(
                    "method '{}' has {} parameters but interface '{}' expects {}",
                    method.name.0,
                    method.params.len(),
                    iface_name,
                    iface_sig.params.len()
                ),
                Some(method.span),
            ));
            state.has_error = true;
            continue;
        }

        for (idx, expected_raw) in iface_sig.raw_param_types.iter().enumerate() {
            // Skip self; already validated separately for struct receiver.
            if idx == 0 {
                continue;
            }
            if let Some(expected) = expected_raw {
                let actual = method
                    .params
                    .get(idx)
                    .and_then(|p| p.ty.as_ref())
                    .map(|t| normalize_type_name(&type_expr_to_string(t)));
                if actual.is_none() {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "parameter {} of method '{}' must have type '{}' per interface '{}'",
                            idx + 1,
                            method.name.0,
                            expected,
                            iface_name
                        ),
                        Some(method.params[idx].span),
                    ));
                    state.has_error = true;
                } else if actual.as_deref() != Some(&normalize_type_name(expected)) {
                    let actual_rendered = method.params[idx]
                        .ty
                        .as_ref()
                        .map(|t| type_expr_to_string(t))
                        .unwrap_or_else(|| "<unknown>".to_string());
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "parameter {} of method '{}' has type '{}' but interface '{}' expects '{}'",
                            idx + 1,
                            method.name.0,
                            actual_rendered,
                            iface_name,
                            expected
                        ),
                        Some(method.params[idx].span),
                    ));
                    state.has_error = true;
                }
            }
        }

        if let Some(expected_ret) = iface_sig.raw_ret_type.as_ref() {
            match method.ret_type.as_ref() {
                Some(actual_ret) => {
                    let rendered = type_expr_to_string(actual_ret);
                    if normalize_type_name(&rendered) != normalize_type_name(expected_ret) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "method '{}' returns '{}' but interface '{}' requires '{}'",
                                method.name.0, rendered, iface_name, expected_ret
                            ),
                            Some(method.span),
                        ));
                        state.has_error = true;
                    }
                }
                None => {
                    diagnostics.push(Diagnostic::error(
                        format!(
                            "method '{}' must return '{}' to satisfy interface '{}'",
                            method.name.0, expected_ret, iface_name
                        ),
                        Some(method.span),
                    ));
                    state.has_error = true;
                }
            }
        }

        let self_kind = detect_self_kind(&method.params[0].ty);
        let mangled_name = interface_method_symbol(&struct_name.0, iface_name, &method.name.0);

        state.methods.insert(
            method.name.0.clone(),
            MethodInfo {
                function_name: mangled_name,
                self_kind,
            },
        );
    }
}

fn collect_interface_impls(
    module: &Module,
    ctx: &mut ResolverContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut impls: HashMap<String, HashMap<String, InterfaceImplState>> = HashMap::new();

    for item in &module.items {
        match item {
            Item::Struct(s) => {
                for iface in &s.interfaces {
                    if !ctx.interfaces.contains_key(&iface.0) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "unknown interface '{}' declared on struct '{}'",
                                iface.0, s.name.0
                            ),
                            Some(s.span),
                        ));
                        continue;
                    }

                    impls
                        .entry(s.name.0.clone())
                        .or_insert_with(HashMap::new)
                        .entry(iface.0.clone())
                        .or_insert_with(InterfaceImplState::default);
                }

                for inline in &s.inline_impls {
                    record_interface_impl(
                        &s.name,
                        &inline.interface,
                        &inline.methods,
                        inline.span,
                        ctx,
                        &mut impls,
                        diagnostics,
                    );
                }
            }
            Item::Impl(block) => {
                if !ctx.struct_types.contains_key(&block.target.0) {
                    diagnostics.push(Diagnostic::error(
                        format!("impl target '{}' is not a known struct", block.target.0),
                        Some(block.span),
                    ));
                    continue;
                }

                record_interface_impl(
                    &block.target,
                    &block.interface,
                    &block.methods,
                    block.span,
                    ctx,
                    &mut impls,
                    diagnostics,
                );
            }
            _ => {}
        }
    }

    for (struct_name, iface_map) in impls.into_iter() {
        for (iface_name, state) in iface_map {
            let Some(iface_def) = ctx.interfaces.get(&iface_name) else {
                continue;
            };

            let mut missing = Vec::new();
            for sig in &iface_def.methods {
                if !state.methods.contains_key(&sig.name) {
                    missing.push(sig.name.clone());
                }
            }

            if !missing.is_empty() {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "struct '{}' is missing implementations for interface '{}' methods: {}",
                        struct_name,
                        iface_name,
                        missing.join(", ")
                    ),
                    None,
                ));
                continue;
            }

            if !state.has_error {
                ctx.interface_impls
                    .entry(struct_name.clone())
                    .or_insert_with(HashMap::new)
                    .insert(iface_name.clone(), state.methods);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::{
        ast::{
            Block, Expr, ExternFunctionDecl, FieldDef, Function, Ident, InlineImpl, InterfaceDef,
            InterfaceMethod, Literal, Param, Stmt, StructDef,
        },
        span::Span,
    };

    fn make_span() -> Span {
        Span::new(0, 10)
    }

    fn path_ty(name: &str) -> TypeExpr {
        TypeExpr::Path {
            segments: vec![name.to_string()],
            span: make_span(),
        }
    }

    #[test]
    fn resolves_empty_module() {
        let module = Module {
            imports: vec![],
            items: vec![],
        };
        let (ctx, diags) = resolve_types(&module);

        assert!(ctx.struct_types.is_empty());
        assert!(diags.is_empty());
    }

    #[test]
    fn resolves_simple_struct() {
        let module = Module {
            imports: vec![],
            items: vec![Item::Struct(StructDef {
                name: Ident("Point".into()),
                generic_params: Vec::new(),
                fields: vec![
                    FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span: make_span(),
                    },
                    FieldDef {
                        name: Ident("y".into()),
                        ty: path_ty("i32"),
                        span: make_span(),
                    },
                ],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, diags) = resolve_types(&module);

        assert!(diags.is_empty());
        assert_eq!(ctx.struct_types.len(), 1);

        let point = ctx.get_struct("Point").unwrap();
        assert_eq!(point.name, "Point");
        assert_eq!(point.fields.len(), 2);
        assert_eq!(point.fields[0], ("x".to_string(), Type::I32));
        assert_eq!(point.fields[1], ("y".to_string(), Type::I32));
    }

    #[test]
    fn detects_duplicate_struct_definitions() {
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("top_left".into()),
                        ty: path_ty("Point"),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);

        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("struct 'Point' is defined multiple times"),
            "unexpected diagnostic: {:?}",
            diags
        );

        let point = ctx.get_struct("Point").unwrap();
        assert_eq!(point.fields[0].1, Type::Named("Point".to_string()));
    }

    #[test]
    fn allows_struct_with_struct_field() {
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Struct(StructDef {
                    generic_params: Vec::new(),
                    name: Ident("Point".into()),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
                Item::Struct(StructDef {
                    generic_params: Vec::new(),
                    name: Ident("Rect".into()),
                    fields: vec![FieldDef {
                        name: Ident("top_left".into()),
                        ty: path_ty("Point".into()),
                        span: make_span(),
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span: make_span(),
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);

        assert!(diags.is_empty());
        assert_eq!(ctx.struct_types.len(), 2);

        let rect = ctx.get_struct("Rect").unwrap();
        assert_eq!(rect.fields[0].1, Type::Named("Point".to_string()));
    }

    #[test]
    fn get_field_returns_correct_type_and_index() {
        let module = Module {
            imports: vec![],
            items: vec![Item::Struct(StructDef {
                generic_params: Vec::new(),
                name: Ident("Point".into()),
                fields: vec![
                    FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span: make_span(),
                    },
                    FieldDef {
                        name: Ident("y".into()),
                        ty: path_ty("i64"),
                        span: make_span(),
                    },
                ],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, _) = resolve_types(&module);

        let (ty, idx) = ctx.get_field("Point", "x").unwrap();
        assert_eq!(ty, Type::I32);
        assert_eq!(idx, 0);

        let (ty, idx) = ctx.get_field("Point", "y").unwrap();
        assert_eq!(ty, Type::I64);
        assert_eq!(idx, 1);

        assert!(ctx.get_field("Point", "z").is_none());
        assert!(ctx.get_field("NotAStruct", "x").is_none());
    }

    #[test]
    fn collects_interface_impl_methods() {
        let span = make_span();
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Drawable".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("draw".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(TypeExpr::Ref {
                                mutability: glyph_core::types::Mutability::Immutable,
                                inner: Box::new(path_ty("Point")),
                                span,
                            }),
                            span,
                        }],
                        ret_type: Some(path_ty("i32")),
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    generic_params: Vec::new(),
                    name: Ident("Point".into()),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span,
                    }],
                    interfaces: vec![Ident("Drawable".into())],
                    methods: Vec::new(),
                    inline_impls: vec![InlineImpl {
                        interface: Ident("Drawable".into()),
                        methods: vec![Function {
                            name: Ident("draw".into()),
                            params: vec![Param {
                                name: Ident("self".into()),
                                ty: Some(TypeExpr::Ref {
                                    mutability: glyph_core::types::Mutability::Immutable,
                                    inner: Box::new(path_ty("Point")),
                                    span,
                                }),
                                span,
                            }],
                            ret_type: Some(path_ty("i32")),
                            body: Block {
                                span,
                                stmts: vec![Stmt::Ret(
                                    Some(Expr::Lit(Literal::Int(1), span)),
                                    span,
                                )],
                            },
                            span,
                        }],
                        span,
                    }],
                    span,
                }),
            ],
        };

        let (ctx, diags) = resolve_types(&module);
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

        let iface_map = ctx.interface_impls.get("Point").expect("impls for Point");
        let drawable = iface_map.get("Drawable").expect("drawable impl");
        let info = drawable.get("draw").expect("draw method info");
        assert_eq!(info.function_name, "Point::Drawable::draw");
        assert!(matches!(info.self_kind, SelfKind::Ref));
    }

    #[test]
    fn reports_missing_interface_methods() {
        let span = make_span();
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Renderable".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("render".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(path_ty("Point".into())),
                            span,
                        }],
                        ret_type: None,
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    generic_params: Vec::new(),
                    name: Ident("Point".into()),
                    fields: vec![],
                    interfaces: vec![Ident("Renderable".into())],
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span,
                }),
            ],
        };

        let (_ctx, diags) = resolve_types(&module);
        assert!(diags.iter().any(|d| {
            d.message
                .contains("missing implementations for interface 'Renderable'")
        }));
    }

    #[test]
    fn resolves_extern_function_signature() {
        let module = Module {
            imports: vec![],
            items: vec![Item::ExternFunction(ExternFunctionDecl {
                abi: Some("C".into()),
                name: Ident("puts".into()),
                params: vec![Param {
                    name: Ident("msg".into()),
                    ty: Some(path_ty("RawPtr<i32>")),
                    span: make_span(),
                }],
                ret_type: Some(path_ty("i32")),
                link_name: None,
                span: make_span(),
            })],
        };

        let (mut ctx, diags) = resolve_types(&module);
        assert!(diags.is_empty(), "unexpected diags: {:?}", diags);
        let sig = ctx.extern_functions.get("puts").expect("extern sig");
        assert_eq!(sig.0.len(), 1);
        assert!(matches!(sig.0[0], Type::RawPtr(_)));
        assert_eq!(sig.1, Some(Type::I32));

        ctx.current_module = Some("main".into());
        let resolved = ctx.resolve_symbol("puts");
        assert!(
            matches!(resolved, Some(ResolvedSymbol::Function(_, ref name)) if name == "puts"),
            "extern symbol not resolved: {:?}",
            resolved
        );
    }

    #[test]
    fn map_rejects_wrong_arity() {
        let map_ty = TypeExpr::App {
            base: Box::new(path_ty("Map")),
            args: vec![path_ty("i32")],
            span: make_span(),
        };
        let module = Module {
            imports: vec![],
            items: vec![Item::Struct(StructDef {
                name: Ident("Box".into()),
                generic_params: Vec::new(),
                fields: vec![FieldDef {
                    name: Ident("entries".into()),
                    ty: map_ty,
                    span: make_span(),
                }],
                interfaces: Vec::new(),
                methods: Vec::new(),
                inline_impls: Vec::new(),
                span: make_span(),
            })],
        };

        let (ctx, mut diags) = resolve_types(&module);
        validate_map_usage(&module, &ctx, &mut diags);
        assert!(diags
            .iter()
            .any(|d| d.message.contains("Map expects 2 type arguments")));
    }

    #[test]
    fn map_requires_hash_impl_for_struct_keys() {
        let span = make_span();
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Hash".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("hash".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(TypeExpr::Ref {
                                mutability: glyph_core::types::Mutability::Immutable,
                                inner: Box::new(path_ty("Point")),
                                span,
                            }),
                            span,
                        }],
                        ret_type: Some(path_ty("u64")),
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span,
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Holder".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("map".into()),
                        ty: TypeExpr::App {
                            base: Box::new(path_ty("Map")),
                            args: vec![path_ty("Point"), path_ty("i32")],
                            span,
                        },
                        span,
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span,
                }),
            ],
        };

        let (ctx, mut diags) = resolve_types(&module);
        validate_map_usage(&module, &ctx, &mut diags);
        assert!(diags.iter().any(|d| {
            d.message
                .contains("Map key type 'Point' must implement Hash")
        }));
    }

    #[test]
    fn map_allows_struct_keys_with_hash_impl() {
        let span = make_span();
        let module = Module {
            imports: vec![],
            items: vec![
                Item::Interface(InterfaceDef {
                    name: Ident("Hash".into()),
                    methods: vec![InterfaceMethod {
                        name: Ident("hash".into()),
                        params: vec![Param {
                            name: Ident("self".into()),
                            ty: Some(TypeExpr::Ref {
                                mutability: glyph_core::types::Mutability::Immutable,
                                inner: Box::new(path_ty("Point")),
                                span,
                            }),
                            span,
                        }],
                        ret_type: Some(path_ty("u64")),
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Point".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("x".into()),
                        ty: path_ty("i32"),
                        span,
                    }],
                    interfaces: vec![Ident("Hash".into())],
                    methods: Vec::new(),
                    inline_impls: vec![InlineImpl {
                        interface: Ident("Hash".into()),
                        methods: vec![Function {
                            name: Ident("hash".into()),
                            params: vec![Param {
                                name: Ident("self".into()),
                                ty: Some(TypeExpr::Ref {
                                    mutability: glyph_core::types::Mutability::Immutable,
                                    inner: Box::new(path_ty("Point")),
                                    span,
                                }),
                                span,
                            }],
                            ret_type: Some(path_ty("u64")),
                            body: Block {
                                span,
                                stmts: vec![Stmt::Ret(
                                    Some(Expr::Lit(Literal::Int(0), span)),
                                    span,
                                )],
                            },
                            span,
                        }],
                        span,
                    }],
                    span,
                }),
                Item::Struct(StructDef {
                    name: Ident("Holder".into()),
                    generic_params: Vec::new(),
                    fields: vec![FieldDef {
                        name: Ident("map".into()),
                        ty: TypeExpr::App {
                            base: Box::new(path_ty("Map")),
                            args: vec![path_ty("Point"), path_ty("i32")],
                            span,
                        },
                        span,
                    }],
                    interfaces: Vec::new(),
                    methods: Vec::new(),
                    inline_impls: Vec::new(),
                    span,
                }),
            ],
        };

        let (ctx, mut diags) = resolve_types(&module);
        validate_map_usage(&module, &ctx, &mut diags);
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
    }
}

use std::collections::HashMap;

use glyph_core::ast::{BinaryOp, Block, Expr, Function, Ident, Item, Module, Stmt, TypeExpr};
use glyph_core::diag::Diagnostic;
use glyph_core::mir::{
    BlockId, Local, LocalId, MirBlock, MirExternFunction, MirFunction, MirInst, MirModule,
    MirValue, Rvalue,
};
use glyph_core::span::Span;
use glyph_core::types::{Mutability, Type};

use crate::resolver::ResolverContext;

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Option<Type>>,
    ret: Option<Type>,
    abi: Option<String>,
    /// The actual target name to call in MIR (unqualified, without aliases).
    target_name: String,
    enum_ctor: Option<EnumCtorInfo>,
}

#[derive(Debug, Clone)]
struct EnumCtorInfo {
    enum_name: String,
    variant_index: usize,
    has_generics: bool,
}

pub fn type_expr_to_string(ty: &TypeExpr) -> String {
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
            if matches!(mutability, Mutability::Mutable) {
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

#[derive(Debug, Clone)]
struct LoopContext {
    header: BlockId, // for continue
    exit: BlockId,   // for break
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LocalState {
    Uninitialized,
    Initialized,
    Moved,
}

pub fn lower_module(module: &Module, resolver: &ResolverContext) -> (MirModule, Vec<Diagnostic>) {
    let (fn_sigs, mut diagnostics) = collect_function_signatures(module, resolver);

    let mut mir = MirModule {
        struct_types: resolver.struct_types.clone(),
        enum_types: resolver.enum_types.clone(),
        functions: Vec::new(),
        extern_functions: Vec::new(),
    };

    for item in &module.items {
        match item {
            Item::Function(func) => {
                let (lowered, diags) = lower_function(func, resolver, &fn_sigs);
                diagnostics.extend(diags);
                mir.functions.push(lowered);
            }
            Item::Struct(_) | Item::Interface(_) | Item::Impl(_) => {
                // Handled during resolution or desugaring stages.
            }
            Item::Enum(_) => {
                // Enums are type-level; constructors are lowered via call sites.
            }
            Item::ExternFunction(func) => {
                let Some(sig) = fn_sigs.get(&func.name.0) else {
                    continue;
                };

                // Extern functions require fully known param types
                let mut params = Vec::new();
                for (idx, ty) in sig.params.iter().enumerate() {
                    if let Some(t) = ty.clone() {
                        params.push(t);
                    } else {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "extern function '{}' parameter {} has unknown type",
                                func.name.0,
                                idx + 1
                            ),
                            Some(func.span),
                        ));
                    }
                }

                let extern_fn = MirExternFunction {
                    name: func.name.0.clone(),
                    ret_type: sig.ret.clone(),
                    params,
                    abi: sig.abi.clone(),
                    link_name: func.link_name.clone(),
                };
                mir.extern_functions.push(extern_fn);
            }
        }
    }

    (mir, diagnostics)
}

fn collect_function_signatures(
    module: &Module,
    resolver: &ResolverContext,
) -> (HashMap<String, FnSig>, Vec<Diagnostic>) {
    let mut signatures = HashMap::new();
    let mut diagnostics = Vec::new();

    fn resolve_fn_sig(
        resolver: &ResolverContext,
        diagnostics: &mut Vec<Diagnostic>,
        key_name: &str,
        target_name: &str,
        params: &[glyph_core::ast::Param],
        ret_type: &Option<TypeExpr>,
        abi: Option<String>,
        is_extern: bool,
        span: Span,
    ) -> FnSig {
        let mut param_types = Vec::new();
        for param in params {
            let ty = match &param.ty {
                Some(t) => {
                    match crate::resolver::resolve_type_expr_to_type(t, resolver) {
                        Some(resolved) => Some(resolved),
                        None => {
                            let ty_str = type_expr_to_string(t);
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "unknown type '{}' for parameter '{}'",
                                    ty_str, param.name.0
                                ),
                                Some(param.span),
                            ));
                            None
                        }
                    }
                }
                None => {
                    if is_extern {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "extern function '{}' parameter '{}' must have a type",
                                key_name, param.name.0
                            ),
                            Some(param.span),
                        ));
                    }
                    None
                }
            };
            param_types.push(ty);
        }

        let ret_type = match ret_type {
            Some(t) => {
                match crate::resolver::resolve_type_expr_to_type(t, resolver) {
                    Some(resolved) => Some(resolved),
                    None => {
                        let ty_str = type_expr_to_string(t);
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "unknown return type '{}' for function '{}'",
                                ty_str, key_name
                            ),
                            Some(span),
                        ));
                        None
                    }
                }
            }
            None => None,
        };

        FnSig {
            params: param_types,
            ret: ret_type,
            abi,
            target_name: target_name.to_string(),
            enum_ctor: None,
        }
    }

    for item in &module.items {
        match item {
            Item::Function(func) => {
                let name = func.name.0.clone();
                if signatures.contains_key(&name) {
                    diagnostics.push(Diagnostic::error(
                        format!("function '{}' is defined multiple times", name),
                        Some(func.span),
                    ));
                    continue;
                }

                let sig = resolve_fn_sig(
                    resolver,
                    &mut diagnostics,
                    &name,
                    &name,
                    &func.params,
                    &func.ret_type,
                    None,
                    false,
                    func.span,
                );
                signatures.insert(name, sig);
            }
            Item::ExternFunction(func) => {
                let name = func.name.0.clone();
                if signatures.contains_key(&name) {
                    diagnostics.push(Diagnostic::error(
                        format!("function '{}' is defined multiple times", name),
                        Some(func.span),
                    ));
                    continue;
                }

                let sig = resolve_fn_sig(
                    resolver,
                    &mut diagnostics,
                    &name,
                    &name,
                    &func.params,
                    &func.ret_type,
                    func.abi.clone(),
                    true,
                    func.span,
                );
                signatures.insert(name, sig);
            }
            _ => {}
        }
    }

    // Also make local functions/externs addressable via fully-qualified module path.
    if let Some(module_id) = &resolver.current_module {
        let module_prefix = module_id.replace('/', "::");
        let local_names: Vec<String> = module
            .items
            .iter()
            .filter_map(|item| match item {
                Item::Function(f) => Some(f.name.0.clone()),
                Item::ExternFunction(f) => Some(f.name.0.clone()),
                _ => None,
            })
            .collect();

        for name in local_names {
            let qualified = format!("{}::{}", module_prefix, name);
            if signatures.contains_key(&qualified) {
                continue;
            }
            if let Some(sig) = signatures.get(&name).cloned() {
                signatures.insert(qualified, sig);
            }
        }
    }

    // Enum variant constructors act like functions returning the enum type.
    for (enum_name, enum_ty) in &resolver.enum_types {
        let has_generics = enum_has_generics(enum_name, module, resolver);
        for (idx, variant) in enum_ty.variants.iter().enumerate() {
            let ctor_name = format!("{}::{}", enum_name, variant.name);
            let mut params = Vec::new();
            if let Some(payload_ty) = &variant.payload {
                params.push(Some(payload_ty.clone()));
            }
            let ctor_sig = FnSig {
                params: params.clone(),
                ret: Some(Type::Enum(enum_name.clone())),
                abi: None,
                target_name: ctor_name.clone(),
                enum_ctor: Some(EnumCtorInfo {
                    enum_name: enum_name.clone(),
                    variant_index: idx,
                    has_generics,
                }),
            };
            if !signatures.contains_key(&ctor_name) {
                signatures.insert(ctor_name.clone(), ctor_sig.clone());
            }

            // Also make unqualified variant name available if not shadowed.
            if !signatures.contains_key(&variant.name) {
                let mut unqualified_sig = ctor_sig.clone();
                unqualified_sig.target_name = ctor_name;
                signatures.insert(variant.name.clone(), unqualified_sig);
            }
        }
    }

    // Add signatures for imported symbols so qualified std calls resolve correctly.
    if let (Some(import_scope), Some(all_modules)) = (&resolver.import_scope, &resolver.all_modules)
    {
        // Selective imports (with optional alias)
        for (local_name, (source_module, original_name)) in &import_scope.direct_symbols {
            if signatures.contains_key(local_name) {
                continue;
            }

            if let Some(module) = all_modules.modules.get(source_module) {
                for item in &module.items {
                    match item {
                        Item::Function(func) if func.name.0 == *original_name => {
                            let sig = resolve_fn_sig(
                                resolver,
                                &mut diagnostics,
                                local_name,
                                &func.name.0,
                                &func.params,
                                &func.ret_type,
                                None,
                                false,
                                func.span,
                            );
                            signatures.insert(local_name.clone(), sig);
                            break;
                        }
                        Item::ExternFunction(func) if func.name.0 == *original_name => {
                            let sig = resolve_fn_sig(
                                resolver,
                                &mut diagnostics,
                                local_name,
                                &func.name.0,
                                &func.params,
                                &func.ret_type,
                                func.abi.clone(),
                                true,
                                func.span,
                            );
                            signatures.insert(local_name.clone(), sig);
                            break;
                        }
                        _ => {}
                    }
                }
            }
        }

        // Wildcard imports: make qualified names available (e.g., std::io::fopen).
        for wildcard in &import_scope.wildcard_modules {
            for (module_id, module) in all_modules
                .modules
                .iter()
                .filter(|(id, _)| *id == wildcard || id.starts_with(&format!("{}/", wildcard)))
            {
                let module_prefix = module_id.replace('/', "::");
                for item in &module.items {
                    match item {
                        Item::Function(func) => {
                            let key = format!("{}::{}", module_prefix, func.name.0);
                            if signatures.contains_key(&key) {
                                continue;
                            }
                            let sig = resolve_fn_sig(
                                resolver,
                                &mut diagnostics,
                                &key,
                                &func.name.0,
                                &func.params,
                                &func.ret_type,
                                None,
                                false,
                                func.span,
                            );
                            signatures.insert(key, sig);
                        }
                        Item::ExternFunction(func) => {
                            let key = format!("{}::{}", module_prefix, func.name.0);
                            if signatures.contains_key(&key) {
                                continue;
                            }
                            let sig = resolve_fn_sig(
                                resolver,
                                &mut diagnostics,
                                &key,
                                &func.name.0,
                                &func.params,
                                &func.ret_type,
                                func.abi.clone(),
                                true,
                                func.span,
                            );
                            signatures.insert(key, sig);
                        }
                        _ => {}
                    }
                }
            }
        }

        // Built-in std helpers needed for print/println even without explicit imports.
        if all_modules.module_symbols.contains_key("std/io") {
            let mut insert_builtin =
                |key: &str, params: Vec<Option<Type>>, ret: Option<Type>, abi: Option<String>| {
                    if signatures.contains_key(key) {
                        return;
                    }
                    signatures.insert(
                        key.to_string(),
                        FnSig {
                            params,
                            ret,
                            abi,
                            target_name: key.split("::").last().unwrap_or(key).to_string(),
                            enum_ctor: None,
                        },
                    );
                };

            insert_builtin(
                "std::io::raw_write",
                vec![
                    Some(Type::I32),
                    Some(Type::RawPtr(Box::new(Type::U8))),
                    Some(Type::U32),
                ],
                Some(Type::I32),
                Some("C".into()),
            );

            let stdout_ref = Type::Ref(Box::new(Type::Named("Stdout".into())), Mutability::Mutable);
            let fmt_builtins = [
                ("std::fmt::fmt_i32", Type::I32),
                ("std::fmt::fmt_u32", Type::U32),
                ("std::fmt::fmt_i64", Type::I64),
                ("std::fmt::fmt_u64", Type::U64),
                ("std::fmt::fmt_bool", Type::Bool),
                ("std::fmt::fmt_str", Type::Str),
                ("std::fmt::fmt_char", Type::Char),
            ];

            for (key, ty) in fmt_builtins {
                insert_builtin(
                    key,
                    vec![Some(ty.clone()), Some(stdout_ref.clone())],
                    None,
                    None,
                );
            }
        }
    }

    (signatures, diagnostics)
}

struct LowerCtx<'a> {
    resolver: &'a ResolverContext,
    fn_sigs: &'a HashMap<String, FnSig>,
    diagnostics: Vec<Diagnostic>,
    locals: Vec<Local>,
    bindings: HashMap<&'a str, LocalId>,
    next_local: u32,
    function_name: String,
    blocks: Vec<MirBlock>,
    current: BlockId,
    loop_stack: Vec<LoopContext>,
    scope_stack: Vec<Vec<LocalId>>,
    local_states: Vec<LocalState>,
    string_counter: u32,
}

impl<'a> LowerCtx<'a> {
    fn new(
        resolver: &'a ResolverContext,
        fn_sigs: &'a HashMap<String, FnSig>,
        function_name: String,
    ) -> Self {
        let mut blocks = Vec::new();
        blocks.push(MirBlock::default());
        Self {
            resolver,
            fn_sigs,
            diagnostics: Vec::new(),
            locals: Vec::new(),
            bindings: HashMap::new(),
            next_local: 0,
            function_name,
            blocks,
            current: BlockId(0),
            loop_stack: Vec::new(),
            scope_stack: vec![Vec::new()],
            local_states: Vec::new(),
            string_counter: 0,
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    fn current_block_mut(&mut self) -> &mut MirBlock {
        &mut self.blocks[self.current.0 as usize]
    }

    fn push_inst(&mut self, inst: MirInst) {
        if let MirInst::Assign { local, .. } = &inst {
            self.handle_reassign(*local);
            if let Some(state) = self.local_states.get_mut(local.0 as usize) {
                *state = LocalState::Initialized;
            }
        }
        self.current_block_mut().insts.push(inst);
    }

    fn terminated(&self) -> bool {
        self.blocks[self.current.0 as usize]
            .insts
            .last()
            .map(|inst| matches!(inst, MirInst::Return(_) | MirInst::Goto(_)))
            .unwrap_or(false)
    }

    fn fresh_local(&mut self, name: Option<&'a str>) -> LocalId {
        let id = LocalId(self.next_local);
        self.next_local += 1;
        self.locals.push(Local {
            name: name.map(|s| s.to_string()),
            ty: None,
            mutable: false,
        });
        self.local_states.push(LocalState::Uninitialized);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.push(id);
        }
        id
    }

    fn fresh_string_global(&mut self) -> String {
        let fn_part = self.function_name.replace("::", "_");
        let name = format!(".str.{}.{}", fn_part, self.string_counter);
        self.string_counter += 1;
        name
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(MirBlock::default());
        id
    }

    fn switch_to(&mut self, id: BlockId) {
        self.current = id;
    }

    fn enter_loop(&mut self, header: BlockId, exit: BlockId) {
        self.loop_stack.push(LoopContext { header, exit });
    }

    fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    fn current_loop(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    fn handle_reassign(&mut self, local: LocalId) {
        if !self.local_needs_drop(local) {
            return;
        }
        if let Some(LocalState::Initialized) = self.local_states.get(local.0 as usize) {
            self.emit_drop(local);
        }
    }

    fn enter_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    fn exit_scope(&mut self) {
        if self.scope_stack.len() <= 1 {
            return;
        }
        if let Some(locals) = self.scope_stack.pop() {
            for local in locals.into_iter().rev() {
                self.drop_local_if_needed(local);
            }
        }
    }

    fn drop_local_if_needed(&mut self, local: LocalId) {
        if !self.local_needs_drop(local) {
            return;
        }
        let idx = local.0 as usize;
        if matches!(self.local_states.get(idx), Some(LocalState::Initialized)) {
            self.emit_drop(local);
        }
    }

    fn drop_all_active_locals(&mut self) {
        let scopes: Vec<Vec<LocalId>> = self.scope_stack.clone();
        for scope in scopes.iter().rev() {
            for &local in scope.iter().rev() {
                self.drop_local_if_needed(local);
            }
        }
    }

    fn emit_drop(&mut self, local: LocalId) {
        self.current_block_mut().insts.push(MirInst::Drop(local));
        if let Some(state) = self.local_states.get_mut(local.0 as usize) {
            *state = LocalState::Moved;
        }
    }

    fn local_ty(&self, local: LocalId) -> Option<&Type> {
        self.locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.as_ref())
    }

    fn local_needs_drop(&self, local: LocalId) -> bool {
        self.local_ty(local)
            .map(|ty| matches!(ty, Type::Own(_) | Type::Shared(_) | Type::String))
            .unwrap_or(false)
    }

    fn consume_local(&mut self, local: LocalId, span: Option<Span>) -> bool {
        // Shared pointers are copyable - they don't move
        let ty = self.local_ty(local);
        if matches!(ty, Some(Type::Shared(_))) {
            return true; // Always allow reuse
        }

        if !self.local_needs_drop(local) {
            return true;
        }
        let idx = local.0 as usize;
        match self.local_states.get(idx) {
            Some(LocalState::Moved) => {
                self.error("use of moved value", span);
                false
            }
            Some(LocalState::Initialized) => {
                if let Some(state) = self.local_states.get_mut(idx) {
                    *state = LocalState::Moved;
                }
                true
            }
            _ => {
                self.error("use of uninitialized ownership pointer", span);
                if let Some(state) = self.local_states.get_mut(idx) {
                    *state = LocalState::Moved;
                }
                false
            }
        }
    }
}

fn imports_sys_argv(resolver: &ResolverContext) -> bool {
    resolver
        .import_scope
        .as_ref()
        .and_then(|scope| scope.direct_symbols.get("argv"))
        .map(|(module, name)| module == "std/sys" && name == "argv")
        .unwrap_or(false)
}

fn lower_function(
    func: &Function,
    resolver: &ResolverContext,
    fn_sigs: &HashMap<String, FnSig>,
) -> (MirFunction, Vec<Diagnostic>) {
    let mut ctx = LowerCtx::new(resolver, fn_sigs, func.name.0.clone());

    // Resolve return type
    let ret_type = func
        .ret_type
        .as_ref()
        .and_then(|t| crate::resolver::resolve_type_expr_to_type(t, resolver));

    // Create locals for parameters and bind them
    let mut param_locals = Vec::new();
    for param in &func.params {
        let local = ctx.fresh_local(Some(&param.name.0));
        // Try to infer parameter type from annotation
        if let Some(ty) = param
            .ty
            .as_ref()
            .and_then(|t| crate::resolver::resolve_type_expr_to_type(t, resolver))
        {
            ctx.locals[local.0 as usize].ty = Some(ty);
        }
        if let Some(state) = ctx.local_states.get_mut(local.0 as usize) {
            *state = LocalState::Initialized;
        }
        ctx.bindings.insert(&param.name.0, local);
        param_locals.push(local);
    }

    if imports_sys_argv(resolver) && !ctx.bindings.contains_key("argv") {
        let argv_local = ctx.fresh_local(Some("argv"));
        ctx.locals[argv_local.0 as usize].ty = Some(Type::App {
            base: "Vec".into(),
            args: vec![Type::String],
        });
        if let Some(state) = ctx.local_states.get_mut(argv_local.0 as usize) {
            *state = LocalState::Initialized;
        }
        ctx.bindings.insert("argv", argv_local);
        ctx.push_inst(MirInst::Assign {
            local: argv_local,
            value: Rvalue::Call {
                name: "argv".into(),
                args: Vec::new(),
            },
        });
    }

    let implicit_return = lower_block(&mut ctx, &func.body);
    if !ctx.terminated() {
        // If block returned a value, use it; otherwise return void
        ctx.drop_all_active_locals();
        ctx.push_inst(MirInst::Return(implicit_return));
    }

    let lowered = MirFunction {
        name: func.name.0.clone(),
        ret_type,
        params: param_locals,
        locals: ctx.locals,
        blocks: ctx.blocks,
    };

    (lowered, ctx.diagnostics)
}

fn lower_block<'a>(ctx: &mut LowerCtx<'a>, block: &'a Block) -> Option<MirValue> {
    ctx.enter_scope();
    let mut last_value = None;

    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;

        match stmt {
            Stmt::Let {
                name, mutable, ty, value, ..
            } => {
                let local = ctx.fresh_local(Some(&name.0));
                ctx.bindings.insert(&name.0, local);

                // Set mutability
                ctx.locals[local.0 as usize].mutable = *mutable;

                if let Some(annot_ty) = ty
                    .as_ref()
                    .and_then(|t| resolve_type_name(&type_expr_to_string(t), ctx.resolver))
                {
                    ctx.locals[local.0 as usize].ty = Some(annot_ty);
                }

                if let Some(rv) = value.as_ref().and_then(|e| lower_expr(ctx, e)) {
                    update_local_type_from_rvalue(ctx, local, &rv);
                    ctx.push_inst(MirInst::Assign { local, value: rv });
                } else {
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Ret(expr, _) => {
                let value = expr.as_ref().and_then(|e| lower_value(ctx, e));
                ctx.drop_all_active_locals();
                ctx.push_inst(MirInst::Return(value));
            }
            Stmt::Break(span) => {
                if let Some(loop_ctx) = ctx.current_loop() {
                    let exit = loop_ctx.exit;
                    ctx.push_inst(MirInst::Goto(exit));
                } else {
                    ctx.error("break statement outside of loop", Some(*span));
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Continue(span) => {
                if let Some(loop_ctx) = ctx.current_loop() {
                    let header = loop_ctx.header;
                    ctx.push_inst(MirInst::Goto(header));
                } else {
                    ctx.error("continue statement outside of loop", Some(*span));
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Expr(expr, _) => {
                if is_last {
                    // Last expression is the implicit return value
                    match expr {
                        Expr::If {
                            cond,
                            then_block,
                            else_block,
                            ..
                        } => {
                            lower_if(ctx, cond, then_block, else_block.as_ref());
                            // If expressions can return values, but we need to handle this
                            // For now, treat as statement
                        }
                        Expr::While { cond, body, .. } => {
                            lower_while(ctx, cond, body);
                        }
                        Expr::For {
                            var,
                            start,
                            end,
                            body,
                            ..
                        } => {
                            lower_for(ctx, var, start, end, body);
                        }
                        _ => {
                            last_value = lower_value(ctx, expr);
                        }
                    }
                } else {
                    match expr {
                        Expr::If {
                            cond,
                            then_block,
                            else_block,
                            ..
                        } => lower_if(ctx, cond, then_block, else_block.as_ref()),
                        Expr::While { cond, body, .. } => lower_while(ctx, cond, body),
                        Expr::For {
                            var,
                            start,
                            end,
                            body,
                            ..
                        } => lower_for(ctx, var, start, end, body),
                        _ => {
                            let _ = lower_expr(ctx, expr);
                            ctx.push_inst(MirInst::Nop);
                        }
                    }
                }
            }
            Stmt::Assign {
                target,
                value,
                span: _,
            } => {
                if let Some(local) = lower_assignment_target(ctx, target) {
                    if let Some(rv) = lower_expr(ctx, value) {
                        update_local_type_from_rvalue(ctx, local, &rv);
                        ctx.push_inst(MirInst::Assign { local, value: rv });
                    } else {
                        ctx.push_inst(MirInst::Nop);
                    }
                } else {
                    ctx.push_inst(MirInst::Nop);
                }
            }
        }
    }

    let result = last_value;
    ctx.exit_scope();
    result
}

fn rvalue_from_value(val: MirValue) -> Option<Rvalue> {
    match val {
        MirValue::Int(i) => Some(Rvalue::ConstInt(i)),
        MirValue::Bool(b) => Some(Rvalue::ConstBool(b)),
        MirValue::Local(id) => Some(Rvalue::Move(id)),
        MirValue::Unit => None,
    }
}

fn lower_match<'a>(
    ctx: &mut LowerCtx<'a>,
    scrutinee: &'a Expr,
    arms: &'a [glyph_core::ast::MatchArm],
    require_value: bool,
    span: Span,
) -> Option<Rvalue> {
    // Evaluate scrutinee into a local
    let scrut_val = lower_value(ctx, scrutinee)?;
    let scrut_local = match scrut_val {
        MirValue::Local(id) => id,
        MirValue::Int(_) | MirValue::Bool(_) | MirValue::Unit => {
            let tmp = ctx.fresh_local(None);
            let rv = rvalue_from_value(scrut_val)?;
            ctx.locals[tmp.0 as usize].ty = None;
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: rv,
            });
            tmp
        }
    };

    let enum_name = match ctx.locals[scrut_local.0 as usize].ty.clone() {
        Some(Type::Enum(name)) => name,
        Some(Type::App { base, .. }) => base,
        _ => {
            ctx.error("match scrutinee must be an enum value", Some(span));
            return None;
        }
    };

    let enum_def = match ctx.resolver.get_enum(&enum_name) {
        Some(e) => e.clone(),
        None => {
            ctx.error(format!("unknown enum type '{}'", enum_name), Some(span));
            return None;
        }
    };

    // Exhaustiveness check
    let mut seen_variants = std::collections::HashSet::new();
    let mut has_wildcard = false;
    for arm in arms {
        match &arm.pattern {
            glyph_core::ast::MatchPattern::Wildcard => has_wildcard = true,
            glyph_core::ast::MatchPattern::Variant { name, .. } => {
                seen_variants.insert(name.0.clone());
            }
        }
    }
    if !has_wildcard && seen_variants.len() < enum_def.variants.len() {
        ctx.error(
            "non-exhaustive match: cover all variants or add `_` arm",
            Some(span),
        );
    }

    // Tag extraction
    let tag_local = ctx.fresh_local(None);
    ctx.locals[tag_local.0 as usize].ty = Some(Type::I32);
    ctx.push_inst(MirInst::Assign {
        local: tag_local,
        value: Rvalue::EnumTag { base: scrut_local },
    });

    // Result local if needed
    let mut result_local = None;

    let join_block = ctx.new_block();
    let mut arm_blocks = Vec::new();
    for _ in arms {
        arm_blocks.push(ctx.new_block());
    }

    // Build chain of comparisons
    let mut current = ctx.current;
    for (idx, arm) in arms.iter().enumerate() {
        let arm_block = arm_blocks[idx];
        match &arm.pattern {
            glyph_core::ast::MatchPattern::Wildcard => {
                ctx.blocks[current.0 as usize]
                    .insts
                    .push(MirInst::Goto(arm_block));
                break;
            }
            glyph_core::ast::MatchPattern::Variant { name, .. } => {
                let variant_index = enum_def
                    .variants
                    .iter()
                    .position(|v| v.name == name.0)
                    .unwrap_or_else(|| {
                        ctx.error(
                            format!("unknown variant '{}' for enum '{}'", name.0, enum_name),
                            Some(arm.span),
                        );
                        0
                    });
                let cond_local = ctx.fresh_local(None);
                ctx.locals[cond_local.0 as usize].ty = Some(Type::Bool);
                ctx.push_inst(MirInst::Assign {
                    local: cond_local,
                    value: Rvalue::Binary {
                        op: BinaryOp::Eq,
                        lhs: MirValue::Local(tag_local),
                        rhs: MirValue::Int(variant_index as i64),
                    },
                });

                let else_block = if idx == arms.len() - 1 {
                    let sink = ctx.new_block();
                    ctx.blocks[sink.0 as usize]
                        .insts
                        .push(MirInst::Goto(join_block));
                    sink
                } else {
                    ctx.new_block()
                };

                ctx.blocks[current.0 as usize].insts.push(MirInst::If {
                    cond: MirValue::Local(cond_local),
                    then_bb: arm_block,
                    else_bb: else_block,
                });

                current = else_block;
            }
        }
    }

    // Lower arms
    for (idx, arm) in arms.iter().enumerate() {
        let arm_block = arm_blocks[idx];
        ctx.switch_to(arm_block);
        ctx.enter_scope();

        if let glyph_core::ast::MatchPattern::Variant { name, binding } = &arm.pattern {
            if let Some(bind_ident) = binding {
                if let Some(variant) = enum_def.variants.iter().find(|v| v.name == name.0) {
                    if let Some(payload_ty) = variant.payload.clone() {
                        let binding_local = ctx.fresh_local(Some(&bind_ident.0));
                        ctx.locals[binding_local.0 as usize].ty = Some(payload_ty.clone());
                        ctx.bindings.insert(&bind_ident.0, binding_local);
                        ctx.push_inst(MirInst::Assign {
                            local: binding_local,
                            value: Rvalue::EnumPayload {
                                base: scrut_local,
                                variant_index: enum_def
                                    .variants
                                    .iter()
                                    .position(|v| v.name == name.0)
                                    .unwrap_or(0)
                                    as u32,
                                payload_type: payload_ty,
                            },
                        });
                    }
                }
            }
        }

        let arm_val = lower_value(ctx, &arm.expr);
        if require_value {
            if let Some(val) = arm_val {
                let res_local = *result_local.get_or_insert_with(|| {
                    let l = ctx.fresh_local(None);
                    ctx.locals[l.0 as usize].ty = match &val {
                        MirValue::Local(id) => ctx.locals[id.0 as usize].ty.clone(),
                        MirValue::Int(_) => Some(Type::I32),
                        MirValue::Bool(_) => Some(Type::Bool),
                        MirValue::Unit => None,
                    };
                    l
                });

                if let Some(rv) = rvalue_from_value(val) {
                    ctx.push_inst(MirInst::Assign {
                        local: res_local,
                        value: rv,
                    });
                }
            }
        }

        if !ctx.terminated() {
            ctx.push_inst(MirInst::Goto(join_block));
        }
        ctx.exit_scope();
    }

    ctx.switch_to(join_block);

    result_local.map(Rvalue::Move)
}

fn lower_if<'a>(
    ctx: &mut LowerCtx<'a>,
    cond: &'a Expr,
    then_blk: &'a Block,
    else_blk: Option<&'a Block>,
) {
    let header = ctx.current;
    let then_id = ctx.new_block();
    let else_id = else_blk.map(|_| ctx.new_block());
    let join_id = ctx.new_block();

    let cond_val = lower_value(ctx, cond).unwrap_or(MirValue::Bool(false));
    ctx.blocks[header.0 as usize].insts.push(MirInst::If {
        cond: cond_val,
        then_bb: then_id,
        else_bb: else_id.unwrap_or(join_id),
    });

    ctx.switch_to(then_id);
    let _ = lower_block(ctx, then_blk);
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(join_id));
    }

    if let Some(else_block) = else_blk {
        let else_id = else_id.unwrap();
        ctx.switch_to(else_id);
        let _ = lower_block(ctx, else_block);
        if !ctx.terminated() {
            ctx.push_inst(MirInst::Goto(join_id));
        }
    }

    ctx.switch_to(join_id);
}

fn lower_while<'a>(ctx: &mut LowerCtx<'a>, cond: &'a Expr, body_block: &'a Block) {
    let header_block = ctx.new_block();
    let body_bb = ctx.new_block();
    let exit_bb = ctx.new_block();

    // Jump from current block to header
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Header: evaluate condition and branch
    ctx.switch_to(header_block);
    let cond_val = lower_value(ctx, cond).unwrap_or(MirValue::Bool(false));
    ctx.push_inst(MirInst::If {
        cond: cond_val,
        then_bb: body_bb,
        else_bb: exit_bb,
    });

    // Body: lower statements, add back edge
    ctx.switch_to(body_bb);
    ctx.enter_loop(header_block, exit_bb);
    let _ = lower_block(ctx, body_block);
    ctx.exit_loop();

    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block)); // Back edge
    }

    // Exit: continue after loop
    ctx.switch_to(exit_bb);
}

fn lower_for<'a>(
    ctx: &mut LowerCtx<'a>,
    var: &'a Ident,
    start: &'a Expr,
    end: &'a Expr,
    body_block: &'a Block,
) {
    // Initialize loop variable: let var = start
    let var_local = ctx.fresh_local(Some(&var.0));
    ctx.bindings.insert(&var.0, var_local);

    if let Some(rv) = lower_expr(ctx, start) {
        update_local_type_from_rvalue(ctx, var_local, &rv);
        ctx.push_inst(MirInst::Assign {
            local: var_local,
            value: rv,
        });
    } else {
        ctx.push_inst(MirInst::Nop);
    }

    // Create blocks for while loop structure
    let header_block = ctx.new_block();
    let body_bb = ctx.new_block();
    let exit_bb = ctx.new_block();

    // Jump to header
    if !ctx.terminated() {
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Header: check condition (var < end)
    ctx.switch_to(header_block);
    let var_val = MirValue::Local(var_local);
    let end_val = lower_value(ctx, end).unwrap_or(MirValue::Int(0));
    let cond_temp = ctx.fresh_local(None);
    ctx.locals[cond_temp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: cond_temp,
        value: Rvalue::Binary {
            op: BinaryOp::Lt,
            lhs: var_val.clone(),
            rhs: end_val,
        },
    });
    ctx.push_inst(MirInst::If {
        cond: MirValue::Local(cond_temp),
        then_bb: body_bb,
        else_bb: exit_bb,
    });

    // Body: execute loop body and increment
    ctx.switch_to(body_bb);
    ctx.enter_loop(header_block, exit_bb);
    let _ = lower_block(ctx, body_block);
    ctx.exit_loop();

    // Increment: var = var + 1
    if !ctx.terminated() {
        let inc_temp = ctx.fresh_local(None);
        ctx.locals[inc_temp.0 as usize].ty = Some(Type::I32);
        ctx.push_inst(MirInst::Assign {
            local: inc_temp,
            value: Rvalue::Binary {
                op: BinaryOp::Add,
                lhs: var_val.clone(),
                rhs: MirValue::Int(1),
            },
        });
        ctx.push_inst(MirInst::Assign {
            local: var_local,
            value: Rvalue::Move(inc_temp),
        });
        ctx.push_inst(MirInst::Goto(header_block));
    }

    // Exit: continue after loop
    ctx.switch_to(exit_bb);
}

fn lower_expr<'a>(ctx: &mut LowerCtx<'a>, expr: &'a Expr) -> Option<Rvalue> {
    match expr {
        Expr::Lit(glyph_core::ast::Literal::Int(i), _) => Some(Rvalue::ConstInt(*i)),
        Expr::Lit(glyph_core::ast::Literal::Bool(b), _) => Some(Rvalue::ConstBool(*b)),
        Expr::Lit(glyph_core::ast::Literal::Str(s), _) => Some(Rvalue::StringLit {
            content: s.clone(),
            global_name: ctx.fresh_string_global(),
        }),
        Expr::InterpString { segments, span } => lower_interp_string(ctx, segments, *span),
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => lower_match(ctx, scrutinee, arms, true, *span),
        Expr::Ident(ident, span) => ctx
            .bindings
            .get(ident.0.as_str())
            .copied()
            .and_then(|local| {
                if ctx.consume_local(local, Some(*span)) {
                    Some(Rvalue::Move(local))
                } else {
                    None
                }
            }),
        Expr::Binary { op, lhs, rhs, .. } => match *op {
            glyph_core::ast::BinaryOp::And | glyph_core::ast::BinaryOp::Or => {
                lower_logical(ctx, op, lhs, rhs)
            }
            _ => lower_binary(ctx, op, lhs, rhs),
        },
        Expr::Call { callee, args, span } => lower_call(ctx, callee, args, *span, false),
        Expr::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            lower_if(ctx, cond, then_block, else_block.as_ref());
            None
        }
        Expr::While { cond, body, .. } => {
            lower_while(ctx, cond, body);
            None
        }
        Expr::For {
            var,
            start,
            end,
            body,
            ..
        } => {
            lower_for(ctx, var, start, end, body);
            None
        }
        Expr::StructLit { name, fields, span } => lower_struct_lit(ctx, name, fields, *span),
        Expr::FieldAccess { base, field, span } => lower_field_access(ctx, base, field, *span),
        Expr::Ref {
            expr,
            mutability,
            span,
        } => lower_ref_expr(ctx, expr, *mutability, *span),
        Expr::ArrayLit { elements, span } => lower_array_lit(ctx, elements, *span),
        Expr::Index { base, index, span } => lower_array_index(ctx, base, index, *span),
        Expr::MethodCall {
            receiver,
            method,
            args,
            span,
        } => lower_method_call(ctx, receiver, method, args, *span),
        Expr::Tuple { elements, span } => lower_tuple_expr(ctx, elements, *span),
        _ => None,
    }
}

fn lower_binary<'a>(
    ctx: &mut LowerCtx<'a>,
    op: &glyph_core::ast::BinaryOp,
    lhs: &'a Expr,
    rhs: &'a Expr,
) -> Option<Rvalue> {
    let lhs_val = lower_value(ctx, lhs)?;
    let rhs_val = lower_value(ctx, rhs)?;
    let tmp = ctx.fresh_local(None);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::Binary {
            op: op.clone(),
            lhs: lhs_val,
            rhs: rhs_val,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_interp_string(
    ctx: &mut LowerCtx<'_>,
    segments: &[glyph_core::ast::InterpSegment],
    span: Span,
) -> Option<Rvalue> {
    let mut buf = String::new();
    for seg in segments {
        match seg {
            glyph_core::ast::InterpSegment::Literal(s) => buf.push_str(s),
            glyph_core::ast::InterpSegment::Expr(_) => {
                ctx.error(
                    "interpolation holes are not yet lowered; only literal segments are supported",
                    Some(span),
                );
                return None;
            }
        }
    }

    Some(Rvalue::StringLit {
        content: buf,
        global_name: ctx.fresh_string_global(),
    })
}

#[derive(Clone)]
enum PrintSegment<'a> {
    Literal(String),
    Expr(&'a Expr, Span),
}

fn resolve_builtin_target(ctx: &mut LowerCtx<'_>, key: &str, span: Span) -> Option<String> {
    if let Some(sig) = ctx.fn_sigs.get(key) {
        Some(sig.target_name.clone())
    } else {
        ctx.error(format!("unknown function '{}'", key), Some(span));
        None
    }
}

fn lower_print_builtin<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    add_newline: bool,
    is_err: bool,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("print/println expects exactly one argument", Some(span));
        return None;
    }

    let fd = if is_err { 2 } else { 1 };
    let arg = &args[0];

    let mut segments: Vec<PrintSegment<'a>> = Vec::new();
    match arg {
        Expr::Lit(glyph_core::ast::Literal::Str(s), _) => {
            segments.push(PrintSegment::Literal(s.clone()));
        }
        Expr::InterpString {
            segments: segs,
            span: interp_span,
        } => {
            for seg in segs {
                match seg {
                    glyph_core::ast::InterpSegment::Literal(s) => {
                        segments.push(PrintSegment::Literal(s.clone()))
                    }
                    glyph_core::ast::InterpSegment::Expr(e) => {
                        segments.push(PrintSegment::Expr(e, *interp_span))
                    }
                }
            }
        }
        _ => {
            ctx.error(
                "print/println currently require a string or interpolated string",
                Some(span),
            );
            return None;
        }
    }

    if segments.is_empty() {
        segments.push(PrintSegment::Literal(String::new()));
    }

    if add_newline {
        segments.push(PrintSegment::Literal("\n".into()));
    }

    // Build writer: Stdout or Stderr
    let writer_struct = if is_err { "Stderr" } else { "Stdout" };
    let writer_local = ctx.fresh_local(None);
    ctx.locals[writer_local.0 as usize].ty = Some(Type::Named(writer_struct.to_string()));
    ctx.push_inst(MirInst::Assign {
        local: writer_local,
        value: Rvalue::StructLit {
            struct_name: writer_struct.to_string(),
            field_values: vec![("fd".into(), MirValue::Int(fd))],
        },
    });

    let mut last = None;
    for seg in segments {
        match seg {
            PrintSegment::Literal(s) => {
                let raw_write = resolve_builtin_target(ctx, "std::io::raw_write", span)?;
                let str_local = ctx.fresh_local(None);
                ctx.locals[str_local.0 as usize].ty = Some(Type::Str);
                let global = ctx.fresh_string_global();
                ctx.push_inst(MirInst::Assign {
                    local: str_local,
                    value: Rvalue::StringLit {
                        content: s.clone(),
                        global_name: global,
                    },
                });
                let out = ctx.fresh_local(None);
                ctx.locals[out.0 as usize].ty = Some(Type::I32);
                ctx.push_inst(MirInst::Assign {
                    local: out,
                    value: Rvalue::Call {
                        name: raw_write,
                        args: vec![
                            MirValue::Int(fd),
                            MirValue::Local(str_local),
                            MirValue::Int(s.len() as i64),
                        ],
                    },
                });
                last = Some(out);
            }
            PrintSegment::Expr(expr, seg_span) => {
                let val = lower_value(ctx, expr)?;
                let val_ty = infer_value_type(&val, ctx);

                // Prepare &mut writer for all formatting calls
                let writer_ref = ctx.fresh_local(None);
                ctx.locals[writer_ref.0 as usize].ty = Some(Type::Ref(
                    Box::new(Type::Named(writer_struct.to_string())),
                    glyph_core::types::Mutability::Mutable,
                ));
                ctx.push_inst(MirInst::Assign {
                    local: writer_ref,
                    value: Rvalue::Ref {
                        base: writer_local,
                        mutability: glyph_core::types::Mutability::Mutable,
                    },
                });

                match val_ty {
                    Some(Type::I32) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_i32", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::U32) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_u32", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::I64) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_i64", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::U64) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_u64", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Bool) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_bool", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Char) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_char", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Str) => {
                        let fmt = resolve_builtin_target(ctx, "std::fmt::fmt_str", seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);
                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    Some(Type::Named(struct_name)) => {
                        // Resolve Format impl
                        // Call free function fmt_<type> if available.
                        let fmt_fn_key = format!("std::fmt::fmt_{}", struct_name);
                        let fmt_fn = resolve_builtin_target(ctx, &fmt_fn_key, seg_span)?;
                        let out = ctx.fresh_local(None);
                        ctx.locals[out.0 as usize].ty = Some(Type::Void);

                        ctx.push_inst(MirInst::Assign {
                            local: out,
                            value: Rvalue::Call {
                                name: fmt_fn,
                                args: vec![val.clone(), MirValue::Local(writer_ref)],
                            },
                        });
                        last = Some(out);
                    }
                    _ => {
                        ctx.error(
                            "Format not implemented for this type; supported: i32, u32, i64, u64, bool, char, str, and types with fmt_<type> in std::fmt",
                            Some(seg_span),
                        );
                        return None;
                    }
                }
            }
        }
    }

    last.map(Rvalue::Move)
}

fn lower_logical<'a>(
    ctx: &mut LowerCtx<'a>,
    op: &glyph_core::ast::BinaryOp,
    lhs: &'a Expr,
    rhs: &'a Expr,
) -> Option<Rvalue> {
    let lhs_val = lower_value(ctx, lhs).unwrap_or(MirValue::Bool(false));
    let lhs_bool = coerce_to_bool(lhs_val.clone());
    let result = ctx.fresh_local(None);
    let rhs_block = ctx.new_block();
    let join_block = ctx.new_block();

    match op {
        glyph_core::ast::BinaryOp::And => {
            ctx.push_inst(MirInst::Assign {
                local: result,
                value: bool_rvalue(lhs_bool.clone()),
            });
            ctx.push_inst(MirInst::If {
                cond: lhs_bool,
                then_bb: rhs_block,
                else_bb: join_block,
            });
            ctx.switch_to(rhs_block);
            let rhs_val = lower_value(ctx, rhs).unwrap_or(MirValue::Bool(false));
            ctx.push_inst(MirInst::Assign {
                local: result,
                value: bool_rvalue(rhs_val),
            });
        }
        glyph_core::ast::BinaryOp::Or => {
            ctx.push_inst(MirInst::Assign {
                local: result,
                value: bool_rvalue(lhs_bool.clone()),
            });
            ctx.push_inst(MirInst::If {
                cond: lhs_bool,
                then_bb: join_block,
                else_bb: rhs_block,
            });
            ctx.switch_to(rhs_block);
            let rhs_val = lower_value(ctx, rhs).unwrap_or(MirValue::Bool(false));
            ctx.push_inst(MirInst::Assign {
                local: result,
                value: bool_rvalue(rhs_val),
            });
        }
        _ => return lower_binary(ctx, op, lhs, rhs),
    }

    ctx.push_inst(MirInst::Goto(join_block));
    ctx.switch_to(join_block);
    Some(Rvalue::Move(result))
}

fn enum_has_generics(enum_name: &str, module: &Module, resolver: &ResolverContext) -> bool {
    if let Some(all_modules) = &resolver.all_modules {
        for (module_id, module) in &all_modules.modules {
            let module_prefix = module_id.replace('/', "::");
            for item in &module.items {
                if let Item::Enum(def) = item {
                    let qualified = format!("{}::{}", module_prefix, def.name.0);
                    if def.name.0 == enum_name || qualified == enum_name {
                        return !def.generic_params.is_empty();
                    }
                }
            }
        }
    }

    for item in &module.items {
        if let Item::Enum(def) = item {
            if def.name.0 == enum_name {
                return !def.generic_params.is_empty();
            }
        }
    }

    false
}

fn lower_call<'a>(
    ctx: &mut LowerCtx<'a>,
    callee: &'a Expr,
    args: &'a [Expr],
    span: Span,
    require_value: bool,
) -> Option<Rvalue> {
    if let Some(builtin) = lower_method_builtin(ctx, callee, args, span) {
        return builtin;
    }

    if let Some(builtin) = lower_static_builtin(ctx, callee, args, span) {
        return Some(builtin);
    }

    let Expr::Ident(name, _) = callee else {
        ctx.error("call target must be an identifier", Some(span));
        return None;
    };

    let Some(sig) = ctx.fn_sigs.get(&name.0) else {
        ctx.error(format!("unknown function '{}'", name.0), Some(span));
        return None;
    };

    if sig.params.len() != args.len() {
        ctx.error(
            format!(
                "function '{}' expects {} arguments but got {}",
                name.0,
                sig.params.len(),
                args.len()
            ),
            Some(span),
        );
        return None;
    }

    // For externs, ensure all parameter types are known
    if sig.params.iter().any(|p| p.is_none()) {
        ctx.error(
            format!("function '{}' has unknown parameter types", name.0),
            Some(span),
        );
        return None;
    }

    // Arity/type check for externs is already covered by sig.params length; here we ensure imports exist.
    if require_value && sig.ret.is_none() {
        ctx.error(
            format!("function '{}' has no return type", name.0),
            Some(span),
        );
        return None;
    }

    let mut lowered_args = Vec::new();
    for arg in args {
        lowered_args.push(lower_value(ctx, arg)?);
    }

    let tmp = ctx.fresh_local(None);
    let mut ret_ty = sig.ret.clone();

    if let Some(enum_ctor) = &sig.enum_ctor {
        if enum_ctor.has_generics && !lowered_args.is_empty() {
            let mut arg_tys = Vec::new();
            for arg in &lowered_args {
                if let Some(arg_ty) = infer_value_type(arg, ctx) {
                    arg_tys.push(arg_ty);
                }
            }
            if !arg_tys.is_empty() {
                ret_ty = Some(Type::App {
                    base: enum_ctor.enum_name.clone(),
                    args: arg_tys,
                });
            }
        }

        if let Some(ret) = ret_ty.as_ref() {
            ctx.locals[tmp.0 as usize].ty = Some(ret.clone());
        }

        let payload = lowered_args.get(0).cloned();
        ctx.push_inst(MirInst::Assign {
            local: tmp,
            value: Rvalue::EnumConstruct {
                enum_name: enum_ctor.enum_name.clone(),
                variant_index: enum_ctor.variant_index as u32,
                payload,
            },
        });
    } else {
        if let Some(ret) = ret_ty.as_ref() {
            ctx.locals[tmp.0 as usize].ty = Some(ret.clone());
        }

        let call_target = sig.target_name.clone();

        ctx.push_inst(MirInst::Assign {
            local: tmp,
            value: Rvalue::Call {
                name: call_target,
                args: lowered_args,
            },
        });
    }

    Some(Rvalue::Move(tmp))
}

/// Infer the type of an expression for method call resolution
fn infer_expr_type(ctx: &LowerCtx, expr: &Expr) -> Option<glyph_core::types::Type> {
    match expr {
        // Local variable: look up in bindings
        Expr::Ident(name, _) => {
            let local = ctx.bindings.get(name.0.as_str()).copied()?;
            ctx.locals.get(local.0 as usize).and_then(|l| l.ty.clone())
        }

        Expr::Lit(glyph_core::ast::Literal::Int(_), _) => Some(Type::I32),
        Expr::Lit(glyph_core::ast::Literal::Bool(_), _) => Some(Type::Bool),
        Expr::Lit(glyph_core::ast::Literal::Char(_), _) => Some(Type::Char),
        Expr::Lit(glyph_core::ast::Literal::Str(_), _) => Some(Type::Str),
        Expr::InterpString { .. } => Some(Type::Str),

        // Struct literal: obvious
        Expr::StructLit { name, .. } => Some(glyph_core::types::Type::Named(name.0.clone())),

        // Field access: get base type, then look up field
        Expr::FieldAccess { base, field, .. } => {
            let base_ty = infer_expr_type(ctx, base)?;
            match base_ty {
                glyph_core::types::Type::Named(struct_name) => {
                    let (field_ty, _) = ctx.resolver.get_field(&struct_name, &field.0)?;
                    Some(field_ty)
                }
                glyph_core::types::Type::Ref(ref inner_ty, _) => {
                    // Dereference and look up field
                    if let glyph_core::types::Type::Named(struct_name) = &**inner_ty {
                        let (field_ty, _) = ctx.resolver.get_field(struct_name, &field.0)?;
                        Some(field_ty)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        // Reference: return reference type
        Expr::Ref {
            expr, mutability, ..
        } => {
            let inner_ty = infer_expr_type(ctx, expr)?;
            Some(glyph_core::types::Type::Ref(
                Box::new(inner_ty),
                *mutability,
            ))
        }

        _ => None,
    }
}

fn lower_method_call<'a>(
    ctx: &mut LowerCtx<'a>,
    receiver: &'a Expr,
    method: &glyph_core::ast::Ident,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    // Handle built-in method-like operations that are not tied to interfaces.
    match method.0.as_str() {
        "len" => {
            if !args.is_empty() {
                ctx.error(".len() does not take arguments", Some(span));
                return None;
            }
            if let Some(rv) = lower_string_len(ctx, receiver, args, span) {
                return Some(rv);
            }
            if let Some(rv) = lower_vec_len(ctx, receiver, span) {
                return Some(rv);
            }
            return lower_array_len(ctx, receiver, span);
        }
        "push" => {
            if let Some(rv) = lower_vec_push(ctx, receiver, args, span) {
                return Some(rv);
            }
        }
        "pop" => {
            if let Some(rv) = lower_vec_pop(ctx, receiver, args, span) {
                return Some(rv);
            }
        }
        "add" => return lower_map_add(ctx, receiver, args, span),
        "update" => return lower_map_update(ctx, receiver, args, span),
        "del" => return lower_map_del(ctx, receiver, args, span),
        "get" => return lower_map_get(ctx, receiver, args, span),
        "has" => return lower_map_has(ctx, receiver, args, span),
        "keys" => return lower_map_keys(ctx, receiver, args, span),
        "vals" => return lower_map_vals(ctx, receiver, args, span),
        "read_to_string" => return lower_file_read_to_string(ctx, receiver, args, span),
        "write_string" => return lower_file_write_string(ctx, receiver, args, span),
        "close" => return lower_file_close(ctx, receiver, args, span),
        "concat" => return lower_string_concat(ctx, receiver, args, span),
        "slice" => return lower_string_slice(ctx, receiver, args, span),
        "trim" => return lower_string_trim(ctx, receiver, args, span),
        "split" => return lower_string_split(ctx, receiver, args, span),
        "starts_with" => return lower_string_starts_with(ctx, receiver, args, span),
        "ends_with" => return lower_string_ends_with(ctx, receiver, args, span),
        "into_raw" => {
            if !args.is_empty() {
                ctx.error("into_raw() does not take arguments", Some(span));
                return None;
            }
            return lower_own_into_raw(ctx, receiver, span);
        }
        "clone" => {
            if !args.is_empty() {
                ctx.error(".clone() does not take arguments", Some(span));
                return None;
            }
            // Check if receiver is Shared<T>
            let base_ty = infer_expr_type(ctx, receiver)?;
            if matches!(base_ty, Type::Shared(_)) {
                return lower_shared_clone(ctx, receiver, span);
            }
            // If not Shared, fall through to regular method dispatch
        }
        _ => {}
    }

    // 1. Infer receiver type
    let receiver_ty = infer_expr_type(ctx, receiver);
    let receiver_ty = match receiver_ty {
        Some(ty) => ty,
        None => {
            ctx.error(
                "cannot infer type of method receiver; consider adding type annotation",
                Some(span),
            );
            return None;
        }
    };

    // 2. Extract struct name from type (handle both by-value and references)
    let struct_name = match &receiver_ty {
        glyph_core::types::Type::Named(name) => name.clone(),
        glyph_core::types::Type::Ref(inner_ty, _) => {
            // Dereference to get the struct name
            if let glyph_core::types::Type::Named(name) = &**inner_ty {
                name.clone()
            } else {
                ctx.error("methods can only be called on struct types", Some(span));
                return None;
            }
        }
        _ => {
            ctx.error("methods can only be called on struct types", Some(span));
            return None;
        }
    };

    // 3. Look up method (inherent first, then interfaces)
    let mut resolved = ctx
        .resolver
        .get_inherent_method(&struct_name, &method.0)
        .map(|info| (info.function_name.clone(), info.self_kind.clone()));

    if resolved.is_none() {
        if let Some(interfaces) = ctx.resolver.interface_impls.get(&struct_name) {
            let mut found: Option<(String, crate::resolver::MethodInfo)> = None;

            for (iface_name, methods) in interfaces {
                if let Some(info) = methods.get(&method.0) {
                    if let Some((existing_iface, _)) = &found {
                        ctx.error(
                            format!(
                                "method '{}' is provided by multiple interfaces on '{}': '{}' and '{}'",
                                method.0, struct_name, existing_iface, iface_name
                            ),
                            Some(span),
                        );
                        return None;
                    }

                    found = Some((iface_name.clone(), info.clone()));
                }
            }

            if let Some((_, info)) = found {
                resolved = Some((info.function_name.clone(), info.self_kind.clone()));
            }
        }
    }

    let Some((mangled_name, self_kind)) = resolved else {
        ctx.error(
            format!("no method '{}' found on type '{}'", method.0, struct_name),
            Some(span),
        );
        return None;
    };

    // 4. Lower receiver value
    let receiver_val = lower_value(ctx, receiver)?;

    // 5. Auto-borrow if method expects reference
    use crate::resolver::SelfKind;
    use glyph_core::types::Mutability;
    let receiver_arg = match self_kind {
        SelfKind::ByValue => {
            // Pass receiver by value (move semantics)
            receiver_val
        }
        SelfKind::Ref => {
            // Auto-borrow as immutable reference
            match receiver_val {
                // Already a reference - use as-is
                MirValue::Local(local)
                    if matches!(
                        ctx.locals.get(local.0 as usize).and_then(|l| l.ty.as_ref()),
                        Some(glyph_core::types::Type::Ref(_, _))
                    ) =>
                {
                    receiver_val
                }
                MirValue::Local(local) => {
                    // Create immutable reference
                    let ref_tmp = ctx.fresh_local(None);
                    ctx.locals[ref_tmp.0 as usize].ty = Some(glyph_core::types::Type::Ref(
                        Box::new(glyph_core::types::Type::Named(struct_name.clone())),
                        Mutability::Immutable,
                    ));
                    ctx.push_inst(MirInst::Assign {
                        local: ref_tmp,
                        value: Rvalue::Ref {
                            base: local,
                            mutability: Mutability::Immutable,
                        },
                    });
                    MirValue::Local(ref_tmp)
                }
                _ => {
                    ctx.error("cannot borrow non-local value", Some(span));
                    return None;
                }
            }
        }
        SelfKind::MutRef => {
            // Auto-borrow as mutable reference
            match receiver_val {
                // Already a mut reference - use as-is
                MirValue::Local(local)
                    if matches!(
                        ctx.locals.get(local.0 as usize).and_then(|l| l.ty.as_ref()),
                        Some(glyph_core::types::Type::Ref(_, Mutability::Mutable))
                    ) =>
                {
                    receiver_val
                }
                MirValue::Local(local) => {
                    // Create mutable reference
                    let ref_tmp = ctx.fresh_local(None);
                    ctx.locals[ref_tmp.0 as usize].ty = Some(glyph_core::types::Type::Ref(
                        Box::new(glyph_core::types::Type::Named(struct_name)),
                        Mutability::Mutable,
                    ));
                    ctx.push_inst(MirInst::Assign {
                        local: ref_tmp,
                        value: Rvalue::Ref {
                            base: local,
                            mutability: Mutability::Mutable,
                        },
                    });
                    MirValue::Local(ref_tmp)
                }
                _ => {
                    ctx.error("cannot borrow non-local value", Some(span));
                    return None;
                }
            }
        }
    };

    // 6. Lower remaining arguments
    let mut all_args = vec![receiver_arg];
    for arg in args {
        let arg_val = lower_value(ctx, arg)?;
        all_args.push(arg_val);
    }

    // 7. Create call with mangled name (reusing call infrastructure)
    let tmp = ctx.fresh_local(None);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::Call {
            name: mangled_name,
            args: all_args,
        },
    });

    Some(Rvalue::Move(tmp))
}

fn lower_method_builtin<'a>(
    ctx: &mut LowerCtx<'a>,
    callee: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Option<Rvalue>> {
    if let Expr::FieldAccess { base, field, .. } = callee {
        match field.0.as_str() {
            "len" => {
                if !args.is_empty() {
                    ctx.error(".len() does not take arguments", Some(span));
                    return Some(None);
                }
                return Some(lower_array_len(ctx, base, span));
            }
            "into_raw" => {
                if !args.is_empty() {
                    ctx.error("into_raw() does not take arguments", Some(span));
                    return Some(None);
                }
                return Some(lower_own_into_raw(ctx, base, span));
            }
            "add" => return Some(lower_map_add(ctx, base, args, span)),
            "update" => return Some(lower_map_update(ctx, base, args, span)),
            "del" => return Some(lower_map_del(ctx, base, args, span)),
            "get" => return Some(lower_map_get(ctx, base, args, span)),
            "has" => return Some(lower_map_has(ctx, base, args, span)),
            "keys" => return Some(lower_map_keys(ctx, base, args, span)),
            "vals" => return Some(lower_map_vals(ctx, base, args, span)),
            _ => {}
        }
    }
    None
}

fn lower_static_builtin<'a>(
    ctx: &mut LowerCtx<'a>,
    callee: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    let Expr::Ident(name, _) = callee else {
        return None;
    };

    match name.0.as_str() {
        "Own::new" => lower_own_new(ctx, args, span),
        "Own::from_raw" => lower_own_from_raw(ctx, args, span),
        "Shared::new" => lower_shared_new(ctx, args, span),
        "Vec::new" => lower_vec_static_new(ctx, args, span),
        "Vec::with_capacity" => lower_vec_static_with_capacity(ctx, args, span),
        "Map::new" => lower_map_static_new(ctx, args, span),
        "Map::with_capacity" => lower_map_static_with_capacity(ctx, args, span),
        "File::open" => lower_file_open(ctx, args, span, false),
        "File::create" => lower_file_open(ctx, args, span, true),
        "String::from_str" => lower_string_from(ctx, args, span),
        "print" | "std::print" => lower_print_builtin(ctx, args, span, false, false),
        "println" | "std::println" => lower_print_builtin(ctx, args, span, true, false),
        "eprint" | "std::eprint" => lower_print_builtin(ctx, args, span, false, true),
        "eprintln" | "std::eprintln" => lower_print_builtin(ctx, args, span, true, true),
        _ => None,
    }
}

fn lower_string_from<'a>(ctx: &mut LowerCtx<'a>, args: &'a [Expr], span: Span) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("String::from expects exactly one argument", Some(span));
        return None;
    }
    let value = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::Call {
            name: "strdup".into(),
            args: vec![value],
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_own_new<'a>(ctx: &mut LowerCtx<'a>, args: &'a [Expr], span: Span) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Own::new expects exactly one argument", Some(span));
        return None;
    }
    let value = lower_value(ctx, &args[0])?;
    let elem_ty = infer_value_type(&value, ctx).or_else(|| {
        ctx.error("could not infer type for Own::new argument", Some(span));
        None
    })?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Own(Box::new(elem_ty.clone())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::OwnNew {
            value,
            elem_type: elem_ty,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_own_from_raw<'a>(ctx: &mut LowerCtx<'a>, args: &'a [Expr], span: Span) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Own::from_raw expects exactly one argument", Some(span));
        return None;
    }
    let ptr = lower_value(ctx, &args[0])?;
    let elem_ty = match infer_value_type(&ptr, ctx) {
        Some(Type::RawPtr(inner)) => inner.as_ref().clone(),
        Some(other) => {
            ctx.error(
                format!("Own::from_raw requires RawPtr but got {:?}", other),
                Some(span),
            );
            return None;
        }
        None => {
            ctx.error("could not infer pointer type for Own::from_raw", Some(span));
            return None;
        }
    };

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Own(Box::new(elem_ty.clone())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::OwnFromRaw {
            ptr,
            elem_type: elem_ty,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_shared_new<'a>(ctx: &mut LowerCtx<'a>, args: &'a [Expr], span: Span) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Shared::new expects exactly one argument", Some(span));
        return None;
    }
    let value = lower_value(ctx, &args[0])?;
    let elem_ty = infer_value_type(&value, ctx).or_else(|| {
        ctx.error("could not infer type for Shared::new argument", Some(span));
        None
    })?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Shared(Box::new(elem_ty.clone())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::SharedNew {
            value,
            elem_type: elem_ty,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_shared_clone<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, span: Span) -> Option<Rvalue> {
    let base_val = lower_value(ctx, base)?;
    let base_local = match base_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error(".clone() requires a local variable", Some(span));
            return None;
        }
    };
    let elem_ty = ctx
        .locals
        .get(base_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(|ty| ty.shared_inner_type())
        .cloned()
        .or_else(|| {
            ctx.error("could not infer inner type for .clone()", Some(span));
            None
        })?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Shared(Box::new(elem_ty.clone())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::SharedClone {
            base: base_local,
            elem_type: elem_ty,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_own_into_raw<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, span: Span) -> Option<Rvalue> {
    let base_val = lower_value(ctx, base)?;
    let base_local = match base_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("into_raw() target must be a local variable", Some(span));
            return None;
        }
    };

    let elem_ty = match ctx.local_ty(base_local) {
        Some(Type::Own(inner)) => inner.as_ref().clone(),
        Some(other) => {
            ctx.error(
                format!(
                    "into_raw() is only supported on Own values, got {:?}",
                    other
                ),
                Some(span),
            );
            return None;
        }
        None => {
            ctx.error("into_raw() target has unknown type", Some(span));
            return None;
        }
    };

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::RawPtr(Box::new(elem_ty.clone())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::OwnIntoRaw {
            base: base_local,
            elem_type: elem_ty,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_struct_lit<'a>(
    ctx: &mut LowerCtx<'a>,
    name: &'a glyph_core::ast::Ident,
    fields: &'a [(glyph_core::ast::Ident, Expr)],
    span: Span,
) -> Option<Rvalue> {
    let struct_name = name.0.clone();
    let Some(struct_type) = ctx.resolver.get_struct(&struct_name) else {
        ctx.error(format!("unknown struct '{}'", struct_name), Some(span));
        return None;
    };

    let mut ok = true;

    let mut lowered_fields = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for (field_name, expr) in fields {
        if !seen.insert(field_name.0.as_str()) {
            ctx.error(
                format!("duplicate field '{}' in struct literal", field_name.0),
                Some(span),
            );
            ok = false;
            continue;
        }

        if !struct_type.fields.iter().any(|(n, _)| n == &field_name.0) {
            ctx.error(
                format!(
                    "struct '{}' has no field named '{}'",
                    struct_name, field_name.0
                ),
                Some(span),
            );
            ok = false;
            continue;
        }

        match lower_value(ctx, expr) {
            Some(value) => lowered_fields.push((field_name.0.clone(), value)),
            None => ok = false,
        }
    }

    for (required, _) in &struct_type.fields {
        if !lowered_fields.iter().any(|(n, _)| n == required) {
            ctx.error(
                format!(
                    "missing field '{}' in struct literal '{}'",
                    required, struct_name
                ),
                Some(span),
            );
            ok = false;
        }
    }

    if !ok {
        return None;
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Named(struct_name.clone()));

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StructLit {
            struct_name,
            field_values: lowered_fields,
        },
    });

    Some(Rvalue::Move(tmp))
}

fn lower_tuple_expr<'a>(
    ctx: &mut LowerCtx<'a>,
    elements: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    // Empty tuple is unit type
    if elements.is_empty() {
        return Some(Rvalue::ConstInt(0));
    }

    let mut elem_types = Vec::new();
    let mut elem_values = Vec::new();

    // Lower each element and infer its type
    for elem_expr in elements {
        let val = lower_value(ctx, elem_expr)?;

        // Infer type from the element expression
        let ty = match &val {
            MirValue::Int(_) => Type::I32,
            MirValue::Bool(_) => Type::Bool,
            MirValue::Local(local_id) => {
                ctx.locals.get(local_id.0 as usize)
                    .and_then(|local| local.ty.clone())
                    .ok_or_else(|| {
                        ctx.error("cannot infer type of tuple element", Some(span));
                    })
                    .ok()?
            }
            MirValue::Unit => Type::Void,
        };

        elem_types.push(ty);
        elem_values.push(val);
    }

    // Generate tuple struct name using monomorphize-style type_key
    let struct_name = tuple_struct_name(&elem_types);

    // Create field values with numbered fields (0, 1, 2, ...)
    let field_values: Vec<(String, MirValue)> = elem_values
        .into_iter()
        .enumerate()
        .map(|(idx, val)| (idx.to_string(), val))
        .collect();

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Tuple(elem_types));

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StructLit {
            struct_name,
            field_values,
        },
    });

    Some(Rvalue::Move(tmp))
}

fn tuple_struct_name(elem_types: &[Type]) -> String {
    if elem_types.is_empty() {
        return "unit".to_string();
    }

    let type_names: Vec<String> = elem_types.iter().map(type_key_simple).collect();
    format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
}

fn type_key_simple(ty: &Type) -> String {
    match ty {
        Type::I8 => "i8".into(),
        Type::I32 => "i32".into(),
        Type::I64 => "i64".into(),
        Type::U8 => "u8".into(),
        Type::U32 => "u32".into(),
        Type::U64 => "u64".into(),
        Type::Usize => "usize".into(),
        Type::F32 => "f32".into(),
        Type::F64 => "f64".into(),
        Type::Bool => "bool".into(),
        Type::Char => "char".into(),
        Type::Str => "str".into(),
        Type::String => "String".into(),
        Type::Void => "void".into(),
        Type::Named(n) => n.replace("::", "_"),
        Type::Enum(n) => format!("enum_{}", n.replace("::", "_")),
        Type::Param(p) => format!("P_{}", p),
        Type::Ref(inner, _) => format!("ref_{}", type_key_simple(inner)),
        Type::Array(inner, size) => format!("arr{}_{}", size, type_key_simple(inner)),
        Type::Own(inner) => format!("own_{}", type_key_simple(inner)),
        Type::RawPtr(inner) => format!("rawptr_{}", type_key_simple(inner)),
        Type::Shared(inner) => format!("shared_{}", type_key_simple(inner)),
        Type::App { base, args } => {
            let args: Vec<String> = args.iter().map(type_key_simple).collect();
            format!("app_{}_{}", base.replace("::", "_"), args.join("__"))
        }
        Type::Tuple(elem_types) => {
            if elem_types.is_empty() {
                "unit".into()
            } else {
                let type_names: Vec<String> = elem_types.iter().map(type_key_simple).collect();
                format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
            }
        }
    }
}

fn lower_array_lit<'a>(ctx: &mut LowerCtx<'a>, elements: &'a [Expr], span: Span) -> Option<Rvalue> {
    // Check for empty array literal
    if elements.is_empty() {
        ctx.error("empty array literals are not supported", Some(span));
        return None;
    }

    // Lower all elements
    let mut mir_elements = Vec::new();
    for elem in elements {
        let mir_val = lower_value(ctx, elem)?;
        mir_elements.push(mir_val);
    }

    // Infer element type from first element
    let elem_type = infer_value_type(&mir_elements[0], ctx)?;

    // Validate all elements have same type
    for (i, val) in mir_elements.iter().enumerate().skip(1) {
        let val_ty = infer_value_type(val, ctx)?;
        if val_ty != elem_type {
            ctx.error(
                format!(
                    "array element {} has type {:?}, expected {:?}",
                    i, val_ty, elem_type
                ),
                Some(span),
            );
            return None;
        }
    }

    let array_size = mir_elements.len();
    let array_type = Type::Array(Box::new(elem_type.clone()), array_size);

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(array_type);

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::ArrayLit {
            elem_type,
            elements: mir_elements,
        },
    });

    Some(Rvalue::Move(tmp))
}

fn lower_array_index<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    index: &'a Expr,
    span: Span,
) -> Option<Rvalue> {
    // Lower base to local
    let base_val = lower_value(ctx, base)?;
    let base_local = match base_val {
        MirValue::Local(local_id) => local_id,
        _ => {
            // If base is not a local, create one
            let tmp = ctx.fresh_local(None);
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: match base_val {
                    MirValue::Int(i) => Rvalue::ConstInt(i),
                    MirValue::Bool(b) => Rvalue::ConstBool(b),
                    MirValue::Local(_) => unreachable!(),
                    MirValue::Unit => unreachable!("unit value cannot be indexed"),
                },
            });
            tmp
        }
    };

    // Get array type from base local
    let base_type = ctx
        .locals
        .get(base_local.0 as usize)
        .and_then(|local| local.ty.as_ref())?;

    let (elem_type, is_vec) = match base_type {
        Type::Array(elem_ty, _) => (elem_ty.as_ref().clone(), false),
        ty if vec_elem_type_from_type(ty).is_some() => (vec_elem_type_from_type(ty).unwrap(), true),
        _ => {
            ctx.error(
                format!("cannot index into non-array type {:?}", base_type),
                Some(span),
            );
            return None;
        }
    };

    // Lower index expression
    let index_val = lower_value(ctx, index)?;

    // Create temp with element type
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(elem_type.clone());

    let value = if is_vec {
        Rvalue::VecIndex {
            vec: base_local,
            elem_type,
            index: index_val,
            bounds_check: true,
        }
    } else {
        Rvalue::ArrayIndex {
            base: base_local,
            index: index_val,
            bounds_check: true, // Always check bounds
        }
    };

    ctx.push_inst(MirInst::Assign { local: tmp, value });

    Some(Rvalue::Move(tmp))
}

fn lower_array_len<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, span: Span) -> Option<Rvalue> {
    let base_local = match base {
        Expr::Ident(ident, _) => ctx.bindings.get(ident.0.as_str()).copied(),
        _ => None,
    };

    let base_local = match base_local {
        Some(local) => local,
        None => {
            ctx.error(".len() target must be a local array", Some(span));
            return None;
        }
    };

    let Some(base_ty) = ctx
        .locals
        .get(base_local.0 as usize)
        .and_then(|local| local.ty.as_ref())
    else {
        ctx.error("array length target has unknown type", Some(span));
        return None;
    };

    if !matches!(base_ty, Type::Array(_, _)) {
        ctx.error(".len() is only supported on arrays", Some(span));
        return None;
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::I32);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::ArrayLen { base: base_local },
    });
    Some(Rvalue::Move(tmp))
}

fn vec_elem_type_from_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::App { base, args } if base == "Vec" => args.get(0).cloned(),
        Type::Ref(inner, _) | Type::Own(inner) | Type::RawPtr(inner) | Type::Shared(inner) => {
            vec_elem_type_from_type(inner)
        }
        _ => None,
    }
}

fn lower_vec_len<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, _span: Span) -> Option<Rvalue> {
    let base_local = match base {
        Expr::Ident(ident, _) => ctx.bindings.get(ident.0.as_str()).copied(),
        _ => None,
    }?;

    let Some(base_ty) = ctx
        .locals
        .get(base_local.0 as usize)
        .and_then(|local| local.ty.as_ref())
    else {
        return None;
    };

    if vec_elem_type_from_type(base_ty).is_none() {
        return None;
    }

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Usize);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecLen { vec: base_local },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_vec_push<'a>(
    ctx: &mut LowerCtx<'a>,
    receiver: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Vec::push expects exactly one argument", Some(span));
        return None;
    }

    let receiver_val = lower_value(ctx, receiver)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("Vec::push receiver must be a local", Some(span));
            return None;
        }
    };

    let elem_type = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(vec_elem_type_from_type)
        .unwrap_or_else(|| {
            ctx.error("could not infer Vec element type", Some(span));
            Type::I32
        });

    let value = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecPush {
            vec: receiver_local,
            elem_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_vec_pop<'a>(
    ctx: &mut LowerCtx<'a>,
    receiver: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Vec::pop does not take arguments", Some(span));
        return None;
    }

    let receiver_val = lower_value(ctx, receiver)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("Vec::pop receiver must be a local", Some(span));
            return None;
        }
    };

    let elem_type = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(vec_elem_type_from_type)
        .unwrap_or_else(|| {
            ctx.error("could not infer Vec element type", Some(span));
            Type::I32
        });

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Option".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecPop {
            vec: receiver_local,
            elem_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_vec_static_new<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Vec::new does not take arguments", Some(span));
        return None;
    }

    // TODO: wire contextual element-type inference; default to i32 for now
    let elem_type = Type::I32;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecNew { elem_type },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_vec_static_with_capacity<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Vec::with_capacity expects one argument", Some(span));
        return None;
    }

    let capacity = lower_value(ctx, &args[0])?;
    // TODO: wire contextual element-type inference; default to i32 for now
    let elem_type = Type::I32;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![elem_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::VecWithCapacity {
            elem_type,
            capacity,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn file_result_type(ok_type: Type) -> Type {
    Type::App {
        base: "Result".into(),
        args: vec![ok_type, Type::Named("Err".into())],
    }
}

fn file_receiver_local<'a>(ctx: &mut LowerCtx<'a>, base: &'a Expr, span: Span) -> Option<LocalId> {
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("file receiver must be a local", Some(span));
            return None;
        }
    };
    let receiver_ty = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref());
    let is_file = matches!(receiver_ty, Some(Type::Named(name)) if name == "File")
        || matches!(receiver_ty, Some(Type::Ref(inner, _)) if matches!(inner.as_ref(), Type::Named(name) if name == "File"));
    if !is_file {
        ctx.error("file methods require a File receiver", Some(span));
        return None;
    }
    Some(receiver_local)
}

fn string_receiver_local<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    span: Span,
) -> Option<LocalId> {
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("string receiver must be a local", Some(span));
            return None;
        }
    };
    let receiver_ty = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref());
    let is_string = matches!(receiver_ty, Some(Type::String))
        || matches!(receiver_ty, Some(Type::Str))
        || matches!(receiver_ty, Some(Type::Ref(inner, _)) if matches!(inner.as_ref(), Type::String | Type::Str));
    if !is_string {
        ctx.error("string methods require a String receiver", Some(span));
        return None;
    }
    Some(receiver_local)
}

fn lower_string_len<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("len() does not take arguments", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Usize);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringLen {
            base: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_concat<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("concat() expects one argument", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let value = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringConcat {
            base: receiver_local,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_slice<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("slice() expects (start, len)", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let start = lower_value(ctx, &args[0])?;
    let len = lower_value(ctx, &args[1])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringSlice {
            base: receiver_local,
            start,
            len,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_trim<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("trim() does not take arguments", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::String);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringTrim {
            base: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_split<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("split() expects a separator", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let sep = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![Type::String],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringSplit {
            base: receiver_local,
            sep,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_starts_with<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("starts_with() expects a prefix", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let needle = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringStartsWith {
            base: receiver_local,
            needle,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_string_ends_with<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("ends_with() expects a suffix", Some(span));
        return None;
    }
    let receiver_local = string_receiver_local(ctx, base, span)?;
    let needle = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::StringEndsWith {
            base: receiver_local,
            needle,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_file_open<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
    create: bool,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("File::open expects a path", Some(span));
        return None;
    }
    let path = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::Named("File".into())));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileOpen { path, create },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_file_read_to_string<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("read_to_string() takes no arguments", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::String));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileReadToString {
            file: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_file_write_string<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("write_string() expects one argument", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let contents = lower_value(ctx, &args[0])?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::U32));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileWriteString {
            file: receiver_local,
            contents,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_file_close<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("close() takes no arguments", Some(span));
        return None;
    }
    let receiver_local = file_receiver_local(ctx, base, span)?;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(file_result_type(Type::I32));
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FileClose {
            file: receiver_local,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn map_key_value_types(ty: &Type) -> Option<(Type, Type)> {
    match ty {
        Type::App { base, args } if base == "Map" => {
            let key = args.get(0)?.clone();
            let val = args.get(1)?.clone();
            Some((key, val))
        }
        Type::Ref(inner, _) | Type::Own(inner) | Type::RawPtr(inner) | Type::Shared(inner) => {
            map_key_value_types(inner)
        }
        _ => None,
    }
}

fn lower_map_static_new<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("Map::new does not take arguments", Some(span));
        return None;
    }
    let key_type = Type::I32;
    let value_type = Type::I32;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Map".into(),
        args: vec![key_type.clone(), value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapNew {
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_static_with_capacity<'a>(
    ctx: &mut LowerCtx<'a>,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("Map::with_capacity expects one argument", Some(span));
        return None;
    }
    let capacity = lower_value(ctx, &args[0])?;
    let key_type = Type::I32;
    let value_type = Type::I32;
    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Map".into(),
        args: vec![key_type.clone(), value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapWithCapacity {
            key_type,
            value_type,
            capacity,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_add<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("add expects (key, value)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("add receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;
    let value = lower_value(ctx, &args[1])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![Type::Void, Type::Named("Err".into())],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapAdd {
            map: receiver_local,
            key_type,
            key,
            value_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_update<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 2 {
        ctx.error("update expects (key, value)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("update receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;
    let value = lower_value(ctx, &args[1])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![Type::Void, Type::Named("Err".into())],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapUpdate {
            map: receiver_local,
            key_type,
            key,
            value_type,
            value,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_del<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("del expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("del receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Result".into(),
        args: vec![value_type.clone(), Type::Named("Err".into())],
    });

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapDel {
            map: receiver_local,
            key_type,
            value_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_get<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("get expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("get receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Option".into(),
        args: vec![value_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapGet {
            map: receiver_local,
            key_type,
            value_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_has<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if args.len() != 1 {
        ctx.error("has expects (key)", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("has receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = value_type; // unused but ensures inference

    let key = lower_value(ctx, &args[0])?;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::Bool);
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapHas {
            map: receiver_local,
            key_type,
            key,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_keys<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("keys does not take arguments", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("keys receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = value_type;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![key_type.clone()],
    });
    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapKeys {
            map: receiver_local,
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn lower_map_vals<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    args: &'a [Expr],
    span: Span,
) -> Option<Rvalue> {
    if !args.is_empty() {
        ctx.error("vals does not take arguments", Some(span));
        return None;
    }
    let receiver_val = lower_value(ctx, base)?;
    let receiver_local = match receiver_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("vals receiver must be a local", Some(span));
            return None;
        }
    };
    let (key_type, value_type) = ctx
        .locals
        .get(receiver_local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(map_key_value_types)
        .unwrap_or((Type::I32, Type::I32));
    let _ = key_type;

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(Type::App {
        base: "Vec".into(),
        args: vec![value_type.clone()],
    });

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::MapVals {
            map: receiver_local,
            key_type,
            value_type,
        },
    });
    Some(Rvalue::Move(tmp))
}

fn infer_value_type(value: &MirValue, ctx: &LowerCtx) -> Option<Type> {
    match value {
        MirValue::Int(_) => Some(Type::I32),
        MirValue::Bool(_) => Some(Type::Bool),
        MirValue::Unit => Some(Type::Void),
        MirValue::Local(local_id) => ctx
            .locals
            .get(local_id.0 as usize)
            .and_then(|local| local.ty.clone()),
    }
}

fn lower_field_access<'a>(
    ctx: &mut LowerCtx<'a>,
    base: &'a Expr,
    field: &'a glyph_core::ast::Ident,
    span: Span,
) -> Option<Rvalue> {
    let base_val = lower_value(ctx, base)?;
    let base_local = match base_val {
        MirValue::Local(id) => id,
        _ => {
            ctx.error("field access base must be a local", Some(span));
            return None;
        }
    };

    // Check if this is a tuple type
    let base_type = ctx.locals.get(base_local.0 as usize).and_then(|l| l.ty.as_ref());

    let (field_type, field_index, struct_name) = if let Some(Type::Tuple(elem_types)) = base_type {
        // Handle tuple field access
        if let Ok(idx) = field.0.parse::<usize>() {
            if idx < elem_types.len() {
                (elem_types[idx].clone(), idx, tuple_struct_name(elem_types))
            } else {
                ctx.error(
                    format!("tuple index {} out of bounds (len is {})", idx, elem_types.len()),
                    Some(span),
                );
                return None;
            }
        } else {
            ctx.error(
                format!("tuple field access must use numeric index, got '{}'", field.0),
                Some(span),
            );
            return None;
        }
    } else {
        // Handle regular struct field access
        let Some(struct_name) = local_struct_name(ctx, base_local) else {
            ctx.error("field access base is not a struct", Some(span));
            return None;
        };

        let Some((field_type, field_index)) = ctx.resolver.get_field(&struct_name, &field.0) else {
            ctx.error(
                format!("struct '{}' has no field named '{}'", struct_name, field.0),
                Some(span),
            );
            return None;
        };

        (field_type, field_index, struct_name)
    };

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(field_type);

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::FieldAccess {
            base: base_local,
            field_name: field.0.clone(),
            field_index: field_index as u32,
        },
    });

    Some(Rvalue::Move(tmp))
}

fn resolve_type_name(name: &str, resolver: &ResolverContext) -> Option<Type> {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return None;
    }

    if trimmed == "&str" {
        return Some(Type::Str);
    }

    if let Some(primitive) = Type::from_name(trimmed) {
        return Some(primitive);
    }

    if let Some(array) = parse_array_type(trimmed, resolver) {
        return Some(array);
    }

    if let Some(own_ty) = parse_own_type(trimmed, resolver) {
        return Some(own_ty);
    }

    if let Some(raw_ptr_ty) = parse_raw_ptr_type(trimmed, resolver) {
        return Some(raw_ptr_ty);
    }

    if let Some(shared_ty) = parse_shared_type(trimmed, resolver) {
        return Some(shared_ty);
    }

    if let Some(app_ty) = parse_type_application(trimmed, resolver) {
        return Some(app_ty);
    }

    if resolver.get_enum(trimmed).is_some() {
        return Some(Type::Enum(trimmed.to_string()));
    }

    if resolver.get_struct(trimmed).is_some() {
        return Some(Type::Named(trimmed.to_string()));
    }

    if let Some(reference) = parse_reference_type(trimmed, resolver) {
        return Some(reference);
    }

    // Try to resolve through imports or qualified names
    if let Some(resolved) = resolver.resolve_symbol(trimmed) {
        match resolved {
            crate::resolver::ResolvedSymbol::Struct(module_id, struct_name) => {
                if resolver.current_module.as_ref() == Some(&module_id) {
                    return Some(Type::Named(struct_name));
                } else {
                    return Some(Type::Named(format!("{}::{}", module_id, struct_name)));
                }
            }
            crate::resolver::ResolvedSymbol::Enum(_, enum_name) => {
                return Some(Type::Enum(enum_name));
            }
            _ => {}
        }
    }

    resolver
        .struct_types
        .contains_key(trimmed)
        .then(|| Type::Named(trimmed.to_string()))
}

fn parse_array_type(name: &str, resolver: &ResolverContext) -> Option<Type> {
    // Parse "[T; N]" format
    if !name.starts_with('[') || !name.ends_with(']') {
        return None;
    }

    // Remove brackets
    let inner = &name[1..name.len() - 1];

    // Find the semicolon at the correct bracket nesting level
    // We need to find the last semicolon that's at depth 0
    let mut depth = 0;
    let mut semicolon_pos = None;
    for (i, ch) in inner.chars().enumerate() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            ';' if depth == 0 => semicolon_pos = Some(i),
            _ => {}
        }
    }

    let semicolon_pos = semicolon_pos?;
    let elem_type_str = inner[..semicolon_pos].trim();
    let size_str = inner[semicolon_pos + 1..].trim();

    // Parse size
    let size: usize = size_str.parse().ok()?;
    if size == 0 {
        return None; // Array size must be > 0
    }

    // Recursively resolve element type
    let elem_type = resolve_type_name(elem_type_str, resolver)?;

    Some(Type::Array(Box::new(elem_type), size))
}

fn parse_own_type(name: &str, resolver: &ResolverContext) -> Option<Type> {
    parse_single_arg_type(name, "Own", resolver).map(|inner| Type::Own(Box::new(inner)))
}

fn parse_raw_ptr_type(name: &str, resolver: &ResolverContext) -> Option<Type> {
    parse_single_arg_type(name, "RawPtr", resolver).map(|inner| Type::RawPtr(Box::new(inner)))
}

fn parse_shared_type(name: &str, resolver: &ResolverContext) -> Option<Type> {
    parse_single_arg_type(name, "Shared", resolver).map(|inner| Type::Shared(Box::new(inner)))
}

fn parse_type_application(name: &str, resolver: &ResolverContext) -> Option<Type> {
    let trimmed = name.trim();
    let lt = trimmed.find('<')?;
    if !trimmed.ends_with('>') {
        return None;
    }

    // Avoid treating "&mut" prefixes as type applications.
    if trimmed.starts_with('&') {
        return None;
    }

    let base_str = trimmed[..lt].trim();
    if base_str.is_empty() {
        return None;
    }

    // Extract the interior between matching <...> at depth 0.
    let mut depth = 0;
    let mut end_idx = None;
    for (idx, ch) in trimmed[lt + 1..].char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                if depth == 0 {
                    end_idx = Some(lt + 1 + idx);
                    break;
                }
                depth -= 1;
            }
            _ => {}
        }
    }
    let end = end_idx?;
    if end + 1 != trimmed.len() {
        return None;
    }

    let inner = &trimmed[lt + 1..end];

    // Split args at commas at depth 0.
    let mut args = Vec::new();
    let mut start = 0;
    let mut depth = 0;
    for (i, ch) in inner.char_indices() {
        match ch {
            '<' | '[' => depth += 1,
            '>' | ']' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => {
                let part = inner[start..i].trim();
                if part.is_empty() {
                    return None;
                }
                args.push(resolve_type_name(part, resolver)?);
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = inner[start..].trim();
    if last.is_empty() {
        return None;
    }
    args.push(resolve_type_name(last, resolver)?);

    // Recognize built-in pointer wrappers.
    match base_str {
        "Own" if args.len() == 1 => return Some(Type::Own(Box::new(args.remove(0)))),
        "RawPtr" if args.len() == 1 => return Some(Type::RawPtr(Box::new(args.remove(0)))),
        "Shared" if args.len() == 1 => return Some(Type::Shared(Box::new(args.remove(0)))),
        _ => {}
    }

    // Normalize base using existing symbol resolution.
    let mut base_name = base_str.to_string();
    if resolver.get_enum(base_str).is_some() {
        base_name = base_str.to_string();
    } else if resolver.get_struct(base_str).is_some() {
        base_name = base_str.to_string();
    } else if let Some(resolved) = resolver.resolve_symbol(base_str) {
        match resolved {
            crate::resolver::ResolvedSymbol::Struct(module_id, struct_name) => {
                if resolver.current_module.as_ref() == Some(&module_id) {
                    base_name = struct_name;
                } else {
                    base_name = format!("{}::{}", module_id, struct_name);
                }
            }
            crate::resolver::ResolvedSymbol::Enum(_module_id, enum_name) => {
                base_name = enum_name;
            }
            _ => {}
        }
    }

    Some(Type::App {
        base: base_name,
        args,
    })
}

fn parse_single_arg_type(name: &str, keyword: &str, resolver: &ResolverContext) -> Option<Type> {
    let trimmed = name.trim();
    let rest = trimmed.strip_prefix(keyword)?;
    let rest = rest.trim_start();
    if !rest.starts_with('<') {
        return None;
    }

    // Drop leading '<'
    let inner = &rest[1..];
    let mut depth = 0;
    let mut end = None;
    for (idx, ch) in inner.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                if depth == 0 {
                    end = Some(idx);
                    break;
                } else {
                    depth -= 1;
                }
            }
            _ => {}
        }
    }

    let end_idx = end?;
    let inner_str = inner[..end_idx].trim();
    let tail = inner[end_idx + 1..].trim();
    if !tail.is_empty() {
        return None;
    }

    resolve_type_name(inner_str, resolver)
}

fn parse_reference_type(name: &str, resolver: &ResolverContext) -> Option<Type> {
    if !name.starts_with('&') {
        return None;
    }

    let mut rest = name;
    let mut mutabilities = Vec::new();

    while let Some(after_amp) = rest.strip_prefix('&') {
        rest = after_amp.trim_start();
        if rest.is_empty() {
            return None;
        }

        let (mutability, after_mut) = take_mutability_prefix(rest);
        mutabilities.push(mutability);
        rest = after_mut.trim_start();

        if rest.is_empty() {
            return None;
        }

        if !rest.starts_with('&') {
            break;
        }
    }

    let base_name = rest.trim();
    if base_name.is_empty() {
        return None;
    }

    if base_name == "str" {
        if mutabilities.len() == 1 && matches!(mutabilities[0], Mutability::Immutable) {
            return Some(Type::Str);
        }
        return None;
    }

    let mut ty = resolve_type_name(base_name, resolver)?;
    for mutability in mutabilities.into_iter().rev() {
        ty = Type::Ref(Box::new(ty), mutability);
    }
    Some(ty)
}

fn take_mutability_prefix(input: &str) -> (Mutability, &str) {
    if let Some(after) = input.strip_prefix("mut") {
        if is_keyword_boundary(after.chars().next()) {
            return (Mutability::Mutable, after.trim_start());
        }
    }
    (Mutability::Immutable, input)
}

fn is_keyword_boundary(next: Option<char>) -> bool {
    match next {
        None => true,
        Some(c) => !c.is_alphanumeric() && c != '_',
    }
}

fn lower_assignment_target<'a>(ctx: &mut LowerCtx<'a>, target: &'a Expr) -> Option<LocalId> {
    match target {
        Expr::Ident(ident, span) => {
            let local_id = ctx.bindings.get(ident.0.as_str()).copied().or_else(|| {
                ctx.error(format!("unknown identifier '{}'", ident.0), Some(*span));
                None
            })?;

            // Check mutability
            if let Some(local) = ctx.locals.get(local_id.0 as usize) {
                if !local.mutable {
                    ctx.error(
                        format!("cannot assign to immutable variable '{}'", ident.0),
                        Some(*span),
                    );
                    return None;
                }
            }

            Some(local_id)
        }
        Expr::FieldAccess { span, .. } => {
            ctx.error("assignment to fields is not supported yet", Some(*span));
            None
        }
        _ => {
            ctx.error("assignment target must be an identifier", expr_span(target));
            None
        }
    }
}

fn lower_ref_expr<'a>(
    ctx: &mut LowerCtx<'a>,
    expr: &'a Expr,
    mutability: Mutability,
    span: Span,
) -> Option<Rvalue> {
    match expr {
        Expr::Ident(ident, _) => {
            let Some(local) = ctx.bindings.get(ident.0.as_str()).copied() else {
                ctx.error(format!("unknown identifier '{}'", ident.0), Some(span));
                return None;
            };
            Some(Rvalue::Ref {
                base: local,
                mutability,
            })
        }
        _ => {
            ctx.error("references can only be taken to locals", Some(span));
            None
        }
    }
}

fn update_local_type_from_rvalue(ctx: &mut LowerCtx, local: LocalId, rv: &Rvalue) {
    if let Some(inferred) = infer_rvalue_type(rv, ctx) {
        ctx.locals[local.0 as usize].ty = Some(inferred);
    }
}

fn infer_rvalue_type(rv: &Rvalue, ctx: &LowerCtx) -> Option<Type> {
    match rv {
        Rvalue::StructLit { struct_name, .. } => Some(Type::Named(struct_name.clone())),
        Rvalue::Move(local) => ctx
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.as_ref().cloned()),
        Rvalue::StringLit { .. } => Some(Type::Str),
        Rvalue::Ref { base, mutability } => ctx
            .locals
            .get(base.0 as usize)
            .and_then(|l| l.ty.as_ref().cloned())
            .map(|inner| Type::Ref(Box::new(inner), *mutability)),
        Rvalue::ArrayLit {
            elem_type,
            elements,
        } => Some(Type::Array(Box::new(elem_type.clone()), elements.len())),
        Rvalue::ArrayIndex { base, .. } => ctx
            .locals
            .get(base.0 as usize)
            .and_then(|l| l.ty.as_ref())
            .and_then(|ty| match ty {
                Type::Array(elem_ty, _) => Some(elem_ty.as_ref().clone()),
                _ => None,
            }),
        Rvalue::ArrayLen { .. } => Some(Type::I32),
        Rvalue::OwnNew { elem_type, .. } => Some(Type::Own(Box::new(elem_type.clone()))),
        Rvalue::OwnIntoRaw { elem_type, .. } => Some(Type::RawPtr(Box::new(elem_type.clone()))),
        Rvalue::OwnFromRaw { elem_type, .. } => Some(Type::Own(Box::new(elem_type.clone()))),
        Rvalue::RawPtrNull { elem_type } => Some(Type::RawPtr(Box::new(elem_type.clone()))),
        Rvalue::SharedNew { elem_type, .. } => Some(Type::Shared(Box::new(elem_type.clone()))),
        Rvalue::SharedClone { elem_type, .. } => Some(Type::Shared(Box::new(elem_type.clone()))),
        Rvalue::EnumConstruct { enum_name, .. } => Some(Type::Enum(enum_name.clone())),
        Rvalue::EnumTag { .. } => Some(Type::I32),
        Rvalue::EnumPayload { payload_type, .. } => Some(payload_type.clone()),
        Rvalue::VecNew { elem_type }
        | Rvalue::VecWithCapacity { elem_type, .. }
        | Rvalue::VecPush { elem_type, .. } => Some(Type::App {
            base: "Vec".into(),
            args: vec![elem_type.clone()],
        }),
        Rvalue::VecPop { elem_type, .. } => Some(Type::App {
            base: "Option".into(),
            args: vec![elem_type.clone()],
        }),
        Rvalue::VecIndex { elem_type, .. } => Some(elem_type.clone()),
        Rvalue::VecLen { .. } => Some(Type::Usize),
        _ => None,
    }
}

fn local_struct_name(ctx: &LowerCtx, local: LocalId) -> Option<String> {
    ctx.locals
        .get(local.0 as usize)
        .and_then(|l| l.ty.as_ref())
        .and_then(struct_name_from_type)
}

fn struct_name_from_type(ty: &Type) -> Option<String> {
    match ty {
        Type::Named(name) => Some(name.clone()),
        Type::Ref(inner, _) => struct_name_from_type(inner),
        Type::Own(inner) => struct_name_from_type(inner),
        Type::RawPtr(inner) => struct_name_from_type(inner),
        Type::Shared(inner) => struct_name_from_type(inner),
        Type::Tuple(elem_types) => Some(tuple_struct_name(elem_types)),
        _ => None,
    }
}

fn expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Lit(_, span) => Some(*span),
        Expr::Ident(_, span) => Some(*span),
        Expr::Binary { span, .. } => Some(*span),
        Expr::Call { span, .. } => Some(*span),
        Expr::If { span, .. } => Some(*span),
        Expr::While { span, .. } => Some(*span),
        Expr::For { span, .. } => Some(*span),
        Expr::Block(block) => Some(block.span),
        Expr::StructLit { span, .. } => Some(*span),
        Expr::FieldAccess { span, .. } => Some(*span),
        Expr::Ref { span, .. } => Some(*span),
        Expr::ArrayLit { span, .. } => Some(*span),
        Expr::Index { span, .. } => Some(*span),
        Expr::MethodCall { span, .. } => Some(*span),
        Expr::Match { span, .. } => Some(*span),
        Expr::InterpString { span, .. } => Some(*span),
        Expr::Tuple { span, .. } => Some(*span),
    }
}

fn bool_rvalue(value: MirValue) -> Rvalue {
    match value {
        MirValue::Bool(b) => Rvalue::ConstBool(b),
        MirValue::Local(id) => Rvalue::Move(id),
        MirValue::Int(i) => Rvalue::ConstBool(i != 0),
        _ => Rvalue::ConstBool(false),
    }
}

fn coerce_to_bool(value: MirValue) -> MirValue {
    match value {
        MirValue::Bool(_) | MirValue::Local(_) => value,
        MirValue::Int(i) => MirValue::Bool(i != 0),
        _ => MirValue::Bool(false),
    }
}

fn rvalue_to_value(rv: Rvalue) -> Option<MirValue> {
    match rv {
        Rvalue::ConstInt(v) => Some(MirValue::Int(v)),
        Rvalue::ConstBool(v) => Some(MirValue::Bool(v)),
        Rvalue::Move(local) => Some(MirValue::Local(local)),
        _ => None,
    }
}

fn lower_value<'a>(ctx: &mut LowerCtx<'a>, expr: &'a Expr) -> Option<MirValue> {
    match expr {
        Expr::Lit(glyph_core::ast::Literal::Int(i), _) => Some(MirValue::Int(*i)),
        Expr::Lit(glyph_core::ast::Literal::Bool(b), _) => Some(MirValue::Bool(*b)),
        Expr::Lit(glyph_core::ast::Literal::Char(c), _) => {
            let tmp = ctx.fresh_local(None);
            ctx.locals[tmp.0 as usize].ty = Some(Type::Char);
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: Rvalue::ConstInt(*c as i64),
            });
            Some(MirValue::Local(tmp))
        }
        Expr::Lit(glyph_core::ast::Literal::Str(s), _) => {
            let tmp = ctx.fresh_local(None);
            ctx.locals[tmp.0 as usize].ty = Some(Type::Str);
            let rv = Rvalue::StringLit {
                content: s.clone(),
                global_name: ctx.fresh_string_global(),
            };
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: rv,
            });
            Some(MirValue::Local(tmp))
        }
        Expr::InterpString { segments, span } => {
            let rv = lower_interp_string(ctx, segments, *span)?;
            let tmp = ctx.fresh_local(None);
            ctx.locals[tmp.0 as usize].ty = Some(Type::Str);
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: rv,
            });
            Some(MirValue::Local(tmp))
        }
        Expr::Ident(ident, span) => ctx
            .bindings
            .get(ident.0.as_str())
            .copied()
            .and_then(|local| {
                if ctx.consume_local(local, Some(*span)) {
                    Some(MirValue::Local(local))
                } else {
                    None
                }
            }),
        Expr::Binary { op, lhs, rhs, .. } => match *op {
            glyph_core::ast::BinaryOp::And | glyph_core::ast::BinaryOp::Or => {
                lower_logical(ctx, op, lhs, rhs).and_then(rvalue_to_value)
            }
            _ => lower_binary(ctx, op, lhs, rhs).and_then(rvalue_to_value),
        },
        Expr::Call { callee, args, span } => {
            lower_call(ctx, callee, args, *span, true).and_then(rvalue_to_value)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            span,
        } => lower_method_call(ctx, receiver, method, args, *span).and_then(rvalue_to_value),
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => lower_match(ctx, scrutinee, arms, true, *span).and_then(rvalue_to_value),
        Expr::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            lower_if(ctx, cond, then_block, else_block.as_ref());
            None
        }
        Expr::StructLit { name, fields, span } => {
            lower_struct_lit(ctx, name, fields, *span).and_then(rvalue_to_value)
        }
        Expr::FieldAccess { base, field, span } => {
            lower_field_access(ctx, base, field, *span).and_then(rvalue_to_value)
        }
        Expr::Ref {
            expr,
            mutability,
            span,
        } => {
            let rv = lower_ref_expr(ctx, expr, *mutability, *span)?;
            let tmp = ctx.fresh_local(None);
            update_local_type_from_rvalue(ctx, tmp, &rv);
            ctx.push_inst(MirInst::Assign {
                local: tmp,
                value: rv,
            });
            Some(MirValue::Local(tmp))
        }
        Expr::ArrayLit { elements, span } => {
            lower_array_lit(ctx, elements, *span).and_then(rvalue_to_value)
        }
        Expr::Index { base, index, span } => {
            lower_array_index(ctx, base, index, *span).and_then(rvalue_to_value)
        }
        _ => None,
    }
}

#[cfg(all(test, feature = "mir_lower_tests"))]
mod tests {
    use super::*;
    use crate::resolver::{ResolverContext, resolve_types};
    use crate::{FrontendOptions, compile_source};
    use glyph_core::ast::{
        BinaryOp, Block, Expr, FieldDef, Function, Ident, Item, Literal, Module, Param, Stmt,
        StructDef, TypeExpr,
    };
    use glyph_core::span::Span;
    use glyph_core::types::{EnumType, Mutability, StructType, Type};

    fn path_ty(name: &str, span: Span) -> TypeExpr {
        TypeExpr::Path {
            segments: vec![name.to_string()],
            span,
        }
    }

    #[test]
    fn lowers_reference_expression() {
        let span = Span::new(0, 5);
        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: Some(path_ty("i32", span)),

            body: Block {
                span,
                stmts: vec![
                    Stmt::Let {
                        name: Ident("p".into()),
                        ty: Some(path_ty("Point", span)),
                        value: Some(Expr::StructLit {
                            name: Ident("Point".into()),
                            fields: vec![
                                (Ident("x".into()), Expr::Lit(Literal::Int(1), span)),
                                (Ident("y".into()), Expr::Lit(Literal::Int(2), span)),
                            ],
                            span,
                        }),
                        span,
                    },
                    Stmt::Let {
                        name: Ident("pref".into()),
                        ty: None,
                        value: Some(Expr::Ref {
                            expr: Box::new(Expr::Ident(Ident("p".into()), span)),
                            mutability: Mutability::Immutable,
                            span,
                        }),
                        span,
                    },
                    Stmt::Ret(
                        Some(Expr::FieldAccess {
                            base: Box::new(Expr::Ident(Ident("pref".into()), span)),
                            field: Ident("x".into()),
                            span,
                        }),
                        span,
                    ),
                ],
            },
            span,
        };

        let point_struct = Item::Struct(StructDef {
            name: Ident("Point".into()),
            generic_params: Vec::new(),
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: path_ty("i32", span),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: path_ty("i32", span),
                    span,
                },
            ],
            interfaces: Vec::new(),
            methods: Vec::new(),
            inline_impls: Vec::new(),
            span,
        });

        let module = Module {
            imports: vec![],
            items: vec![point_struct, Item::Function(func)],
        };

        let module = Module {
            imports: vec![],
            items: vec![point_struct, Item::Function(func)],
        };

        let (resolver, diags) = resolve_types(&module);
        assert!(diags.is_empty());

        let (_mir, lower_diags) = lower_module(&module, &resolver);
        assert!(
            lower_diags.is_empty(),
            "unexpected diagnostics: {:?}",
            lower_diags
        );
    }

    #[test]
    fn lowers_function_to_mir_with_return() {
        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: None,
            body: Block {
                span: Span::new(0, 4),
                stmts: vec![Stmt::Ret(None, Span::new(0, 4))],
            },
            span: Span::new(0, 4),
        };
        let module = Module {
            imports: vec![],
            items: vec![Item::Function(func)],
        };
        let (mir, diags) = lower_module(&module, &ResolverContext::default());
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
        assert_eq!(mir.functions.len(), 1);
        assert!(
            mir.functions[0]
                .blocks
                .iter()
                .any(|b| matches!(b.insts.last(), Some(MirInst::Return(_))))
        );
    }

    #[test]
    fn lowers_binary_expression_to_temp_and_return() {
        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: None,
            body: Block {
                span: Span::new(0, 10),
                stmts: vec![Stmt::Ret(
                    Some(Expr::Binary {
                        op: BinaryOp::Add,
                        lhs: Box::new(Expr::Lit(glyph_core::ast::Literal::Int(1), Span::new(0, 1))),
                        rhs: Box::new(Expr::Lit(glyph_core::ast::Literal::Int(2), Span::new(2, 3))),
                        span: Span::new(0, 3),
                    }),
                    Span::new(0, 3),
                )],
            },
            span: Span::new(0, 10),
        };
        let module = Module {
            imports: vec![],
            items: vec![Item::Function(func)],
        };
        let (mir, diags) = lower_module(&module, &ResolverContext::default());
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
        let insts = &mir.functions[0].blocks[0].insts;
        assert!(insts.iter().any(|i| matches!(i, MirInst::Assign { .. })));
        assert!(matches!(
            insts.last(),
            Some(MirInst::Return(Some(MirValue::Local(_))))
        ));
    }

    #[test]
    fn lowers_struct_literal_and_field_access() {
        let span = Span::new(0, 5);
        let point_struct = Item::Struct(StructDef {
            name: Ident("Point".into()),
            generic_params: Vec::new(),
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: path_ty("i32", span),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: path_ty("i32", span),
                    span,
                },
            ],
            interfaces: Vec::new(),
            methods: Vec::new(),
            inline_impls: Vec::new(),
            span,
        });

        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: Some(path_ty("i32", span)),
            body: Block {
                span,
                stmts: vec![
                    Stmt::Let {
                        name: Ident("p".into()),
                        ty: Some(path_ty("Point", span)),
                        value: Some(Expr::StructLit {
                            name: Ident("Point".into()),
                            fields: vec![
                                (Ident("x".into()), Expr::Lit(Literal::Int(1), span)),
                                (Ident("y".into()), Expr::Lit(Literal::Int(2), span)),
                            ],
                            span,
                        }),
                        span,
                    },
                    Stmt::Ret(
                        Some(Expr::FieldAccess {
                            base: Box::new(Expr::Ident(Ident("p".into()), span)),
                            field: Ident("x".into()),
                            span,
                        }),
                        span,
                    ),
                ],
            },
            span,
        };

        let module = Module {
            imports: vec![],
            items: vec![point_struct, Item::Function(func)],
        };

        let (resolver, diags) = resolve_types(&module);
        assert!(diags.is_empty());

        let (mir, lower_diags) = lower_module(&module, &resolver);
        assert!(
            lower_diags.is_empty(),
            "unexpected diagnostics: {:?}",
            lower_diags
        );

        assert_eq!(mir.struct_types.len(), 1);
        let insts = &mir.functions[0].blocks[0].insts;
        assert!(insts.iter().any(|inst| matches!(
            inst,
            MirInst::Assign {
                value: Rvalue::StructLit { .. },
                ..
            }
        )));
        assert!(insts.iter().any(|inst| matches!(
            inst,
            MirInst::Assign {
                value: Rvalue::FieldAccess { field_name, .. },
                ..
            } if field_name == "x"
        )));
    }

    #[test]
    fn struct_literal_missing_field_reports_error() {
        let span = Span::new(0, 5);
        let point_struct = Item::Struct(StructDef {
            name: Ident("Point".into()),
            generic_params: Vec::new(),
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: path_ty("i32", span),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: path_ty("i32", span),
                    span,
                },
            ],
            interfaces: Vec::new(),
            methods: Vec::new(),
            inline_impls: Vec::new(),
            span,
        });

        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: Some(path_ty("i32", span)),
            body: Block {
                span,
                stmts: vec![Stmt::Ret(
                    Some(Expr::StructLit {
                        name: Ident("Point".into()),
                        fields: vec![(Ident("x".into()), Expr::Lit(Literal::Int(1), span))],
                        span,
                    }),
                    span,
                )],
            },
            span,
        };

        let module = Module {
            imports: vec![],
            items: vec![point_struct, Item::Function(func)],
        };

        let (resolver, diags) = resolve_types(&module);
        assert!(diags.is_empty());

        let (_mir, lower_diags) = lower_module(&module, &resolver);
        assert!(
            lower_diags
                .iter()
                .any(|d| d.message.contains("missing field")),
            "expected missing-field diagnostic, got: {:?}",
            lower_diags
        );
    }

    #[test]
    fn lowers_assignment_statements_to_mir() {
        let span = Span::new(0, 5);
        let func = Function {
            name: Ident("main".into()),
            params: vec![],
            ret_type: None,
            body: Block {
                span,
                stmts: vec![
                    Stmt::Let {
                        name: Ident("x".into()),
                        ty: Some(path_ty("i32", Span::new(0, 0))),
                        value: Some(Expr::Lit(Literal::Int(1), span)),
                        span,
                    },
                    Stmt::Assign {
                        target: Expr::Ident(Ident("x".into()), span),
                        value: Expr::Lit(Literal::Int(2), span),
                        span,
                    },
                    Stmt::Ret(Some(Expr::Ident(Ident("x".into()), span)), span),
                ],
            },
            span,
        };

        let module = Module {
            imports: vec![],
            items: vec![Item::Function(func)],
        };

        let (mir, diags) = lower_module(&module, &ResolverContext::default());
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);

        let assigns: Vec<_> = mir.functions[0].blocks[0]
            .insts
            .iter()
            .filter_map(|inst| match inst {
                MirInst::Assign { local, value } => Some((*local, value.clone())),
                _ => None,
            })
            .collect();

        assert!(
            assigns
                .iter()
                .any(|(local, value)| { local.0 == 0 && matches!(value, Rvalue::ConstInt(2)) })
        );
    }

    #[test]
    fn lowers_interface_method_call_via_impl() {
        let src = r#"
interface Drawable {
  fn draw(self: &Point) -> i32;
}

struct Point {
  x: i32
  impl Drawable {
    fn draw(self: &Point) -> i32 { ret self.x }
  }
}

fn main() -> i32 {
  let p = Point { x: 5 }
  ret p.draw()
}
"#;

        let out = compile_source(
            src,
            FrontendOptions {
                emit_mir: true,
                include_std: false,
            },
        );
        assert!(
            out.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            out.diagnostics
        );

        let main_fn = out
            .mir
            .functions
            .iter()
            .find(|f| f.name == "main")
            .expect("main function present");

        let (call_name, args) = main_fn
            .blocks
            .iter()
            .flat_map(|b| &b.insts)
            .find_map(|inst| match inst {
                MirInst::Assign {
                    value: Rvalue::Call { name, args },
                    ..
                } => Some((name.clone(), args.clone())),
                _ => None,
            })
            .expect("call lowered");

        assert_eq!(call_name, "Point::Drawable::draw");
        assert_eq!(args.len(), 1);

        if let MirValue::Local(local) = &args[0] {
            let ty = main_fn.locals[local.0 as usize]
                .ty
                .as_ref()
                .expect("receiver type");
            assert_eq!(
                ty,
                &Type::Ref(Box::new(Type::Named("Point".into())), Mutability::Immutable)
            );
        } else {
            panic!("expected receiver argument to be a local");
        }
    }

    #[test]
    fn lowers_extern_function_call() {
        let module = Module {
            imports: vec![],
            items: vec![
                Item::ExternFunction(glyph_core::ast::ExternFunctionDecl {
                    abi: Some("C".into()),
                    name: Ident("puts".into()),
                    params: vec![Param {
                        name: Ident("msg".into()),
                        ty: Some(path_ty("i32", Span::new(0, 0))),
                        span: Span::new(0, 0),
                    }],
                    ret_type: None,
                    link_name: None,
                    span: Span::new(0, 0),
                }),
                Item::Function(Function {
                    name: Ident("main".into()),
                    params: vec![],
                    ret_type: Some(path_ty("i32", Span::new(0, 0))),
                    body: Block {
                        span: Span::new(0, 0),
                        stmts: vec![
                            Stmt::Expr(
                                Expr::Call {
                                    callee: Box::new(Expr::Ident(
                                        Ident("puts".into()),
                                        Span::new(0, 0),
                                    )),
                                    args: vec![Expr::Lit(Literal::Int(1), Span::new(0, 0))],
                                    span: Span::new(0, 0),
                                },
                                Span::new(0, 0),
                            ),
                            Stmt::Ret(
                                Some(Expr::Lit(Literal::Int(0), Span::new(0, 0))),
                                Span::new(0, 0),
                            ),
                        ],
                    },
                    span: Span::new(0, 0),
                }),
            ],
        };

        let resolver = ResolverContext::default();
        let (mir, diags) = lower_module(&module, &resolver);
        assert!(diags.is_empty(), "unexpected diagnostics: {:?}", diags);
        assert_eq!(mir.extern_functions.len(), 1);
        assert_eq!(mir.extern_functions[0].name, "puts");

        let main = mir
            .functions
            .iter()
            .find(|f| f.name == "main")
            .expect("main exists");
        let mut found_call = false;
        for block in &main.blocks {
            for inst in &block.insts {
                if let MirInst::Assign {
                    value: Rvalue::Call { name, .. },
                    ..
                } = inst
                {
                    assert_eq!(name, "puts");
                    found_call = true;
                }
            }
        }
        assert!(found_call, "call to extern function not lowered");
    }

    #[test]
    fn extern_function_wrong_arity_reports_error() {
        let module = Module {
            imports: vec![],
            items: vec![
                Item::ExternFunction(glyph_core::ast::ExternFunctionDecl {
                    abi: Some("C".into()),
                    name: Ident("puts".into()),
                    params: vec![Param {
                        name: Ident("msg".into()),
                        ty: Some(path_ty("i32", Span::new(0, 0))),
                        span: Span::new(0, 0),
                    }],
                    ret_type: None,
                    link_name: None,
                    span: Span::new(0, 0),
                }),
                Item::Function(Function {
                    name: Ident("main".into()),
                    params: vec![],
                    ret_type: Some(path_ty("i32", Span::new(0, 0))),
                    body: Block {
                        span: Span::new(0, 0),
                        stmts: vec![
                            Stmt::Expr(
                                Expr::Call {
                                    callee: Box::new(Expr::Ident(
                                        Ident("puts".into()),
                                        Span::new(0, 0),
                                    )),
                                    args: vec![],
                                    span: Span::new(0, 0),
                                },
                                Span::new(0, 0),
                            ),
                            Stmt::Ret(
                                Some(Expr::Lit(Literal::Int(0), Span::new(0, 0))),
                                Span::new(0, 0),
                            ),
                        ],
                    },
                    span: Span::new(0, 0),
                }),
            ],
        };

        let resolver = ResolverContext::default();
        let (_mir, diags) = lower_module(&module, &resolver);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("expects 1 arguments but got 0")),
            "expected arity diagnostic, got {:?}",
            diags
        );
    }
}

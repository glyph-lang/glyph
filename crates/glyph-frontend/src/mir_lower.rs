use std::collections::HashMap;

use glyph_core::ast::{BinaryOp, Block, Expr, Function, Ident, Item, Module, Stmt};
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

                let mut param_types = Vec::new();
                for param in &func.params {
                    let ty = match &param.ty {
                        Some(t) => match resolve_type_name(&t.0, resolver) {
                            Some(resolved) => Some(resolved),
                            None => {
                                diagnostics.push(Diagnostic::error(
                                    format!(
                                        "unknown type '{}' for parameter '{}'",
                                        t.0, param.name.0
                                    ),
                                    Some(param.span),
                                ));
                                None
                            }
                        },
                        None => None,
                    };
                    param_types.push(ty);
                }

                let ret_type = match &func.ret_type {
                    Some(t) => match resolve_type_name(&t.0, resolver) {
                        Some(resolved) => Some(resolved),
                        None => {
                            diagnostics.push(Diagnostic::error(
                                format!("unknown return type '{}' for function '{}'", t.0, name),
                                Some(func.span),
                            ));
                            None
                        }
                    },
                    None => None,
                };

                signatures.insert(
                    name,
                    FnSig {
                        params: param_types,
                        ret: ret_type,
                        abi: None,
                    },
                );
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

                let mut param_types = Vec::new();
                for param in &func.params {
                    let ty = match &param.ty {
                        Some(t) => match resolve_type_name(&t.0, resolver) {
                            Some(resolved) => Some(resolved),
                            None => {
                                diagnostics.push(Diagnostic::error(
                                    format!(
                                        "unknown type '{}' for parameter '{}'",
                                        t.0, param.name.0
                                    ),
                                    Some(param.span),
                                ));
                                None
                            }
                        },
                        None => {
                            diagnostics.push(Diagnostic::error(
                                format!(
                                    "extern function '{}' parameter '{}' must have a type",
                                    name, param.name.0
                                ),
                                Some(param.span),
                            ));
                            None
                        }
                    };
                    param_types.push(ty);
                }

                let ret_type = match &func.ret_type {
                    Some(t) => match resolve_type_name(&t.0, resolver) {
                        Some(resolved) => Some(resolved),
                        None => {
                            diagnostics.push(Diagnostic::error(
                                format!("unknown return type '{}' for function '{}'", t.0, name),
                                Some(func.span),
                            ));
                            None
                        }
                    },
                    None => None,
                };

                signatures.insert(
                    name,
                    FnSig {
                        params: param_types,
                        ret: ret_type,
                        abi: func.abi.clone(),
                    },
                );
            }
            _ => {}
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
            .map(|ty| matches!(ty, Type::Own(_) | Type::Shared(_)))
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
        .and_then(|t| resolve_type_name(&t.0, resolver));

    // Create locals for parameters and bind them
    let mut param_locals = Vec::new();
    for param in &func.params {
        let local = ctx.fresh_local(Some(&param.name.0));
        // Try to infer parameter type from annotation
        if let Some(ty) = param
            .ty
            .as_ref()
            .and_then(|t| resolve_type_name(&t.0, resolver))
        {
            ctx.locals[local.0 as usize].ty = Some(ty);
        }
        if let Some(state) = ctx.local_states.get_mut(local.0 as usize) {
            *state = LocalState::Initialized;
        }
        ctx.bindings.insert(&param.name.0, local);
        param_locals.push(local);
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
                name, ty, value, ..
            } => {
                let local = ctx.fresh_local(Some(&name.0));
                ctx.bindings.insert(&name.0, local);

                if let Some(annot_ty) = ty
                    .as_ref()
                    .and_then(|t| resolve_type_name(&t.0, ctx.resolver))
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

    // Try to resolve function name (may be qualified or imported)
    let resolved_name = if let Some(resolved) = ctx.resolver.resolve_symbol(&name.0) {
        match resolved {
            crate::resolver::ResolvedSymbol::Function(module_id, func_name) => {
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
        ctx.error(format!("unknown function '{}'", name.0), Some(span));
        return None;
    };

    if sig.params.len() != args.len() {
        ctx.error(
            format!(
                "function '{}' expects {} arguments but got {}",
                resolved_name,
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
            format!("function '{}' has unknown parameter types", resolved_name),
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
    if let Some(ret) = sig.ret.as_ref() {
        ctx.locals[tmp.0 as usize].ty = Some(ret.clone());
    }

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::Call {
            name: resolved_name,
            args: lowered_args,
        },
    });

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

        Expr::Lit(glyph_core::ast::Literal::Str(_), _) => Some(Type::Str),

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
            return lower_array_len(ctx, receiver, span);
        }
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
        _ => None,
    }
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

    let elem_type = match base_type {
        Type::Array(elem_ty, _) => elem_ty.as_ref().clone(),
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
    ctx.locals[tmp.0 as usize].ty = Some(elem_type);

    ctx.push_inst(MirInst::Assign {
        local: tmp,
        value: Rvalue::ArrayIndex {
            base: base_local,
            index: index_val,
            bounds_check: true, // Always check bounds
        },
    });

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

    let tmp = ctx.fresh_local(None);
    ctx.locals[tmp.0 as usize].ty = Some(field_type.clone());

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
        Expr::Ident(ident, span) => ctx.bindings.get(ident.0.as_str()).copied().or_else(|| {
            ctx.error(format!("unknown identifier '{}'", ident.0), Some(*span));
            None
        }),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::{ResolverContext, resolve_types};
    use crate::{FrontendOptions, compile_source};
    use glyph_core::ast::{
        BinaryOp, Block, Expr, FieldDef, Function, Ident, Item, Literal, Module, Param, Stmt,
        StructDef,
    };
    use glyph_core::span::Span;
    use glyph_core::types::{Mutability, StructType, Type};

    #[test]
    fn lowers_reference_expression() {
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
                        ty: Some(Ident("i32".into())),
                        value: Some(Expr::Lit(Literal::Int(1), span)),
                        span,
                    },
                    Stmt::Let {
                        name: Ident("r".into()),
                        ty: None,
                        value: Some(Expr::Ref {
                            expr: Box::new(Expr::Ident(Ident("x".into()), span)),
                            mutability: Mutability::Immutable,
                            span,
                        }),
                        span,
                    },
                    Stmt::Ret(None, span),
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
                MirInst::Assign { local, value } => Some((*local, value)),
                _ => None,
            })
            .collect();

        assert!(
            assigns
                .iter()
                .any(|(_, value)| matches!(value, Rvalue::Ref { .. }))
        );
    }

    #[test]
    fn resolve_type_name_parses_shared_reference_to_primitive() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("&i32", &ctx).expect("type");
        assert_eq!(ty, Type::Ref(Box::new(Type::I32), Mutability::Immutable));
    }

    #[test]
    fn resolve_type_name_parses_mut_reference_to_struct() {
        let mut ctx = ResolverContext::default();
        ctx.struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![],
            },
        );

        let ty = resolve_type_name("&mut Point", &ctx).expect("type");
        assert_eq!(
            ty,
            Type::Ref(Box::new(Type::Named("Point".into())), Mutability::Mutable,)
        );
    }

    #[test]
    fn resolve_type_name_keeps_identifiers_starting_with_mut() {
        let mut ctx = ResolverContext::default();
        ctx.struct_types.insert(
            "mutPoint".into(),
            StructType {
                name: "mutPoint".into(),
                fields: vec![],
            },
        );

        let ty = resolve_type_name("&mutPoint", &ctx).expect("type");
        assert_eq!(
            ty,
            Type::Ref(
                Box::new(Type::Named("mutPoint".into())),
                Mutability::Immutable,
            )
        );
    }

    #[test]
    fn resolve_type_name_rejects_missing_inner_type() {
        let ctx = ResolverContext::default();
        assert!(resolve_type_name("&mut", &ctx).is_none());
    }

    #[test]
    fn resolve_type_name_parses_array_of_primitive() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("[i32; 10]", &ctx).expect("type");
        assert_eq!(ty, Type::Array(Box::new(Type::I32), 10));
    }

    #[test]
    fn resolve_type_name_parses_nested_array() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("[[i32; 3]; 2]", &ctx).expect("type");
        assert_eq!(
            ty,
            Type::Array(Box::new(Type::Array(Box::new(Type::I32), 3)), 2)
        );
    }

    #[test]
    fn resolve_type_name_parses_array_of_struct() {
        let mut ctx = ResolverContext::default();
        ctx.struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![],
            },
        );

        let ty = resolve_type_name("[Point; 5]", &ctx).expect("type");
        assert_eq!(ty, Type::Array(Box::new(Type::Named("Point".into())), 5));
    }

    #[test]
    fn resolve_type_name_parses_own_of_primitive() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("Own<i32>", &ctx).expect("type");
        assert_eq!(ty, Type::Own(Box::new(Type::I32)));
    }

    #[test]
    fn resolve_type_name_parses_raw_ptr_of_struct() {
        let mut ctx = ResolverContext::default();
        ctx.struct_types.insert(
            "Node".into(),
            StructType {
                name: "Node".into(),
                fields: vec![],
            },
        );
        let ty = resolve_type_name("RawPtr<Node>", &ctx).expect("type");
        assert_eq!(ty, Type::RawPtr(Box::new(Type::Named("Node".into()))));
    }

    #[test]
    fn resolve_type_name_rejects_malformed_own() {
        let ctx = ResolverContext::default();
        assert!(resolve_type_name("Own<Point", &ctx).is_none());
    }

    #[test]
    fn resolve_type_name_rejects_array_with_zero_size() {
        let ctx = ResolverContext::default();
        assert!(resolve_type_name("[i32; 0]", &ctx).is_none());
    }

    #[test]
    fn resolve_type_name_rejects_array_without_semicolon() {
        let ctx = ResolverContext::default();
        assert!(resolve_type_name("[i32 10]", &ctx).is_none());
    }

    #[test]
    fn resolve_type_name_parses_str() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("str", &ctx).expect("type");
        assert_eq!(ty, Type::Str);
    }

    #[test]
    fn resolve_type_name_parses_ref_str() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("&str", &ctx).expect("type");
        assert_eq!(ty, Type::Str);
    }

    #[test]
    fn resolve_type_name_parses_raw_ptr_u8() {
        let ctx = ResolverContext::default();
        let ty = resolve_type_name("RawPtr<u8>", &ctx).expect("type");
        assert_eq!(ty, Type::RawPtr(Box::new(Type::U8)));
    }

    #[test]
    fn field_access_auto_dereferences_reference_base() {
        let span = Span::new(0, 5);
        let point_struct = Item::Struct(StructDef {
            name: Ident("Point".into()),
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: Ident("i32".into()),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: Ident("i32".into()),
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
            ret_type: Some(Ident("i32".into())),
            body: Block {
                span,
                stmts: vec![
                    Stmt::Let {
                        name: Ident("p".into()),
                        ty: Some(Ident("Point".into())),
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
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: Ident("i32".into()),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: Ident("i32".into()),
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
            ret_type: Some(Ident("i32".into())),
            body: Block {
                span,
                stmts: vec![
                    Stmt::Let {
                        name: Ident("p".into()),
                        ty: Some(Ident("Point".into())),
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
            fields: vec![
                FieldDef {
                    name: Ident("x".into()),
                    ty: Ident("i32".into()),
                    span,
                },
                FieldDef {
                    name: Ident("y".into()),
                    ty: Ident("i32".into()),
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
            ret_type: Some(Ident("i32".into())),
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
                        ty: Some(Ident("i32".into())),
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

        let out = compile_source(src, FrontendOptions { emit_mir: true });
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
                        ty: Some(Ident("i32".into())),
                        span: Span::new(0, 0),
                    }],
                    ret_type: None,
                    link_name: None,
                    span: Span::new(0, 0),
                }),
                Item::Function(Function {
                    name: Ident("main".into()),
                    params: vec![],
                    ret_type: Some(Ident("i32".into())),
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
                        ty: Some(Ident("i32".into())),
                        span: Span::new(0, 0),
                    }],
                    ret_type: None,
                    link_name: None,
                    span: Span::new(0, 0),
                }),
                Item::Function(Function {
                    name: Ident("main".into()),
                    params: vec![],
                    ret_type: Some(Ident("i32".into())),
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

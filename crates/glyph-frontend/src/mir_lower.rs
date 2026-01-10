use std::collections::HashMap;

use glyph_core::ast::{BinaryOp, Block, Expr, Function, Ident, Item, Module, Stmt};
use glyph_core::diag::Diagnostic;
use glyph_core::mir::{
    BlockId, Local, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
};
use glyph_core::span::Span;
use glyph_core::types::{Mutability, Type};

use crate::resolver::ResolverContext;

#[derive(Debug, Clone)]
struct FnSig {
    params: Vec<Option<Type>>,
    ret: Option<Type>,
}

#[derive(Debug, Clone)]
struct LoopContext {
    header: BlockId,  // for continue
    exit: BlockId,    // for break
}

pub fn lower_module(module: &Module, resolver: &ResolverContext) -> (MirModule, Vec<Diagnostic>) {
    let (fn_sigs, mut diagnostics) = collect_function_signatures(module, resolver);

    let mut mir = MirModule {
        struct_types: resolver.struct_types.clone(),
        functions: Vec::new(),
    };

    for item in &module.items {
        match item {
            Item::Function(func) => {
                let (lowered, diags) = lower_function(func, resolver, &fn_sigs);
                diagnostics.extend(diags);
                mir.functions.push(lowered);
            }
            Item::Struct(_) => {
                // Struct definitions are handled by the resolver/type system.
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
        let Item::Function(func) = item else { continue };

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
                            format!("unknown type '{}' for parameter '{}'", t.0, param.name.0),
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
            },
        );
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
    blocks: Vec<MirBlock>,
    current: BlockId,
    loop_stack: Vec<LoopContext>,
}

impl<'a> LowerCtx<'a> {
    fn new(resolver: &'a ResolverContext, fn_sigs: &'a HashMap<String, FnSig>) -> Self {
        let mut blocks = Vec::new();
        blocks.push(MirBlock::default());
        Self {
            resolver,
            fn_sigs,
            diagnostics: Vec::new(),
            locals: Vec::new(),
            bindings: HashMap::new(),
            next_local: 0,
            blocks,
            current: BlockId(0),
            loop_stack: Vec::new(),
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    fn current_block_mut(&mut self) -> &mut MirBlock {
        &mut self.blocks[self.current.0 as usize]
    }

    fn push_inst(&mut self, inst: MirInst) {
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
        id
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
}

fn lower_function(
    func: &Function,
    resolver: &ResolverContext,
    fn_sigs: &HashMap<String, FnSig>,
) -> (MirFunction, Vec<Diagnostic>) {
    let mut ctx = LowerCtx::new(resolver, fn_sigs);

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
        ctx.bindings.insert(&param.name.0, local);
        param_locals.push(local);
    }

    let implicit_return = lower_block(&mut ctx, &func.body);
    if !ctx.terminated() {
        // If block returned a value, use it; otherwise return void
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

    last_value
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
        Expr::Ident(ident, _) => ctx
            .bindings
            .get(ident.0.as_str())
            .copied()
            .map(Rvalue::Move),
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
            name: name.0.clone(),
            args: lowered_args,
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

    if let Some(primitive) = Type::from_name(trimmed) {
        return Some(primitive);
    }

    if let Some(reference) = parse_reference_type(trimmed, resolver) {
        return Some(reference);
    }

    resolver
        .struct_types
        .contains_key(trimmed)
        .then(|| Type::Named(trimmed.to_string()))
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
        Rvalue::Ref { base, mutability } => ctx
            .locals
            .get(base.0 as usize)
            .and_then(|l| l.ty.as_ref().cloned())
            .map(|inner| Type::Ref(Box::new(inner), *mutability)),
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
        Expr::Ident(ident, _) => ctx
            .bindings
            .get(ident.0.as_str())
            .copied()
            .map(MirValue::Local),
        Expr::Binary { op, lhs, rhs, .. } => match *op {
            glyph_core::ast::BinaryOp::And | glyph_core::ast::BinaryOp::Or => {
                lower_logical(ctx, op, lhs, rhs).and_then(rvalue_to_value)
            }
            _ => lower_binary(ctx, op, lhs, rhs).and_then(rvalue_to_value),
        },
        Expr::Call { callee, args, span } => {
            lower_call(ctx, callee, args, *span, true).and_then(rvalue_to_value)
        }
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
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::{ResolverContext, resolve_types};
    use glyph_core::ast::{
        BinaryOp, Block, Expr, FieldDef, Function, Ident, Item, Literal, Module, Stmt, StructDef,
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
}

use std::collections::HashMap;

use glyph_core::ast::{Block, Expr, Function, Item, Module, Stmt};
use glyph_core::mir::{
    BlockId, Local, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
};
use glyph_core::types::Type;

pub fn lower_module(module: &Module) -> MirModule {
    let mut mir = MirModule::default();
    for item in &module.items {
        match item {
            Item::Function(func) => mir.functions.push(lower_function(func)),
            Item::Struct(_) => {
                // Struct definitions are handled by the type system, not lowered to MIR directly
                // They will be processed when we implement Phase 3 (type resolution)
            }
        }
    }
    mir
}

struct LowerCtx<'a> {
    locals: Vec<Local>,
    bindings: HashMap<&'a str, LocalId>,
    next_local: u32,
    blocks: Vec<MirBlock>,
    current: BlockId,
}

impl<'a> LowerCtx<'a> {
    fn new() -> Self {
        let mut blocks = Vec::new();
        blocks.push(MirBlock::default());
        Self {
            locals: Vec::new(),
            bindings: HashMap::new(),
            next_local: 0,
            blocks,
            current: BlockId(0),
        }
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
}

fn lower_function(func: &Function) -> MirFunction {
    let mut ctx = LowerCtx::new();

    // Resolve return type
    let ret_type = func.ret_type.as_ref().and_then(|t| Type::from_name(&t.0));

    // Create locals for parameters and bind them
    let mut param_locals = Vec::new();
    for param in &func.params {
        let local = ctx.fresh_local(Some(&param.name.0));
        // Try to infer parameter type from annotation
        if let Some(ty) = param.ty.as_ref().and_then(|t| Type::from_name(&t.0)) {
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
    MirFunction {
        name: func.name.0.clone(),
        ret_type,
        params: param_locals,
        locals: ctx.locals,
        blocks: ctx.blocks,
    }
}

fn lower_block<'a>(ctx: &mut LowerCtx<'a>, block: &'a Block) -> Option<MirValue> {
    let mut last_value = None;

    for (i, stmt) in block.stmts.iter().enumerate() {
        let is_last = i == block.stmts.len() - 1;

        match stmt {
            Stmt::Let { name, value, .. } => {
                let local = ctx.fresh_local(Some(&name.0));
                ctx.bindings.insert(&name.0, local);
                if let Some(rv) = value.as_ref().and_then(|e| lower_expr(ctx, e)) {
                    ctx.push_inst(MirInst::Assign { local, value: rv });
                } else {
                    ctx.push_inst(MirInst::Nop);
                }
            }
            Stmt::Ret(expr, _) => {
                let value = expr.as_ref().and_then(|e| lower_value(ctx, e));
                ctx.push_inst(MirInst::Return(value));
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
                        _ => {
                            let _ = lower_expr(ctx, expr);
                            ctx.push_inst(MirInst::Nop);
                        }
                    }
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
        Expr::Call { callee, args, .. } => lower_call(ctx, callee, args),
        Expr::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            lower_if(ctx, cond, then_block, else_block.as_ref());
            None
        }
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

fn lower_call<'a>(ctx: &mut LowerCtx<'a>, callee: &'a Expr, args: &'a [Expr]) -> Option<Rvalue> {
    if let Expr::Ident(name, _) = callee {
        let lowered_args: Vec<MirValue> = args.iter().filter_map(|a| lower_value(ctx, a)).collect();
        let tmp = ctx.fresh_local(None);
        ctx.push_inst(MirInst::Assign {
            local: tmp,
            value: Rvalue::Call {
                name: name.0.clone(),
                args: lowered_args,
            },
        });
        Some(Rvalue::Move(tmp))
    } else {
        ctx.push_inst(MirInst::Nop);
        None
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
        Expr::Call { callee, args, .. } => lower_call(ctx, callee, args).and_then(rvalue_to_value),
        Expr::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            lower_if(ctx, cond, then_block, else_block.as_ref());
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::ast::{BinaryOp, Block, Ident, Item, Module, Stmt};
    use glyph_core::span::Span;

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
        let mir = lower_module(&module);
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
        let mir = lower_module(&module);
        let insts = &mir.functions[0].blocks[0].insts;
        assert!(insts.iter().any(|i| matches!(i, MirInst::Assign { .. })));
        assert!(matches!(
            insts.last(),
            Some(MirInst::Return(Some(MirValue::Local(_))))
        ));
    }
}

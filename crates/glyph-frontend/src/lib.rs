use glyph_core::{
    ast::{Block, Expr, Function, Ident, Literal, Module, Stmt},
    diag::Diagnostic,
    mir::MirModule,
    span::Span,
};

mod lexer;
mod method_symbols;
mod mir_lower;
mod module_resolver;
mod monomorphize;
mod parser;
mod resolver;
mod stdlib;

pub use lexer::{LexOutput, lex};
pub use mir_lower::lower_module;
pub use module_resolver::{
    DependencyGraph, ImportScope, ModuleSymbols, MultiModuleContext, build_dependency_graph,
    resolve_multi_module,
};
pub use parser::{ParseOutput, parse};
pub use resolver::{ResolverContext, resolve_types};
pub use stdlib::std_modules;

#[derive(Debug, Clone, Default)]
pub struct FrontendOptions {
    pub emit_mir: bool,
    /// Include the built-in std modules during lowering (import std will work).
    pub include_std: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FrontendOutput {
    pub module: Option<Module>,
    pub mir: MirModule,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn compile_source(source: &str, opts: FrontendOptions) -> FrontendOutput {
    let LexOutput {
        tokens,
        diagnostics,
    } = lex(source);
    let mut diagnostics = diagnostics;

    let mut module = None;

    if diagnostics.is_empty() {
        let ParseOutput {
            module: parsed_module,
            diagnostics: parse_diags,
        } = parse(&tokens, source);
        diagnostics.extend(parse_diags);
        if diagnostics.is_empty() {
            module = Some(parsed_module);
        }
    }

    let mut mir = MirModule::default();

    if diagnostics.is_empty() {
        if let Some(ref m) = module {
            if opts.include_std {
                // Multi-module path with injected std
                let mut modules = std_modules();
                modules.insert("main".to_string(), m.clone());

                match module_resolver::resolve_multi_module(modules, std::path::Path::new(".")) {
                    Ok((multi_ctx, compile_order)) => {
                        let mut merged = MirModule::default();
                        let mut seen_externs = std::collections::HashSet::new();

                        for module_id in compile_order {
                            let module = multi_ctx.modules.get(&module_id).unwrap();

                            let (mut ctx, resolve_diags) = resolver::resolve_types(module);
                            diagnostics.extend(resolve_diags);
                            if !diagnostics.is_empty() {
                                break;
                            }

                            ctx.current_module = Some(module_id.clone());
                            ctx.import_scope = multi_ctx.import_scopes.get(&module_id).cloned();
                            ctx.all_modules = Some(multi_ctx.clone());
                            resolver::populate_imported_types(&mut ctx);

                            let (lowered, lower_diags) = mir_lower::lower_module(module, &ctx);
                            diagnostics.extend(lower_diags);
                            if !diagnostics.is_empty() {
                                break;
                            }

                            merged.struct_types.extend(lowered.struct_types.into_iter());
                            merged.enum_types.extend(lowered.enum_types.into_iter());
                            merged.functions.extend(lowered.functions);

                            for ex in lowered.extern_functions {
                                if seen_externs.insert(ex.name.clone()) {
                                    merged.extern_functions.push(ex);
                                }
                            }
                        }

                        if diagnostics.is_empty() {
                            let mut merged = merged;
                            let mono_diags =
                                monomorphize::monomorphize_mir(&mut merged, &multi_ctx.modules);
                            diagnostics.extend(mono_diags);
                            if diagnostics.is_empty() {
                                mir = merged;
                            }
                        }
                    }
                    Err(diags) => diagnostics.extend(diags),
                }
            } else {
                let (ctx, resolve_diags) = resolver::resolve_types(m);
                diagnostics.extend(resolve_diags);
                if diagnostics.is_empty() {
                    let (mut lowered, lower_diags) = mir_lower::lower_module(m, &ctx);
                    diagnostics.extend(lower_diags);
                    if diagnostics.is_empty() {
                        let modules =
                            std::collections::HashMap::from([("main".to_string(), m.clone())]);
                        diagnostics.extend(monomorphize::monomorphize_mir(&mut lowered, &modules));
                        if diagnostics.is_empty() {
                            mir = lowered;
                        }
                    }
                }
            }
        }
    }

    if diagnostics.is_empty() && !opts.emit_mir {
        mir = MirModule::default();
    }

    FrontendOutput {
        module,
        mir,
        diagnostics,
    }
}

pub fn demo_function() -> Function {
    let span = Span::new(0, 3);
    Function {
        name: Ident("demo".into()),
        params: vec![],
        ret_type: None,
        body: Block {
            span,
            stmts: vec![Stmt::Expr(Expr::Lit(Literal::Int(1), span), span)],
        },
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::diag::Severity;

    #[test]
    fn compile_source_returns_empty_diagnostics_on_valid_input() {
        let output = compile_source("fn main() {}", FrontendOptions::default());
        assert!(output.diagnostics.is_empty());
    }

    #[test]
    fn compile_source_returns_errors_on_lex_failure() {
        let output = compile_source("\"unterminated", FrontendOptions::default());
        assert!(!output.diagnostics.is_empty());
        assert!(matches!(output.diagnostics[0].severity, Severity::Error));
    }

    #[test]
    fn emit_mir_option_lowers_function() {
        let output = compile_source(
            "fn main() { ret }",
            FrontendOptions {
                emit_mir: true,
                include_std: false,
            },
        );
        assert_eq!(output.mir.functions.len(), 1);
        assert_eq!(output.mir.functions[0].name, "main");
    }

    #[test]
    fn emit_mir_const_return_value() {
        let output = compile_source(
            "fn main() { ret 1 }",
            FrontendOptions {
                emit_mir: true,
                include_std: false,
            },
        );
        let insts = &output.mir.functions[0].blocks[0].insts;
        assert!(matches!(
            insts.last(),
            Some(glyph_core::mir::MirInst::Return(Some(
                glyph_core::mir::MirValue::Int(1)
            )))
        ));
    }

    #[test]
    fn compiles_std_println_from_import() {
        let source = "from std import println\nfn main() { println(\"hi\") }";
        let output = compile_source(
            source,
            FrontendOptions {
                emit_mir: true,
                include_std: true,
            },
        );
        assert!(
            output.diagnostics.is_empty(),
            "diags: {:?}",
            output.diagnostics
        );
    }
}

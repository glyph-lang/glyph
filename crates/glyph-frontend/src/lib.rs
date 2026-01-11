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
mod parser;
mod resolver;

pub use lexer::{LexOutput, lex};
pub use mir_lower::lower_module;
pub use module_resolver::{
    DependencyGraph, ImportScope, ModuleSymbols, MultiModuleContext, build_dependency_graph,
    resolve_multi_module,
};
pub use parser::{ParseOutput, parse};
pub use resolver::{ResolverContext, resolve_types};

#[derive(Debug, Clone, Default)]
pub struct FrontendOptions {
    pub emit_mir: bool,
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

    let mut resolver_ctx = None;

    if diagnostics.is_empty() {
        if let Some(ref m) = module {
            let (ctx, resolve_diags) = resolver::resolve_types(m);
            diagnostics.extend(resolve_diags);
            if diagnostics.is_empty() {
                resolver_ctx = Some(ctx);
            }
        }
    }

    if diagnostics.is_empty() {
        if let (Some(ref m), Some(ref ctx)) = (module.as_ref(), resolver_ctx.as_ref()) {
            let (lowered, lower_diags) = mir_lower::lower_module(m, ctx);
            diagnostics.extend(lower_diags);
            if diagnostics.is_empty() {
                mir = lowered;
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
        let output = compile_source("fn main() { ret }", FrontendOptions { emit_mir: true });
        assert_eq!(output.mir.functions.len(), 1);
        assert_eq!(output.mir.functions[0].name, "main");
    }

    #[test]
    fn emit_mir_const_return_value() {
        let output = compile_source("fn main() { ret 1 }", FrontendOptions { emit_mir: true });
        let insts = &output.mir.functions[0].blocks[0].insts;
        assert!(matches!(
            insts.last(),
            Some(glyph_core::mir::MirInst::Return(Some(
                glyph_core::mir::MirValue::Int(1)
            )))
        ));
    }
}

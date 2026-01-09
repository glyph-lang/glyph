pub mod span {
    use serde::{Deserialize, Serialize};

    pub type BytePos = u32;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Span {
        pub start: BytePos,
        pub end: BytePos,
    }

    impl Span {
        pub fn new(start: BytePos, end: BytePos) -> Self {
            assert!(start <= end, "span start must be <= end");
            Self { start, end }
        }

        pub fn len(&self) -> BytePos {
            self.end - self.start
        }

        pub fn join(self, other: Span) -> Span {
            Span::new(self.start.min(other.start), self.end.max(other.end))
        }
    }
}

pub mod diag {
    use serde::{Deserialize, Serialize};

    use crate::span::Span;

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub enum Severity {
        Error,
        Warning,
        Note,
        Help,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Diagnostic {
        pub message: String,
        pub severity: Severity,
        pub span: Option<Span>,
    }

    impl Diagnostic {
        pub fn new(message: impl Into<String>, severity: Severity, span: Option<Span>) -> Self {
            Self {
                message: message.into(),
                severity,
                span,
            }
        }

        pub fn error(message: impl Into<String>, span: Option<Span>) -> Self {
            Self::new(message, Severity::Error, span)
        }

        pub fn warning(message: impl Into<String>, span: Option<Span>) -> Self {
            Self::new(message, Severity::Warning, span)
        }
    }
}

pub mod ast {
    use serde::{Deserialize, Serialize};

    use crate::span::Span;

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Ident(pub String);

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Literal {
        Int(i64),
        Float(f64),
        Bool(bool),
        Str(String),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Expr {
        Lit(Literal, Span),
        Ident(Ident, Span),
        Binary {
            op: BinaryOp,
            lhs: Box<Expr>,
            rhs: Box<Expr>,
            span: Span,
        },
        Call {
            callee: Box<Expr>,
            args: Vec<Expr>,
            span: Span,
        },
        If {
            cond: Box<Expr>,
            then_block: Block,
            else_block: Option<Block>,
            span: Span,
        },
        Block(Block),
        StructLit {
            name: Ident,
            fields: Vec<(Ident, Expr)>,
            span: Span,
        },
        FieldAccess {
            base: Box<Expr>,
            field: Ident,
            span: Span,
        },
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub enum BinaryOp {
        Add,
        Sub,
        Mul,
        Div,
        Eq,
        Ne,
        Lt,
        Le,
        Gt,
        Ge,
        And,
        Or,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Param {
        pub name: Ident,
        pub ty: Option<Ident>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct Block {
        pub span: Span,
        pub stmts: Vec<Stmt>,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Stmt {
        Expr(Expr, Span),
        Ret(Option<Expr>, Span),
        Let {
            name: Ident,
            ty: Option<Ident>,
            value: Option<Expr>,
            span: Span,
        },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct Function {
        pub name: Ident,
        pub params: Vec<Param>,
        pub ret_type: Option<Ident>,
        pub body: Block,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct FieldDef {
        pub name: Ident,
        pub ty: Ident,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct StructDef {
        pub name: Ident,
        pub fields: Vec<FieldDef>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Item {
        Function(Function),
        Struct(StructDef),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
    pub struct Module {
        pub items: Vec<Item>,
    }
}

pub mod token {
    use serde::{Deserialize, Serialize};

    use crate::span::Span;

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub struct Token {
        pub kind: TokenKind,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub enum TokenKind {
        // Keywords
        Fn,
        Let,
        Mut,
        Type,
        Struct,
        Enum,
        Impl,
        Use,
        Pub,
        If,
        Else,
        For,
        While,
        Match,
        Break,
        Cont,
        Ret,
        // Identifiers and literals
        Ident,
        Int,
        Float,
        Str,
        Bool,
        // Punctuation / operators
        LParen,
        RParen,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        Comma,
        Colon,
        Semicolon,
        Dot,
        Arrow,
        FatArrow,
        Plus,
        Minus,
        Star,
        Slash,
        Percent,
        Eq,
        EqEq,
        Bang,
        BangEq,
        Lt,
        Gt,
        Le,
        Ge,
        AmpAmp,
        PipePipe,
        Question,
        Eof,
        Unknown,
    }

    impl Token {
        pub fn new(kind: TokenKind, span: Span) -> Self {
            Self { kind, span }
        }
    }
}

pub mod types {
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub enum Type {
        I32,
        I64,
        U32,
        U64,
        F32,
        F64,
        Bool,
        Void,
        Named(String),
    }

    impl Type {
        pub fn from_name(name: &str) -> Option<Self> {
            match name {
                "i32" | "i" => Some(Type::I32),
                "i64" => Some(Type::I64),
                "u32" | "u" => Some(Type::U32),
                "u64" => Some(Type::U64),
                "f32" => Some(Type::F32),
                "f64" | "f" => Some(Type::F64),
                "bool" | "b" => Some(Type::Bool),
                _ => None,
            }
        }

        pub fn is_int(&self) -> bool {
            matches!(self, Type::I32 | Type::I64 | Type::U32 | Type::U64)
        }

        pub fn is_float(&self) -> bool {
            matches!(self, Type::F32 | Type::F64)
        }

        pub fn is_numeric(&self) -> bool {
            self.is_int() || self.is_float()
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct StructType {
        pub name: String,
        pub fields: Vec<(String, Type)>,
    }
}

pub mod mir {
    use super::ast::BinaryOp;
    use super::types::{StructType, Type};
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct MirFunction {
        pub name: String,
        pub ret_type: Option<Type>,
        pub params: Vec<LocalId>,
        pub locals: Vec<Local>,
        pub blocks: Vec<MirBlock>,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct Local {
        pub name: Option<String>,
        pub ty: Option<Type>,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
    pub struct MirModule {
        pub struct_types: HashMap<String, StructType>,
        pub functions: Vec<MirFunction>,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
    pub struct MirBlock {
        pub insts: Vec<MirInst>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct BlockId(pub u32);

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum MirInst {
        Assign {
            local: LocalId,
            value: Rvalue,
        },
        Return(Option<MirValue>),
        Goto(BlockId),
        If {
            cond: MirValue,
            then_bb: BlockId,
            else_bb: BlockId,
        },
        Nop,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Rvalue {
        ConstInt(i64),
        ConstBool(bool),
        Move(LocalId),
        Binary {
            op: BinaryOp,
            lhs: MirValue,
            rhs: MirValue,
        },
        Call {
            name: String,
            args: Vec<MirValue>,
        },
        StructLit {
            struct_name: String,
            field_values: Vec<(String, MirValue)>,
        },
        FieldAccess {
            base: LocalId,
            field_name: String,
            field_index: u32,
        },
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct LocalId(pub u32);

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum MirValue {
        Unit,
        Int(i64),
        Bool(bool),
        Local(LocalId),
    }
}

#[cfg(test)]
mod tests {
    use super::diag::{Diagnostic, Severity};
    use super::span::Span;

    #[test]
    fn spans_join_and_len() {
        let a = Span::new(0, 4);
        let b = Span::new(2, 6);
        let joined = a.join(b);
        assert_eq!(joined.start, 0);
        assert_eq!(joined.end, 6);
        assert_eq!(joined.len(), 6);
    }

    #[test]
    fn diagnostics_store_message_and_severity() {
        let diag = Diagnostic::error("oops", Some(Span::new(1, 2)));
        assert_eq!(diag.message, "oops");
        assert!(matches!(diag.severity, Severity::Error));
        assert_eq!(diag.span.unwrap().start, 1);
    }

    #[test]
    fn struct_def_construction() {
        use super::ast::{FieldDef, Ident, StructDef};

        let field1 = FieldDef {
            name: Ident("x".into()),
            ty: Ident("i32".into()),
            span: Span::new(0, 5),
        };
        let field2 = FieldDef {
            name: Ident("y".into()),
            ty: Ident("i32".into()),
            span: Span::new(6, 11),
        };

        let struct_def = StructDef {
            name: Ident("Point".into()),
            fields: vec![field1, field2],
            span: Span::new(0, 20),
        };

        assert_eq!(struct_def.name.0, "Point");
        assert_eq!(struct_def.fields.len(), 2);
        assert_eq!(struct_def.fields[0].name.0, "x");
        assert_eq!(struct_def.fields[1].ty.0, "i32");
    }

    #[test]
    fn struct_lit_expression() {
        use super::ast::{Expr, Ident, Literal};

        let expr = Expr::StructLit {
            name: Ident("Point".into()),
            fields: vec![
                (
                    Ident("x".into()),
                    Expr::Lit(Literal::Int(10), Span::new(0, 2)),
                ),
                (
                    Ident("y".into()),
                    Expr::Lit(Literal::Int(20), Span::new(4, 6)),
                ),
            ],
            span: Span::new(0, 10),
        };

        if let Expr::StructLit { name, fields, .. } = expr {
            assert_eq!(name.0, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0.0, "x");
        } else {
            panic!("Expected StructLit");
        }
    }

    #[test]
    fn field_access_expression() {
        use super::ast::{Expr, Ident};

        let base = Expr::Ident(Ident("p".into()), Span::new(0, 1));
        let expr = Expr::FieldAccess {
            base: Box::new(base),
            field: Ident("x".into()),
            span: Span::new(0, 3),
        };

        if let Expr::FieldAccess { field, .. } = expr {
            assert_eq!(field.0, "x");
        } else {
            panic!("Expected FieldAccess");
        }
    }
}

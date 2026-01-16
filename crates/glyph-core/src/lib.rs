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

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    pub enum TypeExpr {
        Path {
            segments: Vec<String>,
            span: Span,
        },
        App {
            base: Box<TypeExpr>,
            args: Vec<TypeExpr>,
            span: Span,
        },
        Ref {
            mutability: crate::types::Mutability,
            inner: Box<TypeExpr>,
            span: Span,
        },
        Array {
            elem: Box<TypeExpr>,
            size: usize,
            span: Span,
        },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Literal {
        Int(i64),
        Float(f64),
        Bool(bool),
        Str(String),
        Char(char),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Expr {
        Lit(Literal, Span),
        InterpString {
            segments: Vec<InterpSegment>,
            span: Span,
        },
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
        Ref {
            expr: Box<Expr>,
            mutability: crate::types::Mutability,
            span: Span,
        },
        While {
            cond: Box<Expr>,
            body: Block,
            span: Span,
        },
        For {
            var: Ident,
            start: Box<Expr>,
            end: Box<Expr>,
            body: Block,
            span: Span,
        },
        ArrayLit {
            elements: Vec<Expr>,
            span: Span,
        },
        Index {
            base: Box<Expr>,
            index: Box<Expr>,
            span: Span,
        },
        MethodCall {
            receiver: Box<Expr>,
            method: Ident,
            args: Vec<Expr>,
            span: Span,
        },
        Match {
            scrutinee: Box<Expr>,
            arms: Vec<MatchArm>,
            span: Span,
        },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum InterpSegment {
        Literal(String),
        Expr(Expr),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum MatchPattern {
        Wildcard,
        Variant { name: Ident, binding: Option<Ident> },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct MatchArm {
        pub pattern: MatchPattern,
        pub expr: Expr,
        pub span: Span,
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
        pub ty: Option<TypeExpr>,
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
            ty: Option<TypeExpr>,
            value: Option<Expr>,
            span: Span,
        },
        Assign {
            target: Expr,
            value: Expr,
            span: Span,
        },
        Break(Span),
        Continue(Span),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct Function {
        pub name: Ident,
        pub params: Vec<Param>,
        pub ret_type: Option<TypeExpr>,
        pub body: Block,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct FieldDef {
        pub name: Ident,
        pub ty: TypeExpr,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct StructDef {
        pub name: Ident,
        pub generic_params: Vec<Ident>,
        pub fields: Vec<FieldDef>,
        pub interfaces: Vec<Ident>,
        pub methods: Vec<Function>,
        pub inline_impls: Vec<InlineImpl>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct EnumVariantDef {
        pub name: Ident,
        pub payload: Option<TypeExpr>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct EnumDef {
        pub name: Ident,
        pub generic_params: Vec<Ident>,
        pub variants: Vec<EnumVariantDef>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct InlineImpl {
        pub interface: Ident,
        pub methods: Vec<Function>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct InterfaceDef {
        pub name: Ident,
        pub methods: Vec<InterfaceMethod>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct InterfaceMethod {
        pub name: Ident,
        pub params: Vec<Param>,
        pub ret_type: Option<TypeExpr>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct ImplBlock {
        pub interface: Ident,
        pub target: Ident,
        pub methods: Vec<Function>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct ImportPath {
        pub segments: Vec<String>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct ImportItem {
        pub name: Ident,
        pub alias: Option<Ident>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum ImportKind {
        Wildcard,
        Selective { items: Vec<ImportItem> },
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct Import {
        pub kind: ImportKind,
        pub path: ImportPath,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct ExternFunctionDecl {
        pub abi: Option<String>,
        pub name: Ident,
        pub params: Vec<Param>,
        pub ret_type: Option<TypeExpr>,
        pub link_name: Option<String>,
        pub span: Span,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Item {
        Function(Function),
        Struct(StructDef),
        Enum(EnumDef),
        Interface(InterfaceDef),
        Impl(ImplBlock),
        ExternFunction(ExternFunctionDecl),
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
    pub struct Module {
        pub imports: Vec<Import>,
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
        Interface,
        Enum,
        Impl,
        Use,
        Import,
        From,
        As,
        Pub,
        If,
        Else,
        For,
        In,
        While,
        Match,
        Break,
        Cont,
        Ret,
        Extern,
        // Identifiers and literals
        Ident,
        Int,
        Float,
        Str,
        Char,
        InterpStr,
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
        DotDot,
        Arrow,
        FatArrow,
        ColonColon,
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
        Amp,
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

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub enum Mutability {
        Immutable,
        Mutable,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub enum Type {
        I8,
        I32,
        I64,
        U8,
        U32,
        U64,
        Usize,
        F32,
        F64,
        Bool,
        Char,
        Str,
        String,
        Void,
        Named(String),
        Enum(String),
        Param(String),
        App { base: String, args: Vec<Type> },
        Ref(Box<Type>, Mutability),
        Array(Box<Type>, usize),
        Own(Box<Type>),
        RawPtr(Box<Type>),
        Shared(Box<Type>),
    }

    impl Type {
        pub fn from_name(name: &str) -> Option<Self> {
            match name {
                "i8" => Some(Type::I8),
                "i32" | "i" => Some(Type::I32),
                "i64" => Some(Type::I64),
                "u8" => Some(Type::U8),
                "u32" | "u" => Some(Type::U32),
                "u64" => Some(Type::U64),
                "usize" => Some(Type::Usize),
                "char" | "c" => Some(Type::Char),
                "f32" => Some(Type::F32),
                "f64" | "f" => Some(Type::F64),
                "bool" | "b" => Some(Type::Bool),
                "str" => Some(Type::Str),
                "String" => Some(Type::String),
                _ => None,
            }
        }

        pub fn is_int(&self) -> bool {
            matches!(
                self,
                Type::I8
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U32
                    | Type::U64
                    | Type::Usize
                    | Type::Char
            )
        }

        pub fn is_float(&self) -> bool {
            matches!(self, Type::F32 | Type::F64)
        }

        pub fn is_numeric(&self) -> bool {
            self.is_int() || self.is_float()
        }

        pub fn is_ref(&self) -> bool {
            matches!(self, Type::Ref(..))
        }

        pub fn is_param(&self) -> bool {
            matches!(self, Type::Param(_))
        }

        pub fn inner_type(&self) -> Option<&Type> {
            match self {
                Type::Ref(inner, _) => Some(inner),
                _ => None,
            }
        }

        pub fn is_mut_ref(&self) -> bool {
            matches!(self, Type::Ref(_, Mutability::Mutable))
        }

        pub fn is_array(&self) -> bool {
            matches!(self, Type::Array(..))
        }

        pub fn array_element_type(&self) -> Option<&Type> {
            match self {
                Type::Array(elem, _) => Some(elem),
                _ => None,
            }
        }

        pub fn array_size(&self) -> Option<usize> {
            match self {
                Type::Array(_, size) => Some(*size),
                _ => None,
            }
        }

        pub fn is_own(&self) -> bool {
            matches!(self, Type::Own(_))
        }

        pub fn own_inner_type(&self) -> Option<&Type> {
            match self {
                Type::Own(inner) => Some(inner),
                _ => None,
            }
        }

        pub fn is_raw_ptr(&self) -> bool {
            matches!(self, Type::RawPtr(_))
        }

        pub fn raw_ptr_inner_type(&self) -> Option<&Type> {
            match self {
                Type::RawPtr(inner) => Some(inner),
                _ => None,
            }
        }

        pub fn is_shared(&self) -> bool {
            matches!(self, Type::Shared(_))
        }

        pub fn shared_inner_type(&self) -> Option<&Type> {
            match self {
                Type::Shared(inner) => Some(inner),
                _ => None,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct StructType {
        pub name: String,
        pub fields: Vec<(String, Type)>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct EnumVariant {
        pub name: String,
        pub payload: Option<Type>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct EnumType {
        pub name: String,
        pub variants: Vec<EnumVariant>,
    }
}

pub mod mir {
    use super::ast::BinaryOp;
    use super::types::{EnumType, Mutability, StructType, Type};
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

    #[derive(Clone, PartialEq, Serialize, Deserialize, Default)]
    pub struct MirModule {
        pub struct_types: HashMap<String, StructType>,
        pub enum_types: HashMap<String, EnumType>,
        pub functions: Vec<MirFunction>,
        pub extern_functions: Vec<MirExternFunction>,
    }

    impl std::fmt::Debug for MirModule {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use std::collections::BTreeMap;

            let mut ds = f.debug_struct("MirModule");
            let struct_types: BTreeMap<_, _> = self.struct_types.iter().collect();
            ds.field("struct_types", &struct_types);
            if !self.enum_types.is_empty() {
                let enum_types: BTreeMap<_, _> = self.enum_types.iter().collect();
                ds.field("enum_types", &enum_types);
            }
            ds.field("functions", &self.functions);
            if !self.extern_functions.is_empty() {
                ds.field("extern_functions", &self.extern_functions);
            }
            ds.finish()
        }
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub struct MirExternFunction {
        pub name: String,
        pub ret_type: Option<Type>,
        pub params: Vec<Type>,
        pub abi: Option<String>,
        pub link_name: Option<String>,
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
        Drop(LocalId),
        Nop,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    pub enum Rvalue {
        ConstInt(i64),
        ConstBool(bool),
        Move(LocalId),
        StringLit {
            content: String,
            global_name: String,
        },
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
        Ref {
            base: LocalId,
            mutability: Mutability,
        },
        ArrayLit {
            elem_type: Type,
            elements: Vec<MirValue>,
        },
        ArrayIndex {
            base: LocalId,
            index: MirValue,
            bounds_check: bool,
        },
        ArrayLen {
            base: LocalId,
        },
        VecNew {
            elem_type: Type,
        },
        VecWithCapacity {
            elem_type: Type,
            capacity: MirValue,
        },
        VecPush {
            vec: LocalId,
            elem_type: Type,
            value: MirValue,
        },
        VecPop {
            vec: LocalId,
            elem_type: Type,
        },
        VecLen {
            vec: LocalId,
        },
        VecIndex {
            vec: LocalId,
            elem_type: Type,
            index: MirValue,
            bounds_check: bool,
        },
        MapNew {
            key_type: Type,
            value_type: Type,
        },
        MapWithCapacity {
            key_type: Type,
            value_type: Type,
            capacity: MirValue,
        },
        MapAdd {
            map: LocalId,
            key_type: Type,
            key: MirValue,
            value_type: Type,
            value: MirValue,
        },
        MapUpdate {
            map: LocalId,
            key_type: Type,
            key: MirValue,
            value_type: Type,
            value: MirValue,
        },
        MapDel {
            map: LocalId,
            key_type: Type,
            value_type: Type,
            key: MirValue,
        },
        MapGet {
            map: LocalId,
            key_type: Type,
            value_type: Type,
            key: MirValue,
        },
        MapHas {
            map: LocalId,
            key_type: Type,
            key: MirValue,
        },
        MapKeys {
            map: LocalId,
            key_type: Type,
            value_type: Type,
        },
        MapVals {
            map: LocalId,
            key_type: Type,
            value_type: Type,
        },
        OwnNew {
            value: MirValue,
            elem_type: Type,
        },
        OwnIntoRaw {
            base: LocalId,
            elem_type: Type,
        },
        OwnFromRaw {
            ptr: MirValue,
            elem_type: Type,
        },
        RawPtrNull {
            elem_type: Type,
        },
        SharedNew {
            value: MirValue,
            elem_type: Type,
        },
        SharedClone {
            base: LocalId,
            elem_type: Type,
        },
        EnumConstruct {
            enum_name: String,
            variant_index: u32,
            payload: Option<MirValue>,
        },
        EnumTag {
            base: LocalId,
        },
        EnumPayload {
            base: LocalId,
            variant_index: u32,
            payload_type: Type,
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
        use super::ast::{FieldDef, Ident, StructDef, TypeExpr};

        let field1 = FieldDef {
            name: Ident("x".into()),
            ty: TypeExpr::Path {
                segments: vec!["i32".into()],
                span: Span::new(0, 5),
            },
            span: Span::new(0, 5),
        };
        let field2 = FieldDef {
            name: Ident("y".into()),
            ty: TypeExpr::Path {
                segments: vec!["i32".into()],
                span: Span::new(6, 11),
            },
            span: Span::new(6, 11),
        };

        let struct_def = StructDef {
            name: Ident("Point".into()),
            generic_params: Vec::new(),
            fields: vec![field1, field2],
            interfaces: Vec::new(),
            methods: Vec::new(),
            inline_impls: Vec::new(),
            span: Span::new(0, 20),
        };

        assert_eq!(struct_def.name.0, "Point");
        assert_eq!(struct_def.fields.len(), 2);
        assert_eq!(struct_def.fields[0].name.0, "x");
        match &struct_def.fields[1].ty {
            TypeExpr::Path { segments, .. } => assert_eq!(segments[0], "i32"),
            _ => panic!("expected path type"),
        }
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

    #[test]
    fn type_is_ref_immutable() {
        use super::types::{Mutability, Type};

        let ref_type = Type::Ref(Box::new(Type::I32), Mutability::Immutable);
        assert!(ref_type.is_ref());
        assert!(!ref_type.is_mut_ref());
    }

    #[test]
    fn type_is_ref_mutable() {
        use super::types::{Mutability, Type};

        let ref_type = Type::Ref(Box::new(Type::I32), Mutability::Mutable);
        assert!(ref_type.is_ref());
        assert!(ref_type.is_mut_ref());
    }

    #[test]
    fn type_inner_type() {
        use super::types::{Mutability, Type};

        let ref_type = Type::Ref(Box::new(Type::I32), Mutability::Immutable);
        assert_eq!(ref_type.inner_type(), Some(&Type::I32));

        let plain_type = Type::I32;
        assert_eq!(plain_type.inner_type(), None);
    }

    #[test]
    fn type_nested_references() {
        use super::types::{Mutability, Type};

        // &mut &i32
        let nested = Type::Ref(
            Box::new(Type::Ref(Box::new(Type::I32), Mutability::Immutable)),
            Mutability::Mutable,
        );

        assert!(nested.is_ref());
        assert!(nested.is_mut_ref());

        let inner = nested.inner_type().unwrap();
        assert!(inner.is_ref());
        assert!(!inner.is_mut_ref());
    }

    #[test]
    fn array_type_construction() {
        use super::types::Type;

        let arr_ty = Type::Array(Box::new(Type::I32), 10);
        assert!(arr_ty.is_array());
        assert_eq!(arr_ty.array_size(), Some(10));

        let elem_ty = arr_ty.array_element_type().unwrap();
        assert_eq!(*elem_ty, Type::I32);
    }

    #[test]
    fn array_helper_methods() {
        use super::types::Type;

        let arr_ty = Type::Array(Box::new(Type::Bool), 5);

        assert!(arr_ty.is_array());
        assert!(!arr_ty.is_int());
        assert!(!arr_ty.is_ref());

        assert_eq!(arr_ty.array_size(), Some(5));
        assert_eq!(arr_ty.array_element_type(), Some(&Type::Bool));

        // Non-array types should return None
        let i32_ty = Type::I32;
        assert!(!i32_ty.is_array());
        assert_eq!(i32_ty.array_size(), None);
        assert_eq!(i32_ty.array_element_type(), None);
    }

    #[test]
    fn nested_array_types() {
        use super::types::Type;

        // [[i32; 3]; 2] - 2D array
        let inner_arr = Type::Array(Box::new(Type::I32), 3);
        let outer_arr = Type::Array(Box::new(inner_arr.clone()), 2);

        assert!(outer_arr.is_array());
        assert_eq!(outer_arr.array_size(), Some(2));

        let inner = outer_arr.array_element_type().unwrap();
        assert!(inner.is_array());
        assert_eq!(inner.array_size(), Some(3));
        assert_eq!(inner.array_element_type(), Some(&Type::I32));
    }
}

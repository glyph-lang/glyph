use super::*;

impl CodegenContext {
    pub(super) fn coerce_int_binop(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
    ) -> (LLVMValueRef, LLVMValueRef) {
        unsafe {
            let lhs_ty = LLVMTypeOf(lhs);
            let rhs_ty = LLVMTypeOf(rhs);

            if LLVMGetTypeKind(lhs_ty) != llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind {
                return (lhs, rhs);
            }
            if LLVMGetTypeKind(rhs_ty) != llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind {
                return (lhs, rhs);
            }

            let lw = LLVMGetIntTypeWidth(lhs_ty);
            let rw = LLVMGetIntTypeWidth(rhs_ty);
            if lw == rw {
                return (lhs, rhs);
            }

            let name = CString::new("int.coerce").unwrap();

            if lw > rw {
                let rhs2 = LLVMBuildSExt(self.builder, rhs, lhs_ty, name.as_ptr());
                (lhs, rhs2)
            } else {
                let lhs2 = LLVMBuildSExt(self.builder, lhs, rhs_ty, name.as_ptr());
                (lhs2, rhs)
            }
        }
    }

    pub(super) fn coerce_int_value(
        &mut self,
        val: LLVMValueRef,
        target_ty: LLVMTypeRef,
        signed: bool,
    ) -> LLVMValueRef {
        unsafe {
            let val_ty = LLVMTypeOf(val);
            if val_ty == target_ty {
                return val;
            }
            if LLVMGetTypeKind(val_ty) != llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind {
                return val;
            }
            if LLVMGetTypeKind(target_ty) != llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind {
                return val;
            }

            let val_bits = LLVMGetIntTypeWidth(val_ty);
            let target_bits = LLVMGetIntTypeWidth(target_ty);
            let name = CString::new("int.cast").unwrap();
            if target_bits > val_bits {
                if signed {
                    LLVMBuildSExt(self.builder, val, target_ty, name.as_ptr())
                } else {
                    LLVMBuildZExt(self.builder, val, target_ty, name.as_ptr())
                }
            } else {
                LLVMBuildTrunc(self.builder, val, target_ty, name.as_ptr())
            }
        }
    }

    pub(super) fn rvalue_tag(&self, rvalue: &Rvalue) -> &'static str {
        match rvalue {
            Rvalue::ConstInt(_) => "ConstInt",
            Rvalue::ConstBool(_) => "ConstBool",
            Rvalue::StringLit { .. } => "StringLit",
            Rvalue::Move(_) => "Move",
            Rvalue::Binary { .. } => "Binary",
            Rvalue::StructLit { .. } => "StructLit",
            Rvalue::FieldAccess { .. } => "FieldAccess",
            Rvalue::EnumConstruct { .. } => "EnumConstruct",
            Rvalue::EnumTag { .. } => "EnumTag",
            Rvalue::EnumPayload { .. } => "EnumPayload",
            Rvalue::FileOpen { .. } => "FileOpen",
            Rvalue::FileReadToString { .. } => "FileReadToString",
            Rvalue::FileWriteString { .. } => "FileWriteString",
            Rvalue::FileClose { .. } => "FileClose",
            Rvalue::StringLen { .. } => "StringLen",
            Rvalue::StringConcat { .. } => "StringConcat",
            Rvalue::StringSlice { .. } => "StringSlice",
            Rvalue::StringTrim { .. } => "StringTrim",
            Rvalue::StringSplit { .. } => "StringSplit",
            Rvalue::StringStartsWith { .. } => "StringStartsWith",
            Rvalue::StringEndsWith { .. } => "StringEndsWith",
            Rvalue::StringClone { .. } => "StringClone",
            Rvalue::Call { .. } => "Call",
            Rvalue::Ref { .. } => "Ref",
            Rvalue::ArrayLit { .. } => "ArrayLit",
            Rvalue::ArrayIndex { .. } => "ArrayIndex",
            Rvalue::ArrayLen { .. } => "ArrayLen",
            Rvalue::VecNew { .. } => "VecNew",
            Rvalue::VecWithCapacity { .. } => "VecWithCapacity",
            Rvalue::VecLen { .. } => "VecLen",
            Rvalue::VecIndex { .. } => "VecIndex",
            Rvalue::VecPush { .. } => "VecPush",
            Rvalue::VecPop { .. } => "VecPop",
            Rvalue::MapNew { .. } => "MapNew",
            Rvalue::MapWithCapacity { .. } => "MapWithCapacity",
            Rvalue::MapAdd { .. } => "MapAdd",
            Rvalue::MapUpdate { .. } => "MapUpdate",
            Rvalue::MapDel { .. } => "MapDel",
            Rvalue::MapGet { .. } => "MapGet",
            Rvalue::MapHas { .. } => "MapHas",
            Rvalue::MapKeys { .. } => "MapKeys",
            Rvalue::MapVals { .. } => "MapVals",
            Rvalue::OwnNew { .. } => "OwnNew",
            Rvalue::OwnIntoRaw { .. } => "OwnIntoRaw",
            Rvalue::OwnFromRaw { .. } => "OwnFromRaw",
            Rvalue::RawPtrNull { .. } => "RawPtrNull",
            Rvalue::SharedNew { .. } => "SharedNew",
            Rvalue::SharedClone { .. } => "SharedClone",
        }
    }

    pub(super) fn mir_value_type(&self, value: &MirValue, func: &MirFunction) -> Option<Type> {
        match value {
            MirValue::Unit => Some(Type::Void),
            MirValue::Int(_) => Some(Type::I32),
            MirValue::Bool(_) => Some(Type::Bool),
            MirValue::Local(id) => func
                .locals
                .get(id.0 as usize)
                .and_then(|local| local.ty.clone()),
        }
    }

    pub(super) fn call_param_types(
        &self,
        name: &str,
        mir_module: &MirModule,
    ) -> (Vec<Option<Type>>, bool) {
        if let Some(func) = mir_module.functions.iter().find(|f| f.name == name) {
            let params = func
                .params
                .iter()
                .map(|id| func.locals.get(id.0 as usize).and_then(|l| l.ty.clone()))
                .collect();
            return (params, false);
        }

        if let Some(ext) = mir_module.extern_functions.iter().find(|f| f.name == name) {
            let params = ext.params.iter().cloned().map(Some).collect();
            return (params, true);
        }

        (Vec::new(), false)
    }

    pub(super) fn codegen_rvalue(
        &mut self,
        rvalue: &Rvalue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        unsafe {
            match rvalue {
                Rvalue::ConstInt(i) => {
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, *i as u64, 1))
                }
                Rvalue::ConstBool(b) => {
                    let ty = LLVMInt1TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, if *b { 1 } else { 0 }, 0))
                }
                Rvalue::StringLit {
                    content,
                    global_name,
                } => self.codegen_string_literal(content, global_name),
                Rvalue::Move(local_id) => {
                    let local_ptr = local_map
                        .get(local_id)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local_id))?;

                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        let llvm_ty = self.local_llvm_type(func, *local_id)?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            llvm_ty,
                            *local_ptr,
                            load_name.as_ptr(),
                        ))
                    } else {
                        Ok(*local_ptr)
                    }
                }
                Rvalue::Binary { op, lhs, rhs } => {
                    use glyph_core::ast::BinaryOp;
                    let name = CString::new("binop")?;

                    let lhs_ty = self.mir_value_type(lhs, func);
                    let rhs_ty = self.mir_value_type(rhs, func);
                    let is_string_like = |ty: Option<&Type>| {
                        matches!(ty, Some(Type::Str | Type::String))
                            || matches!(
                                ty,
                                Some(Type::Ref(inner, _))
                                    if matches!(inner.as_ref(), Type::Str | Type::String)
                            )
                    };

                    if matches!(op, BinaryOp::Eq | BinaryOp::Ne)
                        && is_string_like(lhs_ty.as_ref())
                        && is_string_like(rhs_ty.as_ref())
                    {
                        let lhs_val = match lhs {
                            MirValue::Local(id) => {
                                self.codegen_string_base_value(*id, func, local_map)?
                            }
                            _ => self.codegen_value(lhs, func, local_map)?,
                        };
                        let rhs_val = match rhs {
                            MirValue::Local(id) => {
                                self.codegen_string_base_value(*id, func, local_map)?
                            }
                            _ => self.codegen_value(rhs, func, local_map)?,
                        };
                        let eq_val = self.codegen_string_eq(lhs_val, rhs_val, functions)?;
                        if matches!(op, BinaryOp::Ne) {
                            let one = LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0);
                            return Ok(LLVMBuildXor(
                                self.builder,
                                eq_val,
                                one,
                                CString::new("str.ne")?.as_ptr(),
                            ));
                        }
                        return Ok(eq_val);
                    }

                    let lhs_val0 = self.codegen_value(lhs, func, local_map)?;
                    let rhs_val0 = self.codegen_value(rhs, func, local_map)?;

                    // Integer literals in MIR are currently untyped and codegen as i32.
                    // Coerce integer widths so operations like `usize + 1` work.
                    let (lhs_val, rhs_val) = self.coerce_int_binop(lhs_val0, rhs_val0);

                    let result = match op {
                        BinaryOp::Add => {
                            LLVMBuildAdd(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Sub => {
                            LLVMBuildSub(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Mul => {
                            LLVMBuildMul(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Div => {
                            LLVMBuildSDiv(self.builder, lhs_val, rhs_val, name.as_ptr())
                        }
                        BinaryOp::Eq => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Ne => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntNE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Lt => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Le => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Gt => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::Ge => LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                            lhs_val,
                            rhs_val,
                            name.as_ptr(),
                        ),
                        BinaryOp::And | BinaryOp::Or => {
                            return Err(anyhow!("logical ops should be lowered in MIR"));
                        }
                    };
                    Ok(result)
                }
                Rvalue::StructLit {
                    struct_name,
                    field_values,
                } => self.codegen_struct_literal(struct_name, field_values, func, local_map),
                Rvalue::FieldAccess {
                    base,
                    field_name: _,
                    field_index,
                } => self.codegen_field_access(*base, *field_index as usize, func, local_map),
                Rvalue::EnumConstruct {
                    enum_name,
                    variant_index,
                    payload,
                } => {
                    let llvm_enum = self.get_enum_type(enum_name)?;
                    let layout = self
                        .enum_layouts
                        .get(enum_name)
                        .ok_or_else(|| anyhow!("missing enum layout for {}", enum_name))?;

                    let alloca_name = CString::new(format!("{}_tmp", enum_name))?;
                    let alloca = LLVMBuildAlloca(self.builder, llvm_enum, alloca_name.as_ptr());

                    // Store tag at field 0
                    let tag_ptr = LLVMBuildStructGEP2(
                        self.builder,
                        llvm_enum,
                        alloca,
                        0,
                        CString::new("enum.tag").unwrap().as_ptr(),
                    );
                    let tag_val = LLVMConstInt(
                        LLVMInt32TypeInContext(self.context),
                        *variant_index as u64,
                        0,
                    );
                    LLVMBuildStore(self.builder, tag_val, tag_ptr);

                    // Store payload into its slot (index offset by 1)
                    let field_index = 1 + *variant_index;
                    let field_ptr = LLVMBuildStructGEP2(
                        self.builder,
                        llvm_enum,
                        alloca,
                        field_index,
                        CString::new("enum.payload").unwrap().as_ptr(),
                    );

                    let variant_ty = layout
                        .variants
                        .get(*variant_index as usize)
                        .ok_or_else(|| anyhow!("invalid variant index for {}", enum_name))?
                        .payload
                        .clone();

                    if let Some(val) = payload {
                        let llvm_val = self.codegen_value(val, func, local_map)?;
                        LLVMBuildStore(self.builder, llvm_val, field_ptr);
                    } else {
                        let placeholder_ty = match variant_ty.as_ref() {
                            Some(Type::Void) | None => LLVMInt8TypeInContext(self.context),
                            Some(ty) => self.get_llvm_type(ty)?,
                        };
                        let zero = match variant_ty.as_ref() {
                            Some(Type::Void) | None => LLVMConstInt(placeholder_ty, 0, 0),
                            Some(ty) => {
                                let llvm_ty = self.get_llvm_type(ty)?;
                                LLVMConstNull(llvm_ty)
                            }
                        };
                        LLVMBuildStore(self.builder, zero, field_ptr);
                    }

                    let load_name = CString::new(format!("{}_val", enum_name))?;
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        llvm_enum,
                        alloca,
                        load_name.as_ptr(),
                    ))
                }
                Rvalue::EnumTag { base } => {
                    let (enum_name, enum_ptr) =
                        self.enum_pointer_for_local(*base, func, local_map)?;
                    let llvm_enum = self.get_enum_type(&enum_name)?;
                    let tag_ptr = LLVMBuildStructGEP2(
                        self.builder,
                        llvm_enum,
                        enum_ptr,
                        0,
                        CString::new("enum.tag.ptr")?.as_ptr(),
                    );
                    let tag_ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        tag_ty,
                        tag_ptr,
                        CString::new("enum.tag.load")?.as_ptr(),
                    ))
                }
                Rvalue::EnumPayload {
                    base,
                    variant_index,
                    payload_type,
                } => {
                    let (enum_name, enum_ptr) =
                        self.enum_pointer_for_local(*base, func, local_map)?;
                    let llvm_enum = self.get_enum_type(&enum_name)?;
                    let payload_ptr = LLVMBuildStructGEP2(
                        self.builder,
                        llvm_enum,
                        enum_ptr,
                        1 + *variant_index,
                        CString::new("enum.payload.ptr")?.as_ptr(),
                    );
                    let llvm_payload_ty = match payload_type {
                        Type::Void => LLVMInt8TypeInContext(self.context),
                        _ => self.get_llvm_type(payload_type)?,
                    };
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        llvm_payload_ty,
                        payload_ptr,
                        CString::new("enum.payload")?.as_ptr(),
                    ))
                }
                Rvalue::FileOpen { path, create } => {
                    self.codegen_file_open(path, *create, func, local_map, functions, mir_module)
                }
                Rvalue::FileReadToString { file } => {
                    self.codegen_file_read_to_string(*file, func, local_map, functions, mir_module)
                }
                Rvalue::FileWriteString { file, contents } => self.codegen_file_write_string(
                    *file, contents, func, local_map, functions, mir_module,
                ),
                Rvalue::FileClose { file } => {
                    self.codegen_file_close(*file, func, local_map, functions, mir_module)
                }
                Rvalue::StringLen { base } => {
                    self.codegen_string_len(*base, func, local_map, functions)
                }
                Rvalue::StringConcat { base, value } => {
                    self.codegen_string_concat(*base, value, func, local_map, functions)
                }
                Rvalue::StringSlice { base, start, len } => {
                    self.codegen_string_slice(*base, start, len, func, local_map, functions)
                }
                Rvalue::StringTrim { base } => {
                    self.codegen_string_trim(*base, func, local_map, functions)
                }
                Rvalue::StringSplit { base, sep } => {
                    self.codegen_string_split(*base, sep, func, local_map, functions)
                }
                Rvalue::StringStartsWith { base, needle } => {
                    self.codegen_string_starts_with(*base, needle, func, local_map, functions)
                }
                Rvalue::StringEndsWith { base, needle } => {
                    self.codegen_string_ends_with(*base, needle, func, local_map, functions)
                }
                Rvalue::StringClone { base } => {
                    self.codegen_string_clone(*base, func, local_map, functions)
                }
                Rvalue::Call { name, args } => {
                    if name == "argv" || name == "std::sys::argv" {
                        return self.codegen_sys_argv_value();
                    }

                    // Some low-level libc helpers may be referenced by stdlib lowering even
                    // when they aren't explicitly declared in the MIR module.
                    let callee = if let Some(callee) = functions.get(name).copied() {
                        callee
                    } else {
                        match name.as_str() {
                            "malloc" => self.ensure_malloc_fn()?,
                            "free" => self.ensure_free_fn()?,
                            "strdup" => self.ensure_strdup_fn()?,
                            "strlen" | "memcpy" | "memcmp" | "strstr" | "isspace" => {
                                self.get_or_declare_extern_function(name)?
                            }
                            _ => return Err(anyhow!("unknown function {}", name)),
                        }
                    };

                    // Find the MIR function to get its type signature
                    let fn_ty = if let Some(target_func) =
                        mir_module.functions.iter().find(|f| f.name == *name)
                    {
                        self.llvm_function_type(target_func)?
                    } else if let Some(extern_func) =
                        mir_module.extern_functions.iter().find(|f| f.name == *name)
                    {
                        self.llvm_extern_function_type(extern_func)?
                    } else {
                        match name.as_str() {
                            "malloc" => self.malloc_function_type(),
                            "free" => self.free_function_type(),
                            "strdup" => self.strdup_function_type(),
                            "strlen" | "memcpy" | "memcmp" | "strstr" | "isspace" => {
                                self.function_type_for(name)?
                            }
                            _ => bail!("function {} not found in MIR module", name),
                        }
                    };

                    let (mut param_types, mut is_extern) = self.call_param_types(name, mir_module);
                    if param_types.is_empty() {
                        match name.as_str() {
                            "malloc" => {
                                // usize -> ptr
                                param_types = vec![Some(Type::Usize)];
                                is_extern = true;
                            }
                            "free" => {
                                // ptr -> void
                                param_types = vec![Some(Type::RawPtr(Box::new(Type::I8)))];
                                is_extern = true;
                            }
                            "strdup" => {
                                // str -> String
                                param_types = vec![Some(Type::Str)];
                                is_extern = true;
                            }
                            _ => {}
                        }
                    }

                    // Codegen arguments
                    let mut llvm_args: Vec<LLVMValueRef> = Vec::new();
                    for (idx, arg) in args.iter().enumerate() {
                        let mut arg_val = self.codegen_value(arg, func, local_map)?;

                        if let Some(Some(param_ty)) = param_types.get(idx) {
                            let arg_ty = self.mir_value_type(arg, func);

                            if let Some(Type::Ref(inner, _)) = arg_ty.as_ref() {
                                if inner.as_ref() == param_ty {
                                    let inner_llvm_ty = self.get_llvm_type(param_ty)?;
                                    arg_val = LLVMBuildLoad2(
                                        self.builder,
                                        inner_llvm_ty,
                                        arg_val,
                                        CString::new("arg.deref")?.as_ptr(),
                                    );
                                }
                            }

                            if let Type::Ref(inner, _) = param_ty {
                                if arg_ty.as_ref() == Some(inner.as_ref()) {
                                    let inner_llvm_ty = self.get_llvm_type(inner)?;
                                    let slot = LLVMBuildAlloca(
                                        self.builder,
                                        inner_llvm_ty,
                                        CString::new("arg.addr")?.as_ptr(),
                                    );
                                    LLVMBuildStore(self.builder, arg_val, slot);
                                    arg_val = slot;
                                }
                            }

                            if is_extern {
                                if let Some(arg_ty) = arg_ty.as_ref() {
                                    if (matches!(arg_ty, Type::Str)
                                        && (matches!(param_ty, Type::RawPtr(_))
                                            || matches!(param_ty, Type::Str)))
                                        || (matches!(arg_ty, Type::String)
                                            && (matches!(param_ty, Type::RawPtr(_))
                                                || matches!(param_ty, Type::Str)
                                                || matches!(param_ty, Type::String)))
                                    {
                                        let expected_ty = self.get_llvm_type(param_ty)?;
                                        if LLVMTypeOf(arg_val) != expected_ty {
                                            let cast_name = CString::new("str.as_ptr")?;
                                            arg_val = LLVMBuildBitCast(
                                                self.builder,
                                                arg_val,
                                                expected_ty,
                                                cast_name.as_ptr(),
                                            );
                                        }
                                    }
                                }
                            }

                            let expected_ty = self.get_llvm_type(param_ty)?;
                            let arg_ty = LLVMTypeOf(arg_val);
                            let expected_kind = LLVMGetTypeKind(expected_ty);
                            let arg_kind = LLVMGetTypeKind(arg_ty);
                            if expected_kind == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind
                                && arg_kind == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind
                            {
                                let signed = matches!(param_ty, Type::I8 | Type::I32 | Type::I64);
                                arg_val = self.coerce_int_value(arg_val, expected_ty, signed);
                            } else if expected_kind == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                                && arg_kind == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                                && arg_ty != expected_ty
                            {
                                let cast_name = CString::new("arg.ptr.cast")?;
                                arg_val = LLVMBuildBitCast(
                                    self.builder,
                                    arg_val,
                                    expected_ty,
                                    cast_name.as_ptr(),
                                );
                            }
                        }

                        llvm_args.push(arg_val);
                    }

                    self.build_call2(fn_ty, callee, &mut llvm_args, "call")
                }
                Rvalue::Ref { base, .. } => {
                    let base_ptr = local_map
                        .get(base)
                        .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
                    Ok(*base_ptr)
                }
                Rvalue::ArrayLit {
                    elem_type,
                    elements,
                } => self.codegen_array_literal(elem_type, elements, func, local_map),
                Rvalue::ArrayIndex {
                    base,
                    index,
                    bounds_check,
                } => self.codegen_array_index(*base, index, *bounds_check, func, local_map),
                Rvalue::ArrayLen { base } => self.codegen_array_len(*base, func),
                Rvalue::VecNew { elem_type } | Rvalue::VecWithCapacity { elem_type, .. } => {
                    // Allocate Vec struct and zero/init fields; capacity handled below.
                    let vec_struct =
                        self.codegen_vec_struct_init(func, local_map, elem_type, rvalue)?;
                    Ok(vec_struct)
                }
                Rvalue::VecLen { vec } => self.codegen_vec_len(*vec, func, local_map),
                Rvalue::VecIndex {
                    vec,
                    elem_type,
                    index,
                    bounds_check,
                } => self.codegen_vec_index(*vec, elem_type, index, *bounds_check, func, local_map),
                Rvalue::VecPush {
                    vec,
                    elem_type,
                    value,
                } => self.codegen_vec_push(*vec, elem_type, value, func, local_map),
                Rvalue::VecPop { vec, elem_type } => {
                    self.codegen_vec_pop(*vec, elem_type, func, local_map, mir_module)
                }
                Rvalue::MapNew {
                    key_type,
                    value_type,
                }
                | Rvalue::MapWithCapacity {
                    key_type,
                    value_type,
                    ..
                } => self.codegen_map_init(key_type, value_type, rvalue, func, local_map),
                Rvalue::MapAdd {
                    map,
                    key_type,
                    key,
                    value_type,
                    value,
                } => self.codegen_map_add(
                    *map, key_type, key, value_type, value, func, local_map, functions, mir_module,
                ),
                Rvalue::MapUpdate {
                    map,
                    key_type,
                    key,
                    value_type,
                    value,
                } => self.codegen_map_update(
                    *map, key_type, key, value_type, value, func, local_map, functions, mir_module,
                ),
                Rvalue::MapDel {
                    map,
                    key_type,
                    value_type,
                    key,
                } => self.codegen_map_del(
                    *map, key_type, value_type, key, func, local_map, functions, mir_module,
                ),
                Rvalue::MapGet {
                    map,
                    key_type,
                    value_type,
                    key,
                } => self.codegen_map_get(
                    *map, key_type, value_type, key, func, local_map, functions, mir_module,
                ),
                Rvalue::MapHas { map, key_type, key } => self
                    .codegen_map_has(*map, key_type, key, func, local_map, functions, mir_module),
                Rvalue::MapKeys {
                    map,
                    key_type,
                    value_type,
                } => self.codegen_map_keys(
                    *map, key_type, value_type, func, local_map, functions, mir_module,
                ),
                Rvalue::MapVals {
                    map,
                    key_type,
                    value_type,
                } => self.codegen_map_vals(
                    *map, key_type, value_type, func, local_map, functions, mir_module,
                ),
                Rvalue::OwnNew { elem_type, value } => {
                    self.codegen_own_new(elem_type, value, func, local_map)
                }
                Rvalue::OwnIntoRaw { base, elem_type } => {
                    self.codegen_own_into_raw(*base, elem_type, local_map)
                }
                Rvalue::OwnFromRaw { ptr, elem_type } => {
                    self.codegen_own_from_raw(ptr, elem_type, func, local_map)
                }
                Rvalue::RawPtrNull { elem_type } => self.codegen_raw_ptr_null(elem_type),
                Rvalue::SharedNew { elem_type, value } => {
                    self.codegen_shared_new(elem_type, value, func, local_map)
                }
                Rvalue::SharedClone { base, elem_type } => {
                    self.codegen_shared_clone(*base, elem_type, local_map)
                }
            }
        }
    }

    pub(super) fn codegen_value(
        &mut self,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            match value {
                MirValue::Unit => {
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, 0, 0))
                }
                MirValue::Int(i) => {
                    let ty = LLVMInt32TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, *i as u64, 1))
                }
                MirValue::Bool(b) => {
                    let ty = LLVMInt1TypeInContext(self.context);
                    Ok(LLVMConstInt(ty, if *b { 1 } else { 0 }, 0))
                }
                MirValue::Local(local_id) => {
                    let local_ptr = local_map
                        .get(local_id)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local_id))?;

                    if LLVMGetTypeKind(LLVMTypeOf(*local_ptr))
                        == llvm_sys::LLVMTypeKind::LLVMPointerTypeKind
                    {
                        let load_name = CString::new("load")?;
                        let llvm_ty = self.local_llvm_type(func, *local_id)?;
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            llvm_ty,
                            *local_ptr,
                            load_name.as_ptr(),
                        ))
                    } else {
                        Ok(*local_ptr)
                    }
                }
            }
        }
    }

    pub(super) fn local_llvm_type(
        &self,
        func: &MirFunction,
        local_id: LocalId,
    ) -> Result<LLVMTypeRef> {
        let local = func
            .locals
            .get(local_id.0 as usize)
            .ok_or_else(|| anyhow!("missing local {:?}", local_id))?;
        if let Some(ty) = local.ty.as_ref() {
            if matches!(ty, Type::Void) {
                return Ok(unsafe { LLVMInt8TypeInContext(self.context) });
            }
            self.get_llvm_type(ty)
        } else {
            Ok(unsafe { LLVMInt32TypeInContext(self.context) })
        }
    }
}

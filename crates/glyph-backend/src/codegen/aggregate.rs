use super::*;

impl CodegenContext {
    pub(super) fn codegen_option_none(
        &self,
        elem_type: &Type,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let option_name = format!("Option${}", self.type_key(elem_type));
        self.codegen_enum_value(&option_name, 0, None)
    }

    pub(super) fn codegen_option_some(
        &self,
        elem_type: &Type,
        payload_val: LLVMValueRef,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let option_name = format!("Option${}", self.type_key(elem_type));
        self.codegen_enum_value(&option_name, 1, Some(payload_val))
    }

    pub(super) fn codegen_enum_value(
        &self,
        enum_name: &str,
        variant_index: u32,
        payload: Option<LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let llvm_enum = self.get_enum_type(enum_name)?;
        let layout = self
            .enum_layouts
            .get(enum_name)
            .ok_or_else(|| anyhow!("missing enum layout for {}", enum_name))?;

        unsafe {
            let alloca_name = CString::new(format!("{}_tmp", enum_name))?;
            let alloca = LLVMBuildAlloca(self.builder, llvm_enum, alloca_name.as_ptr());
            let tag_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_enum,
                alloca,
                0,
                CString::new("enum.tag").unwrap().as_ptr(),
            );
            let tag_val = LLVMConstInt(
                LLVMInt32TypeInContext(self.context),
                variant_index as u64,
                0,
            );
            LLVMBuildStore(self.builder, tag_val, tag_ptr);

            let field_index = 1 + variant_index;
            let field_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_enum,
                alloca,
                field_index,
                CString::new("enum.payload").unwrap().as_ptr(),
            );

            let variant_ty = layout
                .variants
                .get(variant_index as usize)
                .ok_or_else(|| anyhow!("invalid variant index for {}", enum_name))?
                .payload
                .clone();

            if let Some(val) = payload {
                LLVMBuildStore(self.builder, val, field_ptr);
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
    }

    pub(super) fn codegen_err_value(&mut self, message: &str) -> Result<LLVMValueRef> {
        let err_name = "Err";
        let llvm_err = self.get_struct_type(err_name)?;
        let global_name = format!("map_err_{}", self.sanitize(message));
        let msg_val = self.codegen_string_literal(message, &global_name)?;

        unsafe {
            let alloca_name = CString::new("err.tmp")?;
            let alloca = LLVMBuildAlloca(self.builder, llvm_err, alloca_name.as_ptr());
            let msg_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_err,
                alloca,
                0,
                CString::new("err.msg")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, msg_val, msg_ptr);
            let load_name = CString::new("err.val")?;
            Ok(LLVMBuildLoad2(
                self.builder,
                llvm_err,
                alloca,
                load_name.as_ptr(),
            ))
        }
    }

    pub(super) fn codegen_result_ok(
        &self,
        ok_type: &Type,
        err_type: &Type,
        payload: Option<LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let enum_name = format!(
            "Result${}__{}",
            self.type_key(ok_type),
            self.type_key(err_type)
        );
        self.codegen_enum_value(&enum_name, 0, payload)
    }

    pub(super) fn codegen_result_err(
        &self,
        ok_type: &Type,
        err_type: &Type,
        payload: Option<LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let enum_name = format!(
            "Result${}__{}",
            self.type_key(ok_type),
            self.type_key(err_type)
        );
        self.codegen_enum_value(&enum_name, 1, payload)
    }

    pub(super) fn codegen_struct_literal(
        &mut self,
        struct_name: &str,
        field_values: &[(String, MirValue)],
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let llvm_struct = self.get_struct_type(struct_name)?;
        let layout = self
            .struct_layouts
            .get(struct_name)
            .ok_or_else(|| anyhow!("missing layout for struct {}", struct_name))?;
        let fields = layout.fields.clone();

        let alloca_name = CString::new(format!("{}_tmp", struct_name))?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_struct, alloca_name.as_ptr()) };

        let mut value_map = HashMap::new();
        for (name, value) in field_values {
            value_map.insert(name.as_str(), value);
        }

        for (idx, (field_name, _)) in fields.iter().enumerate() {
            let value = value_map.get(field_name.as_str()).ok_or_else(|| {
                anyhow!("missing field {} for struct {}", field_name, struct_name)
            })?;
            let llvm_field_val = self.codegen_value(value, func, local_map)?;
            let gep_name = CString::new(format!("{}.{}", struct_name, field_name))?;
            let field_ptr = unsafe {
                LLVMBuildStructGEP2(
                    self.builder,
                    llvm_struct,
                    alloca,
                    idx as u32,
                    gep_name.as_ptr(),
                )
            };
            unsafe {
                LLVMBuildStore(self.builder, llvm_field_val, field_ptr);
            }
        }

        let load_name = CString::new(format!("{}_val", struct_name))?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_struct, alloca, load_name.as_ptr()) })
    }

    pub(super) fn codegen_field_access(
        &mut self,
        base: LocalId,
        field_index: usize,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, struct_ptr) = self.struct_pointer_for_local(base, func, local_map)?;
        let llvm_struct = self.get_struct_type(struct_name.as_str())?;
        let field_ty = {
            let layout = self
                .struct_layouts
                .get(&struct_name)
                .ok_or_else(|| anyhow!("missing layout for struct {}", struct_name))?;
            layout
                .fields
                .get(field_index)
                .map(|(_, ty)| ty.clone())
                .ok_or_else(|| anyhow!("invalid field index {} for {}", field_index, struct_name))?
        };

        let gep_name = CString::new(format!("{}.field{}", struct_name, field_index))?;
        let field_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_struct,
                struct_ptr,
                field_index as u32,
                gep_name.as_ptr(),
            )
        };
        let llvm_field_ty = self.get_llvm_type(&field_ty)?;
        let load_name = CString::new("field.load")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_field_ty, field_ptr, load_name.as_ptr()) })
    }

    pub(super) fn codegen_field_ref(
        &mut self,
        base: LocalId,
        field_index: usize,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, struct_ptr) = self.struct_pointer_for_local(base, func, local_map)?;
        let llvm_struct = self.get_struct_type(struct_name.as_str())?;

        let gep_name = CString::new(format!("{}.field{}", struct_name, field_index))?;
        let field_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_struct,
                struct_ptr,
                field_index as u32,
                gep_name.as_ptr(),
            )
        };
        Ok(field_ptr)
    }

    pub(super) fn struct_pointer_for_local(
        &mut self,
        local: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<(String, LLVMValueRef)> {
        let mut ty = func
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.clone())
            .ok_or_else(|| anyhow!("field access base has unknown type"))?;
        let mut ptr = *local_map
            .get(&local)
            .ok_or_else(|| anyhow!("undefined local {:?}", local))?;

        loop {
            match ty.clone() {
                Type::Named(name) => return Ok((name, ptr)),
                Type::Tuple(elem_types) => {
                    let name = tuple_struct_name_codegen(&elem_types);
                    return Ok((name, ptr));
                }
                Type::Ref(inner, mutability) => {
                    let ref_ty = Type::Ref(inner.clone(), mutability);
                    let load_ty = self.get_llvm_type(&ref_ty)?;
                    let load_name = CString::new("deref.struct")?;
                    ptr = unsafe { LLVMBuildLoad2(self.builder, load_ty, ptr, load_name.as_ptr()) };
                    ty = *inner;
                }
                _ => bail!("field access base is not a struct"),
            }
        }
    }

    pub(super) fn enum_pointer_for_local(
        &mut self,
        local: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<(String, LLVMValueRef)> {
        let mut ty = func
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.clone())
            .ok_or_else(|| anyhow!("enum base has unknown type"))?;
        let mut ptr = *local_map
            .get(&local)
            .ok_or_else(|| anyhow!("undefined local {:?}", local))?;

        loop {
            match ty.clone() {
                Type::Enum(name) => return Ok((name, ptr)),
                Type::Ref(inner, mutability) => {
                    let ref_ty = Type::Ref(inner.clone(), mutability);
                    let load_ty = self.get_llvm_type(&ref_ty)?;
                    let load_name = CString::new("deref.enum")?;
                    ptr = unsafe { LLVMBuildLoad2(self.builder, load_ty, ptr, load_name.as_ptr()) };
                    ty = *inner;
                }
                _ => bail!("base is not an enum"),
            }
        }
    }
}

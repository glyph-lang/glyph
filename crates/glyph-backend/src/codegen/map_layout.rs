use super::*;

impl CodegenContext {
    pub(super) fn map_bucket_name(&self, key_type: &Type, value_type: &Type) -> String {
        format!(
            "MapBucket${}__{}",
            self.type_key(key_type),
            self.type_key(value_type)
        )
    }

    /// Ensures the bucket type for the given key/value types is registered and fully defined.
    /// This must be called before any operations that query bucket type layout (GEP, loads, etc.)
    /// to avoid LLVM hanging on getABITypeAlign queries for opaque struct types.
    pub(super) fn ensure_map_bucket_type(
        &mut self,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<()> {
        let bucket_name = self.map_bucket_name(key_type, value_type);

        let llvm_bucket_ty = if let Some(&existing) = self.struct_types.get(&bucket_name) {
            let is_opaque = unsafe { LLVMIsOpaqueStruct(existing) != 0 };
            if !is_opaque {
                if !self.struct_layouts.contains_key(&bucket_name) {
                    let layout = StructType {
                        name: bucket_name.clone(),
                        fields: vec![
                            ("key".to_string(), key_type.clone()),
                            ("value".to_string(), value_type.clone()),
                            (
                                "next".to_string(),
                                Type::RawPtr(Box::new(Type::Named(bucket_name.clone()))),
                            ),
                        ],
                    };
                    self.struct_layouts.insert(bucket_name, layout);
                }
                return Ok(());
            }
            existing
        } else {
            let bucket_name_c = CString::new(bucket_name.as_str())?;
            let llvm_bucket_ty =
                unsafe { LLVMStructCreateNamed(self.context, bucket_name_c.as_ptr()) };
            self.struct_types
                .insert(bucket_name.clone(), llvm_bucket_ty);
            llvm_bucket_ty
        };

        // Build the field types: [key, value, next]
        let key_llvm_ty = self.get_llvm_type(key_type)?;
        let value_llvm_ty = self.get_llvm_type(value_type)?;
        let next_ptr_ty = unsafe { LLVMPointerType(llvm_bucket_ty, 0) };

        let mut field_tys = vec![key_llvm_ty, value_llvm_ty, next_ptr_ty];

        // Set the struct body
        unsafe {
            LLVMStructSetBody(
                llvm_bucket_ty,
                field_tys.as_mut_ptr(),
                field_tys.len() as u32,
                0,
            );
        }

        // Create a layout for tracking (matches the field order)
        let layout = StructType {
            name: bucket_name.clone(),
            fields: vec![
                ("key".to_string(), key_type.clone()),
                ("value".to_string(), value_type.clone()),
                (
                    "next".to_string(),
                    Type::RawPtr(Box::new(Type::Named(bucket_name.clone()))),
                ),
            ],
        };
        self.struct_layouts.insert(bucket_name, layout);

        Ok(())
    }

    /// Ensures bucket type from map struct name when value_type is not directly available.
    /// Extracts bucket type from map's struct layout.
    pub(super) fn ensure_map_bucket_type_from_map(
        &mut self,
        map_struct_name: &str,
        key_type: &Type,
    ) -> Result<()> {
        let bucket_name = self.map_bucket_name_from_map(map_struct_name)?;

        if let Some(&existing) = self.struct_types.get(&bucket_name) {
            let is_opaque = unsafe { LLVMIsOpaqueStruct(existing) != 0 };
            if !is_opaque {
                return Ok(());
            }
        }

        // Extract value_type from bucket's layout if it exists, otherwise parse from name
        // Bucket name format: "MapBucket$KeyType__ValueType"
        // For now, we'll look it up in existing layouts or create a minimal version
        // In practice, the bucket should already be in the map's layout from MIR
        let value_type_opt = self
            .struct_layouts
            .get(&bucket_name)
            .and_then(|layout| layout.fields.get(1))
            .map(|(_, ty)| ty.clone());

        if let Some(value_type) = value_type_opt {
            return self.ensure_map_bucket_type(key_type, &value_type);
        }

        bail!("Cannot determine value type for bucket {}", bucket_name)
    }

    pub(super) fn map_bucket_name_from_map(&self, map_name: &str) -> Result<String> {
        let layout = self
            .struct_layouts
            .get(map_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", map_name))?;
        let bucket_ty = layout
            .fields
            .first()
            .map(|(_, ty)| ty)
            .ok_or_else(|| anyhow!("map layout missing buckets field"))?;
        match bucket_ty {
            Type::RawPtr(inner) => match inner.as_ref() {
                Type::RawPtr(inner2) => match inner2.as_ref() {
                    Type::Named(name) => Ok(name.clone()),
                    _ => bail!("unexpected buckets inner type"),
                },
                _ => bail!("unexpected buckets field type"),
            },
            _ => bail!("unexpected buckets field type"),
        }
    }

    pub(super) fn codegen_map_allocate_buckets(
        &mut self,
        buckets_ptr: LLVMValueRef,
        cap_ptr: LLVMValueRef,
        cap_val: LLVMValueRef,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<()> {
        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;

        let elem_size = unsafe { LLVMSizeOf(bucket_head_llvm_ty) };
        let alloc_size = unsafe {
            LLVMBuildMul(
                self.builder,
                cap_val,
                elem_size,
                CString::new("map.cap.alloc.size")?.as_ptr(),
            )
        };
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let mut malloc_args = vec![alloc_size];
        let raw_ptr = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("map.cap.malloc")?.as_ptr(),
            )
        };
        let bucket_array = unsafe {
            LLVMBuildBitCast(
                self.builder,
                raw_ptr,
                bucket_array_llvm_ty,
                CString::new("map.cap.cast")?.as_ptr(),
            )
        };

        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let i8_ptr_ty = unsafe { LLVMPointerType(i8_ty, 0) };
        let memset_ptr = unsafe {
            LLVMBuildBitCast(
                self.builder,
                bucket_array,
                i8_ptr_ty,
                CString::new("map.cap.memset.ptr")?.as_ptr(),
            )
        };
        let zero_byte = unsafe { LLVMConstInt(i8_ty, 0, 0) };
        unsafe {
            LLVMBuildMemSet(self.builder, memset_ptr, zero_byte, alloc_size, 0);
        }

        unsafe { LLVMBuildStore(self.builder, bucket_array, buckets_ptr) };
        unsafe { LLVMBuildStore(self.builder, cap_val, cap_ptr) };

        Ok(())
    }
}

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::path::Path;

use anyhow::{Result, anyhow, bail};
use glyph_core::mir::{
    BlockId, LocalId, MirBlock, MirExternFunction, MirFunction, MirInst, MirModule, MirValue,
    Rvalue,
};
use glyph_core::types::{EnumType, Mutability, StructType, Type};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMLinkage, LLVMModule};

pub struct CodegenContext {
    context: *mut LLVMContext,
    module: *mut LLVMModule,
    builder: *mut LLVMBuilder,
    struct_types: HashMap<String, LLVMTypeRef>,
    struct_layouts: HashMap<String, StructType>,
    enum_types: HashMap<String, LLVMTypeRef>,
    enum_layouts: HashMap<String, EnumType>,
    malloc_fn: Option<LLVMValueRef>,
    free_fn: Option<LLVMValueRef>,
    string_globals: HashMap<String, LLVMValueRef>,
}

impl CodegenContext {
    pub fn new(module_name: &str) -> Result<Self> {
        unsafe {
            let context = LLVMContextCreate();
            let module_name_c = CString::new(module_name)?;
            let module = LLVMModuleCreateWithNameInContext(module_name_c.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            Ok(Self {
                context,
                module,
                builder,
                struct_types: HashMap::new(),
                struct_layouts: HashMap::new(),
                enum_types: HashMap::new(),
                enum_layouts: HashMap::new(),
                malloc_fn: None,
                free_fn: None,
                string_globals: HashMap::new(),
            })
        }
    }

    pub fn get_module(&self) -> *mut LLVMModule {
        self.module
    }

    pub fn dump_ir(&self) -> String {
        unsafe {
            let ir_ptr = LLVMPrintModuleToString(self.module);
            let ir_cstr = CStr::from_ptr(ir_ptr);
            let ir_string = ir_cstr.to_string_lossy().into_owned();
            LLVMDisposeMessage(ir_ptr);
            ir_string
        }
    }

    fn create_named_types(&mut self, mir_module: &MirModule) -> Result<()> {
        for (name, layout) in &mir_module.struct_types {
            let name_c = CString::new(name.as_str())?;
            let llvm_ty = unsafe { LLVMStructCreateNamed(self.context, name_c.as_ptr()) };
            self.struct_types.insert(name.clone(), llvm_ty);
            self.struct_layouts.insert(name.clone(), layout.clone());
        }

        for (name, layout) in &mir_module.enum_types {
            let name_c = CString::new(name.as_str())?;
            let llvm_ty = unsafe { LLVMStructCreateNamed(self.context, name_c.as_ptr()) };
            self.enum_types.insert(name.clone(), llvm_ty);
            self.enum_layouts.insert(name.clone(), layout.clone());
        }

        Ok(())
    }

    fn register_struct_types(&mut self, mir_module: &MirModule) -> Result<()> {
        for (name, layout) in &mir_module.struct_types {
            let llvm_ty = *self
                .struct_types
                .get(name)
                .ok_or_else(|| anyhow!("missing llvm type for struct {}", name))?;

            let mut field_tys: Vec<LLVMTypeRef> = Vec::new();
            for (_, field_ty) in &layout.fields {
                field_tys.push(self.get_llvm_type(field_ty)?);
            }

            unsafe {
                LLVMStructSetBody(llvm_ty, field_tys.as_mut_ptr(), field_tys.len() as u32, 0);
            }
        }

        Ok(())
    }

    fn get_vec_layout(&self, struct_name: &str) -> Option<(LLVMTypeRef, LLVMTypeRef)> {
        // Vec$T layout expected: [ptr, len, cap] where len/cap are usize (i64 backend)
        let llvm_ty = *self.struct_types.get(struct_name)?;
        let layout = self.struct_layouts.get(struct_name)?;
        let len_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        if layout.fields.len() == 3 {
            Some((llvm_ty, len_ty))
        } else {
            None
        }
    }

    fn sanitize(&self, name: &str) -> String {
        name.chars()
            .map(|c| if c.is_alphanumeric() { c } else { '_' })
            .collect()
    }

    fn type_key(&self, ty: &Type) -> String {
        match ty {
            Type::I8 => "i8".into(),
            Type::I32 => "i32".into(),
            Type::I64 => "i64".into(),
            Type::U8 => "u8".into(),
            Type::U32 => "u32".into(),
            Type::U64 => "u64".into(),
            Type::Usize => "usize".into(),
            Type::F32 => "f32".into(),
            Type::F64 => "f64".into(),
            Type::Bool => "bool".into(),
            Type::Char => "char".into(),
            Type::Str => "str".into(),
            Type::String => "String".into(),
            Type::Void => "void".into(),
            Type::Named(n) => self.sanitize(n),
            Type::Enum(n) => format!("enum_{}", self.sanitize(n)),
            Type::Param(p) => format!("P_{}", self.sanitize(p)),
            Type::Ref(inner, m) => {
                let m = if matches!(m, Mutability::Mutable) {
                    "mut"
                } else {
                    "ref"
                };
                format!("{}_{}", m, self.type_key(inner))
            }
            Type::Array(inner, size) => format!("arr{}_{}", size, self.type_key(inner)),
            Type::Own(inner) => format!("own_{}", self.type_key(inner)),
            Type::RawPtr(inner) => format!("rawptr_{}", self.type_key(inner)),
            Type::Shared(inner) => format!("shared_{}", self.type_key(inner)),
            Type::App { base, args } => {
                let args: Vec<String> = args.iter().map(|a| self.type_key(a)).collect();
                format!("app_{}_{}", self.sanitize(base), args.join("__"))
            }
        }
    }

    fn ensure_usize(&self, val: LLVMValueRef, usize_ty: LLVMTypeRef) -> Result<LLVMValueRef> {
        unsafe {
            if LLVMTypeOf(val) == usize_ty {
                Ok(val)
            } else {
                let cast_name = CString::new("usize.cast")?;
                Ok(LLVMBuildSExt(
                    self.builder,
                    val,
                    usize_ty,
                    cast_name.as_ptr(),
                ))
            }
        }
    }

    fn codegen_option_none(
        &self,
        elem_type: &Type,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let option_name = format!("Option${}", self.type_key(elem_type));
        let llvm_option = self.get_enum_type(&option_name)?;
        let layout = self
            .enum_layouts
            .get(&option_name)
            .ok_or_else(|| anyhow!("missing option layout for {}", option_name))?;
        if layout.variants.len() < 1 {
            bail!("option layout missing variants");
        }

        unsafe {
            let alloca_name = CString::new("option.none.tmp")?;
            let alloca = LLVMBuildAlloca(self.builder, llvm_option, alloca_name.as_ptr());
            let tag_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_option,
                alloca,
                0,
                CString::new("option.tag")?.as_ptr(),
            );
            let tag_val = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
            LLVMBuildStore(self.builder, tag_val, tag_ptr);
            let load_name = CString::new("option.none.val")?;
            Ok(LLVMBuildLoad2(
                self.builder,
                llvm_option,
                alloca,
                load_name.as_ptr(),
            ))
        }
    }

    fn codegen_option_some(
        &self,
        elem_type: &Type,
        payload_val: LLVMValueRef,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let option_name = format!("Option${}", self.type_key(elem_type));
        let llvm_option = self.get_enum_type(&option_name)?;
        let layout = self
            .enum_layouts
            .get(&option_name)
            .ok_or_else(|| anyhow!("missing option layout for {}", option_name))?;
        if layout.variants.len() < 2 {
            bail!("option layout missing Some variant");
        }

        unsafe {
            let alloca_name = CString::new("option.some.tmp")?;
            let alloca = LLVMBuildAlloca(self.builder, llvm_option, alloca_name.as_ptr());
            // tag at index 0
            let tag_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_option,
                alloca,
                0,
                CString::new("option.tag")?.as_ptr(),
            );
            let tag_val = LLVMConstInt(LLVMInt32TypeInContext(self.context), 1, 0);
            LLVMBuildStore(self.builder, tag_val, tag_ptr);

            // payload at index 1
            let payload_ptr = LLVMBuildStructGEP2(
                self.builder,
                llvm_option,
                alloca,
                1,
                CString::new("option.payload")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, payload_val, payload_ptr);

            let load_name = CString::new("option.some.val")?;
            Ok(LLVMBuildLoad2(
                self.builder,
                llvm_option,
                alloca,
                load_name.as_ptr(),
            ))
        }
    }

    fn register_enum_types(&mut self, mir_module: &MirModule) -> Result<()> {
        for (name, layout) in &mir_module.enum_types {
            let llvm_ty = *self
                .enum_types
                .get(name)
                .ok_or_else(|| anyhow!("missing llvm type for enum {}", name))?;

            let mut field_tys: Vec<LLVMTypeRef> = Vec::new();
            // tag
            field_tys.push(unsafe { LLVMInt32TypeInContext(self.context) });
            for variant in &layout.variants {
                let ty = if let Some(payload) = &variant.payload {
                    self.get_llvm_type(payload)?
                } else {
                    unsafe { LLVMInt8TypeInContext(self.context) }
                };
                field_tys.push(ty);
            }

            unsafe {
                LLVMStructSetBody(llvm_ty, field_tys.as_mut_ptr(), field_tys.len() as u32, 0);
            }
        }

        Ok(())
    }

    fn get_struct_type(&self, name: &str) -> Result<LLVMTypeRef> {
        self.struct_types
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("unknown struct type {}", name))
    }

    fn get_enum_type(&self, name: &str) -> Result<LLVMTypeRef> {
        self.enum_types
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("unknown enum type {}", name))
    }

    fn get_llvm_type(&self, ty: &Type) -> Result<LLVMTypeRef> {
        let ty = ty.clone();
        unsafe {
            Ok(match ty {
                Type::I8 => LLVMInt8TypeInContext(self.context),
                Type::I32 => LLVMInt32TypeInContext(self.context),
                Type::I64 => LLVMInt64TypeInContext(self.context),
                Type::U8 => LLVMInt8TypeInContext(self.context),
                Type::U32 => LLVMInt32TypeInContext(self.context),
                Type::U64 => LLVMInt64TypeInContext(self.context),
                Type::Usize => LLVMInt64TypeInContext(self.context),
                Type::F32 => LLVMFloatTypeInContext(self.context),
                Type::F64 => LLVMDoubleTypeInContext(self.context),
                Type::Bool => LLVMInt1TypeInContext(self.context),
                Type::Char => LLVMInt32TypeInContext(self.context),
                Type::Str => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::String => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Named(name) => self.get_struct_type(&name)?,
                Type::Enum(name) => self.get_enum_type(&name)?,
                Type::Ref(inner, _) => {
                    let inner_ty = self.get_llvm_type(&inner)?;
                    LLVMPointerType(inner_ty, 0)
                }
                Type::Array(elem_ty, size) => {
                    let elem_llvm = self.get_llvm_type(&elem_ty)?;
                    LLVMArrayType(elem_llvm, size as u32)
                }
                Type::Own(inner) | Type::RawPtr(inner) | Type::Shared(inner) => {
                    let elem_ty = self.get_llvm_type(&inner)?;
                    LLVMPointerType(elem_ty, 0)
                }
                Type::Param(_) | Type::App { .. } => {
                    anyhow::bail!("generic types must be monomorphized before codegen")
                }
            })
        }
    }

    pub fn codegen_module(&mut self, mir_module: &MirModule) -> Result<()> {
        self.create_named_types(mir_module)?;
        self.register_struct_types(mir_module)?;
        self.register_enum_types(mir_module)?;

        // Declare all functions up-front so calls can reference any order.
        let functions = self.declare_functions(mir_module)?;

        for func in &mir_module.functions {
            let llvm_func = *functions
                .get(&func.name)
                .ok_or_else(|| anyhow!("missing declared function {}", func.name))?;
            self.codegen_function_body(func, llvm_func, &functions, mir_module)?;
        }

        Ok(())
    }

    fn declare_functions(
        &mut self,
        mir_module: &MirModule,
    ) -> Result<HashMap<String, LLVMValueRef>> {
        let mut functions = HashMap::new();

        for func in &mir_module.functions {
            let func_type = self.llvm_function_type(func)?;
            let func_name = CString::new(func.name.as_str())?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            functions.insert(func.name.clone(), llvm_func);
        }

        for func in &mir_module.extern_functions {
            let func_type = self.llvm_extern_function_type(func)?;
            let symbol_name = func.link_name.as_ref().unwrap_or(&func.name);
            let func_name = CString::new(symbol_name.as_str())?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            unsafe { LLVMSetLinkage(llvm_func, LLVMLinkage::LLVMExternalLinkage) };
            // ABI mapping (v0: only "C" supported; default LLVM CC is C).
            if let Some(abi) = &func.abi {
                if abi != "C" {
                    bail!("unsupported ABI '{}': only \"C\" is supported", abi);
                }
                // If additional ABIs are added, map them via LLVMSetFunctionCallConv here.
            }
            functions.insert(func.name.clone(), llvm_func);
        }

        Ok(functions)
    }

    fn llvm_function_type(&self, func: &MirFunction) -> Result<LLVMTypeRef> {
        // Determine return type
        let ret_type = func
            .ret_type
            .as_ref()
            .map(|t| self.get_llvm_type(t))
            .transpose()?
            .unwrap_or_else(|| unsafe { LLVMVoidTypeInContext(self.context) });

        // Build parameter types
        let mut param_types: Vec<LLVMTypeRef> = Vec::new();
        for &param_id in &func.params {
            let local = &func.locals[param_id.0 as usize];
            let param_ty = local
                .ty
                .as_ref()
                .map(|t| self.get_llvm_type(t))
                .transpose()?
                .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });
            param_types.push(param_ty);
        }

        Ok(unsafe {
            LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not variadic
            )
        })
    }

    fn llvm_extern_function_type(&self, func: &MirExternFunction) -> Result<LLVMTypeRef> {
        let ret_type = func
            .ret_type
            .as_ref()
            .map(|t| self.get_llvm_type(t))
            .transpose()?
            .unwrap_or_else(|| unsafe { LLVMVoidTypeInContext(self.context) });

        let mut param_types: Vec<LLVMTypeRef> = Vec::new();
        for param_ty in &func.params {
            param_types.push(self.get_llvm_type(param_ty)?);
        }

        Ok(unsafe {
            LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not variadic in v0
            )
        })
    }

    fn codegen_function_body(
        &mut self,
        func: &MirFunction,
        llvm_func: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        // Create basic blocks
        let mut bb_map: HashMap<BlockId, LLVMBasicBlockRef> = HashMap::new();
        for (i, _) in func.blocks.iter().enumerate() {
            let bb_name = CString::new(format!("bb{}", i))?;
            let bb =
                unsafe { LLVMAppendBasicBlockInContext(self.context, llvm_func, bb_name.as_ptr()) };
            bb_map.insert(BlockId(i as u32), bb);
        }

        // Create local allocas
        let mut local_map: HashMap<LocalId, LLVMValueRef> = HashMap::new();

        // Set up entry block for allocas
        if let Some(&entry_bb) = bb_map.get(&BlockId(0)) {
            unsafe { LLVMPositionBuilderAtEnd(self.builder, entry_bb) };

            for (i, local) in func.locals.iter().enumerate() {
                let local_id = LocalId(i as u32);

                let local_ty = local
                    .ty
                    .as_ref()
                    .map(|t| self.get_llvm_type(t))
                    .transpose()?
                    .unwrap_or_else(|| unsafe { LLVMInt32TypeInContext(self.context) });

                let local_name = local.name.clone().unwrap_or_else(|| format!("tmp{}", i));
                let local_name = CString::new(local_name)?;

                let alloca =
                    unsafe { LLVMBuildAlloca(self.builder, local_ty, local_name.as_ptr()) };
                local_map.insert(local_id, alloca);
            }

            for (i, &param_id) in func.params.iter().enumerate() {
                let param_val = unsafe { LLVMGetParam(llvm_func, i as u32) };
                let slot = local_map
                    .get(&param_id)
                    .ok_or_else(|| anyhow!("missing storage for param {:?}", param_id))?;
                unsafe {
                    LLVMBuildStore(self.builder, param_val, *slot);
                }
            }
        }

        // Codegen each basic block
        for (i, block) in func.blocks.iter().enumerate() {
            let bb = bb_map.get(&BlockId(i as u32)).unwrap();
            unsafe { LLVMPositionBuilderAtEnd(self.builder, *bb) };
            self.codegen_block(func, block, &local_map, &bb_map, functions, mir_module)?;
        }

        Ok(())
    }

    fn codegen_block(
        &mut self,
        func: &MirFunction,
        block: &MirBlock,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        for inst in &block.insts {
            self.codegen_inst(func, inst, local_map, bb_map, functions, mir_module)?;
        }
        Ok(())
    }

    fn codegen_inst(
        &mut self,
        func: &MirFunction,
        inst: &MirInst,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        bb_map: &HashMap<BlockId, LLVMBasicBlockRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<()> {
        unsafe {
            match inst {
                MirInst::Assign { local, value } => {
                    let val = self.codegen_rvalue(value, func, local_map, functions, mir_module)?;
                    let local_ptr = local_map
                        .get(local)
                        .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                    LLVMBuildStore(self.builder, val, *local_ptr);
                }
                MirInst::Return(val) => {
                    if let Some(v) = val {
                        let ret_val = self.codegen_value(v, func, local_map)?;
                        LLVMBuildRet(self.builder, ret_val);
                    } else {
                        LLVMBuildRetVoid(self.builder);
                    }
                }
                MirInst::Goto(target) => {
                    let target_bb = bb_map
                        .get(target)
                        .ok_or_else(|| anyhow!("undefined block {:?}", target))?;
                    LLVMBuildBr(self.builder, *target_bb);
                }
                MirInst::If {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let cond_val = self.codegen_value(cond, func, local_map)?;
                    let then_block = bb_map
                        .get(then_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", then_bb))?;
                    let else_block = bb_map
                        .get(else_bb)
                        .ok_or_else(|| anyhow!("undefined block {:?}", else_bb))?;
                    LLVMBuildCondBr(self.builder, cond_val, *then_block, *else_block);
                }
                MirInst::Drop(local) => {
                    self.codegen_drop_local(*local, func, local_map)?;
                }
                MirInst::Nop => {}
            }
        }
        Ok(())
    }

    fn codegen_drop_local(
        &mut self,
        local: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<()> {
        let ty = match func
            .locals
            .get(local.0 as usize)
            .and_then(|l| l.ty.as_ref())
        {
            Some(ty) => ty,
            None => return Ok(()),
        };
        match ty {
            Type::Own(inner) => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_own_slot(*slot, inner)
            }
            Type::Shared(inner) => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_shared_slot(*slot, inner)
            }
            Type::String => {
                let slot = local_map
                    .get(&local)
                    .ok_or_else(|| anyhow!("undefined local {:?}", local))?;
                self.codegen_drop_string_slot(*slot)
            }
            Type::App { base, args } if base == "Vec" => {
                let elem = args.get(0).cloned().unwrap_or(Type::I32);
                self.codegen_drop_vec(local, &elem, func, local_map)
            }
            _ => Ok(()),
        }
    }

    fn codegen_drop_own_slot(&mut self, slot: LLVMValueRef, inner: &Type) -> Result<()> {
        unsafe {
            let ptr_ty = self.get_llvm_type(&Type::Own(Box::new(inner.clone())))?;
            let load_name = CString::new("own.ptr")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let cmp_name = CString::new("own.isnull")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                cmp_name.as_ptr(),
            );

            let current_bb = LLVMGetInsertBlock(self.builder);
            if current_bb.is_null() {
                bail!("builder not positioned in a block for drop");
            }
            let parent_func = LLVMGetBasicBlockParent(current_bb);
            if parent_func.is_null() {
                bail!("drop parent function missing");
            }

            let drop_name = CString::new("own.drop")?;
            let cont_name = CString::new("own.drop.cont")?;
            let drop_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, drop_name.as_ptr());
            let cont_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, cont_name.as_ptr());

            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            self.codegen_free(ptr_val)?;
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
        }
        Ok(())
    }

    fn codegen_drop_shared_slot(&mut self, slot: LLVMValueRef, elem_type: &Type) -> Result<()> {
        unsafe {
            // 1. Load pointer
            let ptr_ty = self.get_llvm_type(&Type::Shared(Box::new(elem_type.clone())))?;
            let load_name = CString::new("shared.drop.load")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());

            // 2. Null check (pointer might already be consumed)
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let is_null_name = CString::new("shared.drop.is_null")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                is_null_name.as_ptr(),
            );

            // 3. Create basic blocks
            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let dec_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.dec")?.as_ptr(),
            );
            let free_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.free")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("shared.cont")?.as_ptr(),
            );

            // If null, skip to continue
            LLVMBuildCondBr(self.builder, is_null, cont_bb, dec_bb);

            // 4. Decrement block
            LLVMPositionBuilderAtEnd(self.builder, dec_bb);

            // Get struct type
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(self.context, field_tys.as_mut_ptr(), 2, 0);

            // Get refcount pointer
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                ptr_val,
                0,
                CString::new("shared.rc.ptr")?.as_ptr(),
            );

            // Load old refcount
            let old_count = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                refcount_ptr,
                CString::new("shared.rc.old")?.as_ptr(),
            );

            // Decrement refcount
            let one = LLVMConstInt(usize_ty, 1, 0);
            let new_count = LLVMBuildSub(
                self.builder,
                old_count,
                one,
                CString::new("shared.rc.new")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, new_count, refcount_ptr);

            // Check if refcount reached zero
            let zero = LLVMConstInt(usize_ty, 0, 0);
            let is_zero = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                new_count,
                zero,
                CString::new("shared.rc.is_zero")?.as_ptr(),
            );

            // If zero, free; else continue
            LLVMBuildCondBr(self.builder, is_zero, free_bb, cont_bb);

            // 5. Free block (refcount is zero)
            LLVMPositionBuilderAtEnd(self.builder, free_bb);
            self.codegen_free(ptr_val)?;

            // Set slot to null
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            // 6. Continue block
            LLVMPositionBuilderAtEnd(self.builder, cont_bb);

            Ok(())
        }
    }

    fn codegen_drop_string_slot(&mut self, slot: LLVMValueRef) -> Result<()> {
        unsafe {
            let ptr_ty = self.get_llvm_type(&Type::String)?;
            let load_name = CString::new("string.ptr")?;
            let ptr_val = LLVMBuildLoad2(self.builder, ptr_ty, slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(ptr_ty);
            let cmp_name = CString::new("string.isnull")?;
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr_val,
                null_ptr,
                cmp_name.as_ptr(),
            );

            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let drop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("string.drop")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("string.drop.cont")?.as_ptr(),
            );

            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            self.codegen_free(ptr_val)?;
            LLVMBuildStore(self.builder, null_ptr, slot);
            LLVMBuildBr(self.builder, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
        }
        Ok(())
    }

    fn codegen_string_literal(&mut self, content: &str, global_name: &str) -> Result<LLVMValueRef> {
        if let Some(&ptr) = self.string_globals.get(global_name) {
            return Ok(ptr);
        }

        unsafe {
            // Null-terminated bytes for C interop
            let mut bytes: Vec<u8> = content.bytes().collect();
            bytes.push(0);

            let i8_ty = LLVMInt8TypeInContext(self.context);
            let array_ty = LLVMArrayType(i8_ty, bytes.len() as u32);

            let name_c = CString::new(global_name)?;
            let global = LLVMAddGlobal(self.module, array_ty, name_c.as_ptr());
            LLVMSetGlobalConstant(global, 1);
            LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);

            let mut const_bytes: Vec<LLVMValueRef> = bytes
                .iter()
                .map(|b| LLVMConstInt(i8_ty, *b as u64, 0))
                .collect();
            let init = LLVMConstArray(i8_ty, const_bytes.as_mut_ptr(), const_bytes.len() as u32);
            LLVMSetInitializer(global, init);

            // Pointer to first character
            let i32_ty = LLVMInt32TypeInContext(self.context);
            let zero = LLVMConstInt(i32_ty, 0, 0);
            let mut indices = vec![zero, zero];
            let ptr = LLVMConstGEP2(array_ty, global, indices.as_mut_ptr(), indices.len() as u32);

            self.string_globals.insert(global_name.to_string(), ptr);
            Ok(ptr)
        }
    }

    fn mir_value_type(&self, value: &MirValue, func: &MirFunction) -> Option<Type> {
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

    fn call_param_types(&self, name: &str, mir_module: &MirModule) -> (Vec<Option<Type>>, bool) {
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

    fn codegen_rvalue(
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
                    let lhs_val = self.codegen_value(lhs, func, local_map)?;
                    let rhs_val = self.codegen_value(rhs, func, local_map)?;
                    let name = CString::new("binop")?;

                    use glyph_core::ast::BinaryOp;
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
                        let placeholder_ty = if let Some(ty) = variant_ty.as_ref() {
                            self.get_llvm_type(ty)?
                        } else {
                            LLVMInt8TypeInContext(self.context)
                        };
                        let zero = if let Some(ty) = variant_ty.as_ref() {
                            let llvm_ty = self.get_llvm_type(ty)?;
                            LLVMConstNull(llvm_ty)
                        } else {
                            LLVMConstInt(placeholder_ty, 0, 0)
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
                    let llvm_payload_ty = self.get_llvm_type(payload_type)?;
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        llvm_payload_ty,
                        payload_ptr,
                        CString::new("enum.payload")?.as_ptr(),
                    ))
                }
                Rvalue::Call { name, args } => {
                    let callee = functions
                        .get(name)
                        .copied()
                        .ok_or_else(|| anyhow!("unknown function {}", name))?;

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
                        bail!("function {} not found in MIR module", name);
                    };

                    let (param_types, is_extern) = self.call_param_types(name, mir_module);

                    // Codegen arguments
                    let mut llvm_args: Vec<LLVMValueRef> = Vec::new();
                    for (idx, arg) in args.iter().enumerate() {
                        let mut arg_val = self.codegen_value(arg, func, local_map)?;

                        if let Some(Some(param_ty)) = param_types.get(idx) {
                            if is_extern {
                                if let Some(arg_ty) = self.mir_value_type(arg, func) {
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
                        }

                        llvm_args.push(arg_val);
                    }

                    let call_name = CString::new("call")?;
                    Ok(LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        callee,
                        llvm_args.as_mut_ptr(),
                        llvm_args.len() as u32,
                        call_name.as_ptr(),
                    ))
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

    fn codegen_value(
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

    fn local_llvm_type(&self, func: &MirFunction, local_id: LocalId) -> Result<LLVMTypeRef> {
        let local = func
            .locals
            .get(local_id.0 as usize)
            .ok_or_else(|| anyhow!("missing local {:?}", local_id))?;
        if let Some(ty) = local.ty.as_ref() {
            self.get_llvm_type(ty)
        } else {
            Ok(unsafe { LLVMInt32TypeInContext(self.context) })
        }
    }

    fn codegen_struct_literal(
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

    fn codegen_field_access(
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

    fn codegen_array_literal(
        &mut self,
        elem_type: &Type,
        elements: &[MirValue],
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        if elements.is_empty() {
            bail!("array literal must have at least one element");
        }

        let array_ty = Type::Array(Box::new(elem_type.clone()), elements.len());
        let llvm_array_ty = self.get_llvm_type(&array_ty)?;
        let alloca_name = CString::new("array.tmp")?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_array_ty, alloca_name.as_ptr()) };

        let i32_ty = unsafe { LLVMInt32TypeInContext(self.context) };
        let zero = unsafe { LLVMConstInt(i32_ty, 0, 0) };

        for (idx, element) in elements.iter().enumerate() {
            let elem_val = self.codegen_value(element, func, local_map)?;
            let index_const = unsafe { LLVMConstInt(i32_ty, idx as u64, 0) };
            let mut indices = vec![zero, index_const];
            let gep_name = CString::new(format!("array.elem{}", idx))?;
            let elem_ptr = unsafe {
                LLVMBuildInBoundsGEP2(
                    self.builder,
                    llvm_array_ty,
                    alloca,
                    indices.as_mut_ptr(),
                    indices.len() as u32,
                    gep_name.as_ptr(),
                )
            };
            unsafe {
                LLVMBuildStore(self.builder, elem_val, elem_ptr);
            }
        }

        let load_name = CString::new("array.value")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_array_ty, alloca, load_name.as_ptr()) })
    }

    fn codegen_array_index(
        &mut self,
        base: LocalId,
        index: &MirValue,
        bounds_check: bool,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_ptr = *local_map
            .get(&base)
            .ok_or_else(|| anyhow!("undefined local {:?}", base))?;

        let base_ty = func
            .locals
            .get(base.0 as usize)
            .and_then(|local| local.ty.as_ref())
            .ok_or_else(|| anyhow!("array index base has unknown type"))?;

        let (elem_ty, size) = match base_ty {
            Type::Array(elem, size) => (elem.as_ref().clone(), *size),
            _ => bail!("array index base is not an array"),
        };

        let llvm_array_ty = self.get_llvm_type(base_ty)?;
        let elem_llvm_ty = self.get_llvm_type(&elem_ty)?;
        let index_val = self.codegen_value(index, func, local_map)?;

        if bounds_check {
            self.emit_bounds_check(index_val, size)?;
        }

        let i32_ty = unsafe { LLVMInt32TypeInContext(self.context) };
        let zero = unsafe { LLVMConstInt(i32_ty, 0, 0) };
        let mut indices = vec![zero, index_val];
        let gep_name = CString::new("array.index")?;
        let elem_ptr = unsafe {
            LLVMBuildInBoundsGEP2(
                self.builder,
                llvm_array_ty,
                base_ptr,
                indices.as_mut_ptr(),
                indices.len() as u32,
                gep_name.as_ptr(),
            )
        };
        let load_name = CString::new("array.elem.load")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, elem_llvm_ty, elem_ptr, load_name.as_ptr()) })
    }

    fn codegen_vec_struct_init(
        &mut self,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        elem_type: &Type,
        rvalue: &Rvalue,
    ) -> Result<LLVMValueRef> {
        let inst_name = format!("Vec${}", self.type_key(elem_type));
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&inst_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", inst_name))?;

        let alloca_name = CString::new("vec.tmp")?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_vec_ty, alloca_name.as_ptr()) };

        // Field 0: data ptr (RawPtr<T>)
        let data_field_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                alloca,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };
        let elem_ptr_ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;

        // Field 1: len
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                alloca,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        unsafe { LLVMBuildStore(self.builder, zero, len_ptr) };

        // Field 2: cap
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                alloca,
                2,
                CString::new("vec.cap").unwrap().as_ptr(),
            )
        };
        let cap_val = match rvalue {
            Rvalue::VecWithCapacity { capacity, .. } => {
                self.codegen_value(capacity, func, local_map)?
            }
            _ => zero,
        };
        let cap_val = self.ensure_usize(cap_val, usize_ty)?;
        unsafe { LLVMBuildStore(self.builder, cap_val, cap_ptr) };

        // Field 0: data ptr (RawPtr<T>)
        let data_field_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                alloca,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };

        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_size = unsafe { LLVMSizeOf(elem_llvm_ty) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("vec.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let alloc_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.cap.alloc")?.as_ptr(),
            )
        };
        let zero_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.cap.zero")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.cap.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, zero_bb, alloc_bb) };

        // alloc path (cap > 0)
        unsafe { LLVMPositionBuilderAtEnd(self.builder, alloc_bb) };
        let alloc_size = unsafe {
            LLVMBuildMul(
                self.builder,
                cap_val,
                elem_size,
                CString::new("vec.cap.alloc.size")?.as_ptr(),
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
                CString::new("vec.cap.malloc")?.as_ptr(),
            )
        };
        let data_alloc = unsafe {
            LLVMBuildBitCast(
                self.builder,
                raw_ptr,
                elem_ptr_ty,
                CString::new("vec.cap.cast")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, data_alloc, data_field_ptr) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        // zero-cap path
        unsafe { LLVMPositionBuilderAtEnd(self.builder, zero_bb) };
        let null_data = unsafe { LLVMConstNull(elem_ptr_ty) };
        unsafe { LLVMBuildStore(self.builder, null_data, data_field_ptr) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        // done
        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };

        let load_name = CString::new("vec.value")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_vec_ty, alloca, load_name.as_ptr()) })
    }

    fn codegen_vec_len(
        &mut self,
        vec: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, vec_ptr) = self.struct_pointer_for_local(vec, func, local_map)?;
        let (llvm_vec_ty, _) = self
            .get_vec_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", struct_name))?;
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        Ok(unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
            )
        })
    }

    fn codegen_vec_index(
        &mut self,
        vec: LocalId,
        elem_type: &Type,
        index: &MirValue,
        bounds_check: bool,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, vec_ptr) = self.struct_pointer_for_local(vec, func, local_map)?;
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", struct_name))?;

        let index_val = self.codegen_value(index, func, local_map)?;

        // Load len
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };
        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
            )
        };

        let idx_usize = self.ensure_usize(index_val, usize_ty)?;

        if bounds_check {
            self.emit_vec_bounds_check(idx_usize, len_val, usize_ty)?;
        }

        // data pointer
        let data_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };
        let raw_ptr_ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;
        let data_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                raw_ptr_ty,
                data_ptr,
                CString::new("vec.data.load")?.as_ptr(),
            )
        };

        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_llvm_ty,
                data_val,
                vec![idx_usize].as_mut_ptr(),
                1,
                CString::new("vec.elem.ptr")?.as_ptr(),
            )
        };

        Ok(unsafe {
            LLVMBuildLoad2(
                self.builder,
                elem_llvm_ty,
                elem_ptr,
                CString::new("vec.elem.load")?.as_ptr(),
            )
        })
    }

    fn codegen_vec_push(
        &mut self,
        vec: LocalId,
        elem_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let (struct_name, vec_ptr) = self.struct_pointer_for_local(vec, func, local_map)?;
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", struct_name))?;

        // Field pointers
        let data_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                2,
                CString::new("vec.cap").unwrap().as_ptr(),
            )
        };

        let current_len = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
            )
        };
        let current_cap = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("vec.cap.load")?.as_ptr(),
            )
        };

        // Branch if growth needed
        let cmp_name = CString::new("vec.need_grow")?;
        let need_grow = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_len,
                current_cap,
                cmp_name.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        if parent_bb.is_null() {
            bail!("builder not positioned in block for vec push");
        }
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        if parent_fn.is_null() {
            bail!("vec push parent function missing");
        }

        let grow_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.grow")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.push.cont")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildCondBr(self.builder, need_grow, grow_bb, cont_bb) };

        // ---- Grow block ----
        unsafe { LLVMPositionBuilderAtEnd(self.builder, grow_bb) };
        let one = unsafe { LLVMConstInt(usize_ty, 1, 0) };
        let two = unsafe { LLVMConstInt(usize_ty, 2, 0) };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let is_zero_cap = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_cap,
                zero,
                CString::new("vec.cap.zero")?.as_ptr(),
            )
        };
        let doubled = unsafe {
            LLVMBuildMul(
                self.builder,
                current_cap,
                two,
                CString::new("vec.cap.double")?.as_ptr(),
            )
        };
        let new_cap = unsafe {
            LLVMBuildSelect(
                self.builder,
                is_zero_cap,
                one,
                doubled,
                CString::new("vec.cap.new")?.as_ptr(),
            )
        };

        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_size = unsafe { LLVMSizeOf(elem_llvm_ty) };
        let alloc_size = unsafe {
            LLVMBuildMul(
                self.builder,
                new_cap,
                elem_size,
                CString::new("vec.alloc.size")?.as_ptr(),
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
                CString::new("vec.grow.malloc")?.as_ptr(),
            )
        };
        let new_data_ptr = unsafe {
            LLVMBuildBitCast(
                self.builder,
                raw_ptr,
                self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?,
                CString::new("vec.grow.cast")?.as_ptr(),
            )
        };

        // Copy old elements if any
        let cap_nonzero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                current_cap,
                zero,
                CString::new("vec.cap.nonzero")?.as_ptr(),
            )
        };
        let copy_loop_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.copy.loop")?.as_ptr(),
            )
        };
        let copy_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.copy.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_nonzero, copy_loop_bb, copy_done_bb) };

        // loop: i in [0, current_len)
        unsafe { LLVMPositionBuilderAtEnd(self.builder, copy_loop_bb) };
        let idx_slot = unsafe {
            LLVMBuildAlloca(self.builder, usize_ty, CString::new("vec.copy.i")?.as_ptr())
        };
        unsafe { LLVMBuildStore(self.builder, zero, idx_slot) };
        let loop_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.copy.check")?.as_ptr(),
            )
        };
        let loop_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.copy.body")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, loop_check_bb) };

        // check
        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_check_bb) };
        let idx_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("vec.copy.i.load")?.as_ptr(),
            )
        };
        let cond = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                current_len,
                CString::new("vec.copy.cond")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cond, loop_body_bb, copy_done_bb) };

        // body
        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_body_bb) };
        let old_data_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?,
                data_ptr,
                CString::new("vec.data.old")?.as_ptr(),
            )
        };
        let elem_ty_loop = self.get_llvm_type(elem_type)?;
        let src_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_ty_loop,
                old_data_val,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("vec.copy.src")?.as_ptr(),
            )
        };
        let dst_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_ty_loop,
                new_data_ptr,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("vec.copy.dst")?.as_ptr(),
            )
        };
        let elem_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                elem_ty_loop,
                src_ptr,
                CString::new("vec.copy.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, elem_val, dst_ptr) };
        let inc = unsafe {
            LLVMBuildAdd(
                self.builder,
                idx_val,
                one,
                CString::new("vec.copy.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, inc, idx_slot) };
        unsafe { LLVMBuildBr(self.builder, loop_check_bb) };

        // done: free old buffer if cap>0, then install new data/cap
        unsafe { LLVMPositionBuilderAtEnd(self.builder, copy_done_bb) };
        let free_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.free.old")?.as_ptr(),
            )
        };
        let after_free_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.free.cont")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_nonzero, free_bb, after_free_bb) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, free_bb) };
        let old_data_val_free = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?,
                data_ptr,
                CString::new("vec.data.old.free")?.as_ptr(),
            )
        };
        self.codegen_free(old_data_val_free)?;
        unsafe { LLVMBuildBr(self.builder, after_free_bb) };
        unsafe { LLVMPositionBuilderAtEnd(self.builder, after_free_bb) };
        unsafe { LLVMBuildStore(self.builder, new_data_ptr, data_ptr) };
        unsafe { LLVMBuildStore(self.builder, new_cap, cap_ptr) };
        unsafe { LLVMBuildBr(self.builder, cont_bb) };

        // ---- No-grow path falls through to cont_bb ----
        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let data_after = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?,
                data_ptr,
                CString::new("vec.data.after")?.as_ptr(),
            )
        };
        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_llvm_ty,
                data_after,
                vec![current_len].as_mut_ptr(),
                1,
                CString::new("vec.push.dest")?.as_ptr(),
            )
        };
        let value_val = self.codegen_value(value, func, local_map)?;
        unsafe { LLVMBuildStore(self.builder, value_val, elem_ptr) };
        let new_len = unsafe {
            LLVMBuildAdd(
                self.builder,
                current_len,
                one,
                CString::new("vec.len.next")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_len, len_ptr) };

        let load_name = CString::new("vec.push.val")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_vec_ty, vec_ptr, load_name.as_ptr()) })
    }

    fn codegen_vec_pop(
        &mut self,
        vec: LocalId,
        elem_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, vec_ptr) = self.struct_pointer_for_local(vec, func, local_map)?;
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", struct_name))?;

        // Field pointers
        let data_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };

        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let is_empty = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                len_val,
                zero,
                CString::new("vec.empty")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        if parent_bb.is_null() {
            bail!("builder not positioned in block for vec pop");
        }
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        if parent_fn.is_null() {
            bail!("vec pop parent function missing");
        }

        let some_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.pop.some")?.as_ptr(),
            )
        };
        let none_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.pop.none")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.pop.cont")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildCondBr(self.builder, is_empty, none_bb, some_bb) };

        // None branch
        unsafe { LLVMPositionBuilderAtEnd(self.builder, none_bb) };
        let none_val = self.codegen_option_none(elem_type, mir_module)?;
        unsafe { LLVMBuildBr(self.builder, cont_bb) };

        // Some branch
        unsafe { LLVMPositionBuilderAtEnd(self.builder, some_bb) };
        let new_len = unsafe {
            LLVMBuildSub(
                self.builder,
                len_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("vec.len.dec")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_len, len_ptr) };

        let raw_ptr_ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;
        let data_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                raw_ptr_ty,
                data_ptr,
                CString::new("vec.data.load")?.as_ptr(),
            )
        };
        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_llvm_ty,
                data_val,
                vec![new_len].as_mut_ptr(),
                1,
                CString::new("vec.pop.ptr")?.as_ptr(),
            )
        };
        let popped_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                elem_llvm_ty,
                elem_ptr,
                CString::new("vec.pop.load")?.as_ptr(),
            )
        };
        let some_val = self.codegen_option_some(elem_type, popped_val, mir_module)?;
        unsafe { LLVMBuildBr(self.builder, cont_bb) };

        // Continuation phi
        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let option_name = format!("Option${}", self.type_key(elem_type));
        let llvm_option_ty = self.get_enum_type(&option_name)?;
        let phi = unsafe {
            let phi_name = CString::new("vec.pop.phi")?;
            let phi_node = LLVMBuildPhi(self.builder, llvm_option_ty, phi_name.as_ptr());
            let mut incoming_vals = vec![none_val, some_val];
            let mut incoming_bbs = vec![none_bb, some_bb];
            LLVMAddIncoming(
                phi_node,
                incoming_vals.as_mut_ptr(),
                incoming_bbs.as_mut_ptr(),
                incoming_vals.len() as u32,
            );
            phi_node
        };

        Ok(phi)
    }

    fn codegen_drop_vec(
        &mut self,
        vec: LocalId,
        elem_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<()> {
        let (struct_name, vec_ptr) = self.struct_pointer_for_local(vec, func, local_map)?;
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", struct_name))?;

        // Field pointers
        let data_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                0,
                CString::new("vec.data").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                1,
                CString::new("vec.len").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                2,
                CString::new("vec.cap").unwrap().as_ptr(),
            )
        };

        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("vec.cap.load")?.as_ptr(),
            )
        };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let has_data = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                cap_val,
                zero,
                CString::new("vec.cap.nonzero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        if parent_bb.is_null() {
            bail!("builder not positioned in block for vec drop");
        }
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        if parent_fn.is_null() {
            bail!("vec drop parent function missing");
        }

        let drop_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.drop.body")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.drop.cont")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildCondBr(self.builder, has_data, drop_bb, cont_bb) };

        // Drop body
        unsafe { LLVMPositionBuilderAtEnd(self.builder, drop_bb) };
        let raw_ptr_ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;
        let data_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                raw_ptr_ty,
                data_ptr,
                CString::new("vec.data.load")?.as_ptr(),
            )
        };

        // loop over elements 0..len
        let idx_slot = unsafe {
            LLVMBuildAlloca(self.builder, usize_ty, CString::new("vec.drop.i")?.as_ptr())
        };
        unsafe { LLVMBuildStore(self.builder, zero, idx_slot) };
        let loop_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.drop.check")?.as_ptr(),
            )
        };
        let loop_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("vec.drop.body.loop")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, loop_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_check_bb) };
        let idx_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("vec.drop.i.load")?.as_ptr(),
            )
        };
        let cond = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                len_val,
                CString::new("vec.drop.cond")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cond, loop_body_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_body_bb) };
        let elem_llvm_ty = self.get_llvm_type(elem_type)?;
        let elem_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                elem_llvm_ty,
                data_val,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("vec.drop.elem.ptr")?.as_ptr(),
            )
        };
        self.codegen_drop_elem_slot(elem_ptr, elem_type)?;
        let inc = unsafe {
            LLVMBuildAdd(
                self.builder,
                idx_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("vec.drop.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, inc, idx_slot) };
        unsafe { LLVMBuildBr(self.builder, loop_check_bb) };

        // After loop: free buffer
        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let data_to_free = unsafe {
            LLVMBuildLoad2(
                self.builder,
                raw_ptr_ty,
                data_ptr,
                CString::new("vec.data.free")?.as_ptr(),
            )
        };
        self.codegen_free(data_to_free)?;
        unsafe { LLVMBuildStore(self.builder, zero, len_ptr) };
        unsafe { LLVMBuildStore(self.builder, zero, cap_ptr) };
        let null_data = unsafe { LLVMConstNull(raw_ptr_ty) };
        unsafe { LLVMBuildStore(self.builder, null_data, data_ptr) };

        Ok(())
    }

    fn codegen_drop_elem_slot(&mut self, slot: LLVMValueRef, elem_type: &Type) -> Result<()> {
        match elem_type {
            Type::Own(inner) => self.codegen_drop_own_slot(slot, inner),
            Type::Shared(inner) => self.codegen_drop_shared_slot(slot, inner),
            Type::String => self.codegen_drop_string_slot(slot),
            _ => Ok(()),
        }
    }

    fn codegen_array_len(&mut self, base: LocalId, func: &MirFunction) -> Result<LLVMValueRef> {
        let base_ty = func
            .locals
            .get(base.0 as usize)
            .and_then(|local| local.ty.as_ref())
            .ok_or_else(|| anyhow!("array length base has unknown type"))?;

        let size = match base_ty {
            Type::Array(_, size) => *size,
            _ => bail!(".len() is only supported on arrays"),
        };

        let i32_ty = unsafe { LLVMInt32TypeInContext(self.context) };
        Ok(unsafe { LLVMConstInt(i32_ty, size as u64, 0) })
    }

    fn codegen_own_new(
        &mut self,
        elem_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            let llvm_elem_ty = self.get_llvm_type(elem_type)?;
            let size_val = LLVMSizeOf(llvm_elem_ty);
            let malloc_fn = self.ensure_malloc_fn()?;
            let malloc_ty = self.malloc_function_type();
            let mut args = vec![size_val];
            let call_name = CString::new("own.alloc")?;
            let raw_ptr = LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );
            let typed_ptr_ty = LLVMPointerType(llvm_elem_ty, 0);
            let cast_name = CString::new("own.ptr")?;
            let typed_ptr =
                LLVMBuildBitCast(self.builder, raw_ptr, typed_ptr_ty, cast_name.as_ptr());
            let value_val = self.codegen_value(value, func, local_map)?;
            LLVMBuildStore(self.builder, value_val, typed_ptr);
            Ok(typed_ptr)
        }
    }

    fn codegen_own_into_raw(
        &mut self,
        base: LocalId,
        elem_type: &Type,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let slot = local_map
            .get(&base)
            .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
        let llvm_ty = self.get_llvm_type(&Type::Own(Box::new(elem_type.clone())))?;
        unsafe {
            let load_name = CString::new("own.into")?;
            let ptr_val = LLVMBuildLoad2(self.builder, llvm_ty, *slot, load_name.as_ptr());
            let null_ptr = LLVMConstPointerNull(llvm_ty);
            LLVMBuildStore(self.builder, null_ptr, *slot);
            Ok(ptr_val)
        }
    }

    fn codegen_own_from_raw(
        &mut self,
        ptr: &MirValue,
        elem_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let value = self.codegen_value(ptr, func, local_map)?;
        let desired_ty = self.get_llvm_type(&Type::Own(Box::new(elem_type.clone())))?;
        unsafe {
            if LLVMTypeOf(value) == desired_ty {
                Ok(value)
            } else {
                let cast_name = CString::new("own.from.raw")?;
                Ok(LLVMBuildBitCast(
                    self.builder,
                    value,
                    desired_ty,
                    cast_name.as_ptr(),
                ))
            }
        }
    }

    fn codegen_raw_ptr_null(&mut self, elem_type: &Type) -> Result<LLVMValueRef> {
        let ty = self.get_llvm_type(&Type::RawPtr(Box::new(elem_type.clone())))?;
        Ok(unsafe { LLVMConstPointerNull(ty) })
    }

    fn codegen_shared_new(
        &mut self,
        elem_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            // 1. Create struct type [refcount: usize, data: T]
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(
                self.context,
                field_tys.as_mut_ptr(),
                2,
                0, // not packed
            );

            // 2. Allocate memory
            let size_val = LLVMSizeOf(rc_struct);
            let malloc_fn = self.ensure_malloc_fn()?;
            let malloc_ty = self.malloc_function_type();
            let mut args = vec![size_val];
            let call_name = CString::new("shared.alloc")?;
            let raw_ptr = LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );

            // 3. Cast to typed pointer
            let typed_ptr_ty = LLVMPointerType(rc_struct, 0);
            let cast_name = CString::new("shared.ptr")?;
            let typed_ptr =
                LLVMBuildBitCast(self.builder, raw_ptr, typed_ptr_ty, cast_name.as_ptr());

            // 4. Initialize refcount to 1
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                typed_ptr,
                0,
                CString::new("shared.rc")?.as_ptr(),
            );
            let one = LLVMConstInt(usize_ty, 1, 0);
            LLVMBuildStore(self.builder, one, refcount_ptr);

            // 5. Store the data value
            let data_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                typed_ptr,
                1,
                CString::new("shared.data")?.as_ptr(),
            );
            let value_val = self.codegen_value(value, func, local_map)?;
            LLVMBuildStore(self.builder, value_val, data_ptr);

            // 6. Return pointer (API returns pointer to whole struct)
            Ok(typed_ptr)
        }
    }

    fn codegen_shared_clone(
        &mut self,
        base: LocalId,
        elem_type: &Type,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        unsafe {
            // 1. Load the Shared<T> pointer
            let slot = local_map
                .get(&base)
                .ok_or_else(|| anyhow!("undefined local {:?}", base))?;
            let shared_ty = self.get_llvm_type(&Type::Shared(Box::new(elem_type.clone())))?;
            let load_name = CString::new("shared.load")?;
            let ptr_val = LLVMBuildLoad2(self.builder, shared_ty, *slot, load_name.as_ptr());

            // 2. Get struct type for GEP
            let usize_ty = LLVMInt64TypeInContext(self.context);
            let elem_llvm_ty = self.get_llvm_type(elem_type)?;
            let mut field_tys = vec![usize_ty, elem_llvm_ty];
            let rc_struct = LLVMStructTypeInContext(self.context, field_tys.as_mut_ptr(), 2, 0);

            // 3. Get pointer to refcount field
            let refcount_ptr = LLVMBuildStructGEP2(
                self.builder,
                rc_struct,
                ptr_val,
                0,
                CString::new("shared.rc.ptr")?.as_ptr(),
            );

            // 4. Load current refcount
            let rc_name = CString::new("shared.rc.old")?;
            let old_count = LLVMBuildLoad2(self.builder, usize_ty, refcount_ptr, rc_name.as_ptr());

            // 5. Increment refcount
            let one = LLVMConstInt(usize_ty, 1, 0);
            let inc_name = CString::new("shared.rc.new")?;
            let new_count = LLVMBuildAdd(self.builder, old_count, one, inc_name.as_ptr());
            LLVMBuildStore(self.builder, new_count, refcount_ptr);

            // 6. Return the same pointer (refcount incremented)
            Ok(ptr_val)
        }
    }

    fn ensure_malloc_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.malloc_fn {
            return Ok(func);
        }
        unsafe {
            let fn_ty = self.malloc_function_type();
            let name = CString::new("malloc")?;
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.malloc_fn = Some(func);
            Ok(func)
        }
    }

    fn ensure_free_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.free_fn {
            return Ok(func);
        }
        unsafe {
            let fn_ty = self.free_function_type();
            let name = CString::new("free")?;
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.free_fn = Some(func);
            Ok(func)
        }
    }

    fn malloc_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let size_ty = LLVMInt64TypeInContext(self.context);
            let mut params = vec![size_ty];
            LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
        }
    }

    fn free_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let void_ty = LLVMVoidTypeInContext(self.context);
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = vec![i8_ptr];
            LLVMFunctionType(void_ty, params.as_mut_ptr(), params.len() as u32, 0)
        }
    }

    fn codegen_free(&mut self, ptr: LLVMValueRef) -> Result<()> {
        unsafe {
            let free_fn = self.ensure_free_fn()?;
            let fn_ty = self.free_function_type();
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let cast_name = CString::new("own.free.cast")?;
            let cast_ptr = LLVMBuildBitCast(self.builder, ptr, i8_ptr, cast_name.as_ptr());
            let mut args = vec![cast_ptr];
            let call_name = CString::new("own.free")?;
            LLVMBuildCall2(
                self.builder,
                fn_ty,
                free_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                call_name.as_ptr(),
            );
        }
        Ok(())
    }

    fn emit_bounds_check(&mut self, index_val: LLVMValueRef, array_size: usize) -> Result<()> {
        unsafe {
            let i32_ty = LLVMInt32TypeInContext(self.context);
            let zero = LLVMConstInt(i32_ty, 0, 0);
            let nonneg_name = CString::new("idx.nonneg")?;
            let non_negative = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                index_val,
                zero,
                nonneg_name.as_ptr(),
            );

            let size_const = LLVMConstInt(i32_ty, array_size as u64, 0);
            let upper_name = CString::new("idx.upper")?;
            let within_upper = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                index_val,
                size_const,
                upper_name.as_ptr(),
            );

            let cond_name = CString::new("idx.in_bounds")?;
            let cond = LLVMBuildAnd(self.builder, non_negative, within_upper, cond_name.as_ptr());

            let current_block = LLVMGetInsertBlock(self.builder);
            if current_block.is_null() {
                bail!("bounds check requires valid insertion block");
            }
            let parent_func = LLVMGetBasicBlockParent(current_block);
            if parent_func.is_null() {
                bail!("bounds check parent function missing");
            }

            let ok_name = CString::new("bounds.ok")?;
            let panic_name = CString::new("bounds.panic")?;
            let ok_bb = LLVMAppendBasicBlockInContext(self.context, parent_func, ok_name.as_ptr());
            let panic_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, panic_name.as_ptr());

            LLVMBuildCondBr(self.builder, cond, ok_bb, panic_bb);

            LLVMPositionBuilderAtEnd(self.builder, panic_bb);
            self.emit_panic()?;
            LLVMBuildUnreachable(self.builder);

            LLVMPositionBuilderAtEnd(self.builder, ok_bb);
        }
        Ok(())
    }

    fn emit_vec_bounds_check(
        &mut self,
        index_val: LLVMValueRef,
        len_val: LLVMValueRef,
        len_ty: LLVMTypeRef,
    ) -> Result<()> {
        unsafe {
            let zero = LLVMConstInt(len_ty, 0, 0);
            let nonneg_name = CString::new("idx.nonneg")?;
            let non_negative = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                index_val,
                zero,
                nonneg_name.as_ptr(),
            );

            let upper_name = CString::new("idx.upper")?;
            let within_upper = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                index_val,
                len_val,
                upper_name.as_ptr(),
            );

            let cond_name = CString::new("idx.in_bounds")?;
            let cond = LLVMBuildAnd(self.builder, non_negative, within_upper, cond_name.as_ptr());

            let current_block = LLVMGetInsertBlock(self.builder);
            if current_block.is_null() {
                bail!("bounds check requires valid insertion block");
            }
            let parent_func = LLVMGetBasicBlockParent(current_block);
            if parent_func.is_null() {
                bail!("bounds check parent function missing");
            }

            let ok_name = CString::new("bounds.ok")?;
            let panic_name = CString::new("bounds.panic")?;
            let ok_bb = LLVMAppendBasicBlockInContext(self.context, parent_func, ok_name.as_ptr());
            let panic_bb =
                LLVMAppendBasicBlockInContext(self.context, parent_func, panic_name.as_ptr());

            LLVMBuildCondBr(self.builder, cond, ok_bb, panic_bb);

            LLVMPositionBuilderAtEnd(self.builder, panic_bb);
            self.emit_panic()?;
            LLVMBuildUnreachable(self.builder);

            LLVMPositionBuilderAtEnd(self.builder, ok_bb);
        }
        Ok(())
    }

    fn emit_panic(&mut self) -> Result<()> {
        unsafe {
            let trap_name = CString::new("llvm.trap")?;
            let void_ty = LLVMVoidTypeInContext(self.context);
            let trap_ty = LLVMFunctionType(void_ty, std::ptr::null_mut(), 0, 0);
            let mut trap_fn = LLVMGetNamedFunction(self.module, trap_name.as_ptr());
            if trap_fn.is_null() {
                trap_fn = LLVMAddFunction(self.module, trap_name.as_ptr(), trap_ty);
            }
            let call_name = CString::new("panic.trap")?;
            LLVMBuildCall2(
                self.builder,
                trap_ty,
                trap_fn,
                std::ptr::null_mut(),
                0,
                call_name.as_ptr(),
            );
        }
        Ok(())
    }

    fn struct_pointer_for_local(
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

    fn enum_pointer_for_local(
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

    /// Execute a function via JIT for testing purposes
    /// Note: This creates a clone of the module for JIT execution
    pub fn jit_execute_i32(&self, fn_name: &str) -> Result<i32> {
        self.jit_execute_i32_with_symbols(fn_name, &HashMap::new())
    }

    /// Execute a function via JIT with custom symbol resolution
    /// The symbols map provides address resolution for extern functions
    /// Note: This creates a clone of the module for JIT execution
    pub fn jit_execute_i32_with_symbols(
        &self,
        fn_name: &str,
        symbols: &HashMap<String, u64>,
    ) -> Result<i32> {
        use llvm_sys::execution_engine::*;
        use llvm_sys::target::*;

        unsafe {
            // Initialize native target for JIT
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            // Clone the module so the execution engine can take ownership
            let module_clone = LLVMCloneModule(self.module);

            // Create MCJIT execution engine
            let mut ee = std::ptr::null_mut();
            let mut error = std::ptr::null_mut();

            LLVMLinkInMCJIT();

            if LLVMCreateMCJITCompilerForModule(
                &mut ee,
                module_clone,
                std::ptr::null_mut(),
                0,
                &mut error,
            ) != 0
            {
                let err_msg = if error.is_null() {
                    "unknown error".to_string()
                } else {
                    let msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    msg
                };
                return Err(anyhow!("Failed to create execution engine: {}", err_msg));
            }

            // Register custom symbols with the execution engine
            for (sym_name, &addr) in symbols {
                let sym_name_c = CString::new(sym_name.as_str())?;
                let func = LLVMGetNamedFunction(module_clone, sym_name_c.as_ptr());
                if !func.is_null() {
                    LLVMAddGlobalMapping(ee, func, addr as *mut std::ffi::c_void);
                }
            }

            // Find the function
            let fn_name_c = CString::new(fn_name)?;
            let func_addr = LLVMGetFunctionAddress(ee, fn_name_c.as_ptr());

            if func_addr == 0 {
                LLVMDisposeExecutionEngine(ee);
                return Err(anyhow!("Function {} not found", fn_name));
            }

            // Cast to function pointer and call it
            let func: extern "C" fn() -> i32 = std::mem::transmute(func_addr);
            let result = func();

            // Clean up execution engine (which also disposes the cloned module)
            LLVMDisposeExecutionEngine(ee);

            Ok(result)
        }
    }

    /// Emit an object file (.o) to disk using LLVM's target machine API
    pub fn emit_object_file(&self, output_path: &Path) -> Result<()> {
        unsafe {
            // Initialize all LLVM targets (x86, ARM, etc.)
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();

            // Get the default target triple for this machine (e.g., "x86_64-apple-darwin24.0.0")
            let target_triple = LLVMGetDefaultTargetTriple();
            LLVMSetTarget(self.module, target_triple);

            // Get target from triple
            let mut target = std::ptr::null_mut();
            let mut error = std::ptr::null_mut();
            if LLVMGetTargetFromTriple(target_triple, &mut target, &mut error) != 0 {
                let err_msg = if error.is_null() {
                    "unknown error".to_string()
                } else {
                    let msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    msg
                };
                LLVMDisposeMessage(target_triple);
                return Err(anyhow!("Failed to get target: {}", err_msg));
            }

            // Create target machine with native CPU features
            let cpu = CString::new("generic")?;
            let features = CString::new("")?;
            let target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                cpu.as_ptr(),
                features.as_ptr(),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                LLVMRelocMode::LLVMRelocPIC, // Position-independent code for linking
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            if target_machine.is_null() {
                LLVMDisposeMessage(target_triple);
                return Err(anyhow!("Failed to create target machine"));
            }

            // Convert output path to C string
            let output_path_str = output_path
                .to_str()
                .ok_or_else(|| anyhow!("Invalid output path"))?;
            let output_path_c = CString::new(output_path_str)?;

            // Emit object file to disk
            let mut error = std::ptr::null_mut();
            if LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                output_path_c.as_ptr() as *mut i8,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut error,
            ) != 0
            {
                let err_msg = if error.is_null() {
                    "unknown error".to_string()
                } else {
                    let msg = CStr::from_ptr(error).to_string_lossy().into_owned();
                    LLVMDisposeMessage(error);
                    msg
                };
                LLVMDisposeTargetMachine(target_machine);
                LLVMDisposeMessage(target_triple);
                return Err(anyhow!("Failed to emit object file: {}", err_msg));
            }

            // Clean up
            LLVMDisposeTargetMachine(target_machine);
            LLVMDisposeMessage(target_triple);

            Ok(())
        }
    }
}

impl Drop for CodegenContext {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::mir::{
        Local, LocalId, MirBlock, MirFunction, MirInst, MirModule, MirValue, Rvalue,
    };
    use std::collections::HashMap;

    #[test]
    fn creates_empty_module() {
        let ctx = CodegenContext::new("test").unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("test"));
    }

    #[test]
    fn codegens_simple_function() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![],
                blocks: vec![MirBlock {
                    insts: vec![MirInst::Return(Some(MirValue::Int(42)))],
                }],
            }],
            extern_functions: Vec::new(),
        };
        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("define i32 @main"));
        assert!(ir.contains("ret i32 42"));
    }

    #[test]
    fn jit_executes_simple_function() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![],
                blocks: vec![MirBlock {
                    insts: vec![MirInst::Return(Some(MirValue::Int(42)))],
                }],
            }],
            extern_functions: Vec::new(),
        };
        ctx.codegen_module(&mir).unwrap();
        let result = ctx.jit_execute_i32("main").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn codegens_struct_literal() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mut struct_types = HashMap::new();
        struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![("x".into(), Type::I32), ("y".into(), Type::I32)],
            },
        );

        let mir = MirModule {
            struct_types,
            enum_types: HashMap::new(),
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::Named("Point".into())),
                params: vec![],
                locals: vec![
                    Local {
                        name: Some("p".into()),
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::Named("Point".into())),
                    },
                ],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(1),
                            value: Rvalue::StructLit {
                                struct_name: "Point".into(),
                                field_values: vec![
                                    ("x".into(), MirValue::Int(1)),
                                    ("y".into(), MirValue::Int(2)),
                                ],
                            },
                        },
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Move(LocalId(1)),
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
            extern_functions: Vec::new(),
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("%Point = type { i32, i32 }"));
        assert!(ir.contains("getelementptr inbounds"));
    }

    #[test]
    fn codegens_extern_declare_and_call() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            extern_functions: vec![MirExternFunction {
                name: "foo".into(),
                ret_type: Some(Type::I32),
                params: vec![Type::I32],
                abi: Some("C".into()),
                link_name: None,
            }],
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![Local {
                    name: None,
                    ty: Some(Type::I32),
                }],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Call {
                                name: "foo".into(),
                                args: vec![MirValue::Int(1)],
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("declare i32 @foo(i32)"));
        assert!(ir.contains("call i32 @foo"));
    }

    #[test]
    fn codegens_field_access() {
        let mut ctx = CodegenContext::new("test").unwrap();
        let mut struct_types = HashMap::new();
        struct_types.insert(
            "Point".into(),
            StructType {
                name: "Point".into(),
                fields: vec![("x".into(), Type::I32), ("y".into(), Type::I32)],
            },
        );

        let mir = MirModule {
            struct_types,
            enum_types: HashMap::new(),
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![
                    Local {
                        name: Some("p".into()),
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::Named("Point".into())),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::I32),
                    },
                ],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(1),
                            value: Rvalue::StructLit {
                                struct_name: "Point".into(),
                                field_values: vec![
                                    ("x".into(), MirValue::Int(10)),
                                    ("y".into(), MirValue::Int(20)),
                                ],
                            },
                        },
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Move(LocalId(1)),
                        },
                        MirInst::Assign {
                            local: LocalId(2),
                            value: Rvalue::FieldAccess {
                                base: LocalId(0),
                                field_name: "y".into(),
                                field_index: 1,
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(2)))),
                    ],
                }],
            }],
            extern_functions: Vec::new(),
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();
        assert!(ir.contains("getelementptr inbounds"));
        assert!(ir.contains("ret i32"));
    }

    #[test]
    fn jit_resolves_extern_symbol_from_host() {
        // Define a test host function that will be called from JIT code
        extern "C" fn test_add_ten(x: i32) -> i32 {
            x + 10
        }

        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            extern_functions: vec![MirExternFunction {
                name: "test_add_ten".into(),
                ret_type: Some(Type::I32),
                params: vec![Type::I32],
                abi: Some("C".into()),
                link_name: None,
            }],
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![Local {
                    name: None,
                    ty: Some(Type::I32),
                }],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Call {
                                name: "test_add_ten".into(),
                                args: vec![MirValue::Int(5)],
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();

        // Create symbol map with the address of our test function
        let mut symbols = HashMap::new();
        symbols.insert("test_add_ten".to_string(), test_add_ten as u64);

        // Execute and verify the result
        let result = ctx.jit_execute_i32_with_symbols("main", &symbols).unwrap();
        assert_eq!(result, 15);
    }

    #[test]
    fn jit_extern_symbol_codegen_without_execution() {
        // This test verifies that extern functions are declared in LLVM IR
        // but does NOT attempt to execute code with missing symbols (which would crash)
        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            extern_functions: vec![MirExternFunction {
                name: "missing_function".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                abi: Some("C".into()),
                link_name: None,
            }],
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![Local {
                    name: None,
                    ty: Some(Type::I32),
                }],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Call {
                                name: "missing_function".into(),
                                args: vec![],
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();
        let ir = ctx.dump_ir();

        // Verify the extern function is declared
        assert!(ir.contains("declare i32 @missing_function()"));
        // Verify it's called
        assert!(ir.contains("call i32 @missing_function"));

        // Note: Actually executing this code would crash due to missing symbol.
        // In a production JIT, you'd want symbol resolution validation before execution.
    }

    #[test]
    fn jit_hello_world_with_putchar() {
        // First "Hello World" - calling libc putchar to print 'H'
        extern "C" fn putchar_wrapper(c: i32) -> i32 {
            // Mock putchar for testing (real one would write to stdout)
            c // Just return the character code
        }

        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            extern_functions: vec![MirExternFunction {
                name: "putchar".into(),
                ret_type: Some(Type::I32),
                params: vec![Type::I32],
                abi: Some("C".into()),
                link_name: None,
            }],
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![Local {
                    name: None,
                    ty: Some(Type::I32),
                }],
                blocks: vec![MirBlock {
                    insts: vec![
                        // Call putchar('H') - ASCII 72
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::Call {
                                name: "putchar".into(),
                                args: vec![MirValue::Int(72)], // 'H'
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(0)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();

        // Register putchar symbol
        let mut symbols = HashMap::new();
        symbols.insert("putchar".to_string(), putchar_wrapper as u64);

        // Execute - should "print" 'H' and return 72
        let result = ctx.jit_execute_i32_with_symbols("main", &symbols).unwrap();
        assert_eq!(result, 72); // putchar returns the character it printed
    }

    #[test]
    fn jit_hello_world_with_puts_literal() {
        extern "C" fn puts_wrapper(ptr: *const i8) -> i32 {
            unsafe { CStr::from_ptr(ptr).to_bytes().len() as i32 }
        }

        let mut ctx = CodegenContext::new("test").unwrap();
        let mir = MirModule {
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            extern_functions: vec![MirExternFunction {
                name: "puts".into(),
                ret_type: Some(Type::I32),
                params: vec![Type::Str],
                abi: Some("C".into()),
                link_name: None,
            }],
            functions: vec![MirFunction {
                name: "main".into(),
                ret_type: Some(Type::I32),
                params: vec![],
                locals: vec![
                    Local {
                        name: None,
                        ty: Some(Type::Str),
                    },
                    Local {
                        name: None,
                        ty: Some(Type::I32),
                    },
                ],
                blocks: vec![MirBlock {
                    insts: vec![
                        MirInst::Assign {
                            local: LocalId(0),
                            value: Rvalue::StringLit {
                                content: "Hello".into(),
                                global_name: ".str.main.0".into(),
                            },
                        },
                        MirInst::Assign {
                            local: LocalId(1),
                            value: Rvalue::Call {
                                name: "puts".into(),
                                args: vec![MirValue::Local(LocalId(0))],
                            },
                        },
                        MirInst::Return(Some(MirValue::Local(LocalId(1)))),
                    ],
                }],
            }],
        };

        ctx.codegen_module(&mir).unwrap();

        let mut symbols = HashMap::new();
        symbols.insert("puts".to_string(), puts_wrapper as u64);

        let result = ctx.jit_execute_i32_with_symbols("main", &symbols).unwrap();
        assert_eq!(result, 5);
    }
}

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

const MAP_INITIAL_BUCKETS: u64 = 8;
const MAP_LOAD_FACTOR_NUM: u64 = 3;
const MAP_LOAD_FACTOR_DEN: u64 = 4;

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
    strdup_fn: Option<LLVMValueRef>,
    string_globals: HashMap<String, LLVMValueRef>,
    function_types: HashMap<String, LLVMTypeRef>,
    argv_global: Option<LLVMValueRef>,
    argc_global: Option<LLVMValueRef>,
    argv_vec_global: Option<LLVMValueRef>,
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
                strdup_fn: None,
                string_globals: HashMap::new(),
                function_types: HashMap::new(),
                argv_global: None,
                argc_global: None,
                argv_vec_global: None,
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

    fn get_map_layout(&self, struct_name: &str) -> Option<(LLVMTypeRef, LLVMTypeRef)> {
        // Map$K__V layout: [buckets ptr, cap, len]
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

    fn function_type_for(&self, name: &str) -> Result<LLVMTypeRef> {
        self.function_types
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("missing function type for {}", name))
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
            Type::Tuple(elem_types) => {
                if elem_types.is_empty() {
                    "unit".into()
                } else {
                    let type_names: Vec<String> = elem_types.iter().map(|t| self.type_key(t)).collect();
                    format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
                }
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

    fn mir_uses_sys_argv(&self, mir_module: &MirModule) -> bool {
        for func in &mir_module.functions {
            for block in &func.blocks {
                for inst in &block.insts {
                    if let MirInst::Assign { value, .. } = inst {
                        if let Rvalue::Call { name, .. } = value {
                            if name == "argv" || name == "std::sys::argv" {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        false
    }

    fn argv_vec_llvm_type(&self) -> Result<LLVMTypeRef> {
        let vec_name = format!("Vec${}", self.type_key(&Type::String));
        self.get_struct_type(&vec_name)
    }

    fn ensure_sys_argv_globals(
        &mut self,
        vec_ty: LLVMTypeRef,
    ) -> Result<(LLVMValueRef, LLVMValueRef, LLVMValueRef)> {
        unsafe {
            if self.argc_global.is_none() {
                let usize_ty = LLVMInt64TypeInContext(self.context);
                let name = CString::new("glyph.argc")?;
                let global = LLVMAddGlobal(self.module, usize_ty, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstInt(usize_ty, 0, 0));
                self.argc_global = Some(global);
            }
            if self.argv_global.is_none() {
                let i8_ty = LLVMInt8TypeInContext(self.context);
                let i8_ptr = LLVMPointerType(i8_ty, 0);
                let i8_ptr_ptr = LLVMPointerType(i8_ptr, 0);
                let name = CString::new("glyph.argv")?;
                let global = LLVMAddGlobal(self.module, i8_ptr_ptr, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstPointerNull(i8_ptr_ptr));
                self.argv_global = Some(global);
            }
            if self.argv_vec_global.is_none() {
                let name = CString::new("glyph.argv.vec")?;
                let global = LLVMAddGlobal(self.module, vec_ty, name.as_ptr());
                LLVMSetInitializer(global, LLVMConstNull(vec_ty));
                self.argv_vec_global = Some(global);
            }
        }

        Ok((
            self.argc_global.unwrap(),
            self.argv_global.unwrap(),
            self.argv_vec_global.unwrap(),
        ))
    }

    fn codegen_sys_argv_value(&mut self) -> Result<LLVMValueRef> {
        let vec_ty = self.argv_vec_llvm_type()?;
        let (_, _, argv_vec_global) = self.ensure_sys_argv_globals(vec_ty)?;
        let load_name = CString::new("argv.load")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, vec_ty, argv_vec_global, load_name.as_ptr()) })
    }

    fn codegen_sys_argv_build(
        &mut self,
        argc_val: LLVMValueRef,
        argv_val: LLVMValueRef,
    ) -> Result<LLVMValueRef> {
        let vec_ty = self.argv_vec_llvm_type()?;
        let (argc_global, argv_global, argv_vec_global) = self.ensure_sys_argv_globals(vec_ty)?;
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let argc_val = self.ensure_usize(argc_val, usize_ty)?;

        unsafe {
            LLVMBuildStore(self.builder, argc_val, argc_global);
            LLVMBuildStore(self.builder, argv_val, argv_global);
        }

        let vec_val = self.codegen_vec_init_with_capacity_value(&Type::String, argc_val)?;
        let vec_ptr = unsafe {
            let alloca = LLVMBuildAlloca(self.builder, vec_ty, CString::new("argv.vec")?.as_ptr());
            LLVMBuildStore(self.builder, vec_val, alloca);
            alloca
        };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let len_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                argc_val,
                zero,
                CString::new("argv.len.zero")?.as_ptr(),
            )
        };
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let i8_ptr = unsafe { LLVMPointerType(i8_ty, 0) };
        let i8_ptr_ptr = unsafe { LLVMPointerType(i8_ptr, 0) };
        let argv_is_null = unsafe {
            let null_ptr = LLVMConstPointerNull(i8_ptr_ptr);
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                argv_val,
                null_ptr,
                CString::new("argv.ptr.null")?.as_ptr(),
            )
        };
        let skip_loop = unsafe {
            LLVMBuildOr(
                self.builder,
                len_is_zero,
                argv_is_null,
                CString::new("argv.loop.skip")?.as_ptr(),
            )
        };

        unsafe {
            let parent_bb = LLVMGetInsertBlock(self.builder);
            let parent_fn = LLVMGetBasicBlockParent(parent_bb);
            let loop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.loop")?.as_ptr(),
            );
            let done_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.done")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, skip_loop, done_bb, loop_bb);

            LLVMPositionBuilderAtEnd(self.builder, loop_bb);
            let idx_slot =
                LLVMBuildAlloca(self.builder, usize_ty, CString::new("argv.idx")?.as_ptr());
            LLVMBuildStore(self.builder, zero, idx_slot);

            let check_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.check")?.as_ptr(),
            );
            let body_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.body")?.as_ptr(),
            );
            let loop_done_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("argv.loop.done")?.as_ptr(),
            );
            LLVMBuildBr(self.builder, check_bb);

            LLVMPositionBuilderAtEnd(self.builder, check_bb);
            let idx_val = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("argv.idx.load")?.as_ptr(),
            );
            let idx_ok = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                argc_val,
                CString::new("argv.idx.lt")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, idx_ok, body_bb, loop_done_bb);

            LLVMPositionBuilderAtEnd(self.builder, body_bb);
            let arg_ptr_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ptr,
                argv_val,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("argv.gep")?.as_ptr(),
            );
            let arg_ptr = LLVMBuildLoad2(
                self.builder,
                i8_ptr,
                arg_ptr_ptr,
                CString::new("argv.arg")?.as_ptr(),
            );
            let name = CString::new("strdup")?;
            let strdup_fn = LLVMGetNamedFunction(self.module, name.as_ptr());
            if strdup_fn.is_null() {
                bail!("missing strdup declaration for argv initialization");
            }
            let mut args = vec![arg_ptr];
            let dup = LLVMBuildCall2(
                self.builder,
                self.strdup_function_type(),
                strdup_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("argv.dup")?.as_ptr(),
            );
            self.codegen_vec_push_no_grow(vec_ptr, &Type::String, dup)?;
            let next_idx = LLVMBuildAdd(
                self.builder,
                idx_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("argv.idx.next")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, next_idx, idx_slot);
            LLVMBuildBr(self.builder, check_bb);

            LLVMPositionBuilderAtEnd(self.builder, loop_done_bb);
            LLVMBuildBr(self.builder, done_bb);

            LLVMPositionBuilderAtEnd(self.builder, done_bb);
            let vec_val = LLVMBuildLoad2(
                self.builder,
                vec_ty,
                vec_ptr,
                CString::new("argv.vec.load")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, vec_val, argv_vec_global);
            Ok(vec_val)
        }
    }

    fn codegen_main_wrapper(
        &mut self,
        mir_module: &MirModule,
        user_main: LLVMValueRef,
    ) -> Result<()> {
        let Some(main_mir) = mir_module.functions.iter().find(|f| f.name == "main") else {
            return Ok(());
        };
        if !main_mir.params.is_empty() {
            return Ok(());
        }

        unsafe {
            let i32_ty = LLVMInt32TypeInContext(self.context);
            let i8_ty = LLVMInt8TypeInContext(self.context);
            let i8_ptr = LLVMPointerType(i8_ty, 0);
            let i8_ptr_ptr = LLVMPointerType(i8_ptr, 0);
            let mut params = vec![i32_ty, i8_ptr_ptr];
            let fn_ty = LLVMFunctionType(i32_ty, params.as_mut_ptr(), params.len() as u32, 0);
            let name = CString::new("main")?;
            let wrapper = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            let entry = LLVMAppendBasicBlockInContext(
                self.context,
                wrapper,
                CString::new("entry")?.as_ptr(),
            );
            LLVMPositionBuilderAtEnd(self.builder, entry);

            let argc_param = LLVMGetParam(wrapper, 0);
            let argv_param = LLVMGetParam(wrapper, 1);
            let argc_val = LLVMBuildZExt(
                self.builder,
                argc_param,
                LLVMInt64TypeInContext(self.context),
                CString::new("argc.ext")?.as_ptr(),
            );
            let _ = self.codegen_sys_argv_build(argc_val, argv_param)?;

            let fn_ty = self.llvm_function_type(main_mir)?;
            let call_name = CString::new("main.call")?;
            let call_val = LLVMBuildCall2(
                self.builder,
                fn_ty,
                user_main,
                std::ptr::null_mut(),
                0,
                call_name.as_ptr(),
            );

            match main_mir.ret_type.as_ref() {
                None | Some(Type::Void) => {
                    LLVMBuildRet(self.builder, LLVMConstInt(i32_ty, 0, 0));
                }
                Some(Type::I32) => {
                    LLVMBuildRet(self.builder, call_val);
                }
                Some(_) => {
                    let cast_val = LLVMBuildTrunc(
                        self.builder,
                        call_val,
                        i32_ty,
                        CString::new("main.ret.cast")?.as_ptr(),
                    );
                    LLVMBuildRet(self.builder, cast_val);
                }
            };
        }

        Ok(())
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

    fn codegen_enum_value(
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

    fn codegen_err_value(&mut self, message: &str) -> Result<LLVMValueRef> {
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

    fn codegen_result_ok(
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

    fn codegen_result_err(
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
                    match payload {
                        Type::Void => unsafe { LLVMInt8TypeInContext(self.context) },
                        _ => self.get_llvm_type(payload)?,
                    }
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
                Type::Named(name) => {
                    // Check enum_types first, then struct_types
                    self.enum_types
                        .get(&name)
                        .copied()
                        .or_else(|| self.struct_types.get(&name).copied())
                        .ok_or_else(|| anyhow!("unknown type {} (not found in enum or struct types)", name))?
                },
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
                Type::Tuple(elem_types) => {
                    if elem_types.is_empty() {
                        // Empty tuple is void/unit type
                        LLVMVoidTypeInContext(self.context)
                    } else {
                        // Generate tuple struct name and look it up
                        let struct_name = tuple_struct_name_codegen(&elem_types);
                        self.struct_types
                            .get(&struct_name)
                            .copied()
                            .ok_or_else(|| anyhow!("tuple type {} not found in struct_types", struct_name))?
                    }
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

        let needs_sys_argv = self.mir_uses_sys_argv(mir_module);

        // Declare all functions up-front so calls can reference any order.
        let functions = self.declare_functions(mir_module, needs_sys_argv)?;

        for func in &mir_module.functions {
            let llvm_func = *functions
                .get(&func.name)
                .ok_or_else(|| anyhow!("missing declared function {}", func.name))?;
            self.codegen_function_body(func, llvm_func, &functions, mir_module)?;
        }

        if needs_sys_argv {
            if let Some(main_func) = functions.get("main").copied() {
                self.codegen_main_wrapper(mir_module, main_func)?;
            }
        }

        Ok(())
    }

    fn declare_functions(
        &mut self,
        mir_module: &MirModule,
        needs_sys_argv: bool,
    ) -> Result<HashMap<String, LLVMValueRef>> {
        let mut functions = HashMap::new();

        for func in &mir_module.functions {
            let func_type = self.llvm_function_type(func)?;
            let llvm_name = if needs_sys_argv && func.name == "main" && func.params.is_empty() {
                "__glyph_main"
            } else {
                func.name.as_str()
            };
            let func_name = CString::new(llvm_name)?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            self.function_types.insert(func.name.clone(), func_type);
            functions.insert(func.name.clone(), llvm_func);
        }

        for func in &mir_module.extern_functions {
            if functions.contains_key(&func.name) {
                continue;
            }
            let func_type = self.llvm_extern_function_type(func)?;
            let symbol_name = func.link_name.as_ref().unwrap_or(&func.name);
            let func_name = CString::new(symbol_name.as_str())?;
            let llvm_func = unsafe { LLVMAddFunction(self.module, func_name.as_ptr(), func_type) };
            self.function_types.insert(func.name.clone(), func_type);
            unsafe { LLVMSetLinkage(llvm_func, LLVMLinkage::LLVMExternalLinkage) };
            if symbol_name == "strdup" && self.strdup_fn.is_none() {
                self.strdup_fn = Some(llvm_func);
            }
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
            Type::App { base, args } if base == "Map" => {
                let key_ty = args.get(0).cloned().unwrap_or(Type::I32);
                let val_ty = args.get(1).cloned().unwrap_or(Type::I32);
                self.codegen_drop_map(local, &key_ty, &val_ty, func, local_map)
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

    fn codegen_drop_file_slot(&mut self, slot: LLVMValueRef) -> Result<()> {
        unsafe {
            let file_ty = self.get_struct_type("File")?;
            let handle_ptr = LLVMBuildStructGEP2(
                self.builder,
                file_ty,
                slot,
                0,
                CString::new("file.handle")?.as_ptr(),
            );
            let handle_ty = LLVMGetElementType(LLVMTypeOf(handle_ptr));
            let handle_val = LLVMBuildLoad2(
                self.builder,
                handle_ty,
                handle_ptr,
                CString::new("file.handle.load")?.as_ptr(),
            );
            let null_ptr = LLVMConstNull(handle_ty);
            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                handle_val,
                null_ptr,
                CString::new("file.handle.null")?.as_ptr(),
            );

            let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let drop_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("file.drop")?.as_ptr(),
            );
            let cont_bb = LLVMAppendBasicBlockInContext(
                self.context,
                func,
                CString::new("file.drop.cont")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, is_null, cont_bb, drop_bb);

            LLVMPositionBuilderAtEnd(self.builder, drop_bb);
            let fclose_fn = LLVMGetNamedFunction(self.module, CString::new("fclose")?.as_ptr());
            if !fclose_fn.is_null() {
                let mut args = vec![handle_val];
                let fn_ty = LLVMGetElementType(LLVMTypeOf(fclose_fn));
                LLVMBuildCall2(
                    self.builder,
                    fn_ty,
                    fclose_fn,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    CString::new("file.drop.close")?.as_ptr(),
                );
            }
            LLVMBuildStore(self.builder, null_ptr, handle_ptr);
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
                    let llvm_payload_ty = self.get_llvm_type(payload_type)?;
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
                Rvalue::Call { name, args } => {
                    if name == "argv" || name == "std::sys::argv" {
                        return self.codegen_sys_argv_value();
                    }

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

    fn codegen_map_init(
        &mut self,
        key_type: &Type,
        value_type: &Type,
        rvalue: &Rvalue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let inst_name = format!(
            "Map${}__{}",
            self.type_key(key_type),
            self.type_key(value_type)
        );
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&inst_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", inst_name))?;

        let alloca_name = CString::new("map.tmp")?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_map_ty, alloca_name.as_ptr()) };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_val = match rvalue {
            Rvalue::MapWithCapacity { capacity, .. } => {
                self.codegen_value(capacity, func, local_map)?
            }
            _ => zero,
        };
        let cap_val = self.ensure_usize(cap_val, usize_ty)?;

        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                alloca,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };

        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name)));
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                alloca,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, cap_val, cap_ptr) };

        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                alloca,
                2,
                CString::new("map.len").unwrap().as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, zero, len_ptr) };

        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let alloc_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.cap.alloc")?.as_ptr(),
            )
        };
        let zero_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.cap.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, zero_bb, alloc_bb) };

        // alloc path
        unsafe { LLVMPositionBuilderAtEnd(self.builder, alloc_bb) };
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
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        // zero-cap path
        unsafe { LLVMPositionBuilderAtEnd(self.builder, zero_bb) };
        let null_buckets = unsafe { LLVMConstNull(bucket_array_llvm_ty) };
        unsafe { LLVMBuildStore(self.builder, null_buckets, buckets_ptr) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        // done
        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };

        let load_name = CString::new("map.value")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_map_ty, alloca, load_name.as_ptr()) })
    }

    fn cast_int_to_u64(&mut self, val: LLVMValueRef, signed: bool) -> Result<LLVMValueRef> {
        let u64_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        unsafe {
            if LLVMTypeOf(val) == u64_ty {
                Ok(val)
            } else {
                let name = CString::new("hash.cast")?;
                let cast = if signed {
                    LLVMBuildSExt(self.builder, val, u64_ty, name.as_ptr())
                } else {
                    LLVMBuildZExt(self.builder, val, u64_ty, name.as_ptr())
                };
                Ok(cast)
            }
        }
    }

    fn codegen_map_hash(
        &mut self,
        key_type: &Type,
        key: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        match key_type {
            Type::I8 | Type::I32 | Type::I64 => {
                let val = self.codegen_value(key, func, local_map)?;
                self.cast_int_to_u64(val, true)
            }
            Type::U8 | Type::U32 | Type::U64 | Type::Usize | Type::Bool | Type::Char => {
                let val = self.codegen_value(key, func, local_map)?;
                self.cast_int_to_u64(val, false)
            }
            Type::Ref(_, _) | Type::RawPtr(_) | Type::Own(_) | Type::Shared(_) => {
                let val = self.codegen_value(key, func, local_map)?;
                let u64_ty = unsafe { LLVMInt64TypeInContext(self.context) };
                let name = CString::new("hash.ptr")?;
                Ok(unsafe { LLVMBuildPtrToInt(self.builder, val, u64_ty, name.as_ptr()) })
            }
            Type::Named(name) => {
                let hash_name = format!("{}::Hash::hash", name);
                let callee = functions
                    .get(&hash_name)
                    .copied()
                    .ok_or_else(|| anyhow!("missing hash method {}", hash_name))?;

                let fn_ty = if let Some(target_func) =
                    mir_module.functions.iter().find(|f| f.name == hash_name)
                {
                    self.llvm_function_type(target_func)?
                } else {
                    bail!("missing hash function {}", hash_name);
                };

                let arg_ptr = match key {
                    MirValue::Local(local_id) => *local_map
                        .get(local_id)
                        .ok_or_else(|| anyhow!("missing local for hash {:?}", local_id))?,
                    _ => {
                        let value = self.codegen_value(key, func, local_map)?;
                        let elem_ty = self.get_llvm_type(key_type)?;
                        let tmp = unsafe {
                            LLVMBuildAlloca(
                                self.builder,
                                elem_ty,
                                CString::new("hash.key.tmp")?.as_ptr(),
                            )
                        };
                        unsafe { LLVMBuildStore(self.builder, value, tmp) };
                        tmp
                    }
                };

                let mut args = vec![arg_ptr];
                let call_name = CString::new("hash.call")?;
                Ok(unsafe {
                    LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        callee,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        call_name.as_ptr(),
                    )
                })
            }
            _ => bail!("unsupported Map key type for hashing: {:?}", key_type),
        }
    }

    fn codegen_map_hash_from_ptr(
        &mut self,
        key_type: &Type,
        key_ptr: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        match key_type {
            Type::I8 | Type::I32 | Type::I64 => {
                let llvm_ty = self.get_llvm_type(key_type)?;
                let val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        llvm_ty,
                        key_ptr,
                        CString::new("hash.load")?.as_ptr(),
                    )
                };
                self.cast_int_to_u64(val, true)
            }
            Type::U8 | Type::U32 | Type::U64 | Type::Usize | Type::Bool | Type::Char => {
                let llvm_ty = self.get_llvm_type(key_type)?;
                let val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        llvm_ty,
                        key_ptr,
                        CString::new("hash.load")?.as_ptr(),
                    )
                };
                self.cast_int_to_u64(val, false)
            }
            Type::Ref(_, _) | Type::RawPtr(_) | Type::Own(_) | Type::Shared(_) => {
                let raw_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        self.get_llvm_type(key_type)?,
                        key_ptr,
                        CString::new("hash.ptr.load")?.as_ptr(),
                    )
                };
                let u64_ty = unsafe { LLVMInt64TypeInContext(self.context) };
                let name = CString::new("hash.ptr")?;
                Ok(unsafe { LLVMBuildPtrToInt(self.builder, raw_val, u64_ty, name.as_ptr()) })
            }
            Type::Named(name) => {
                let hash_name = format!("{}::Hash::hash", name);
                let callee = functions
                    .get(&hash_name)
                    .copied()
                    .ok_or_else(|| anyhow!("missing hash method {}", hash_name))?;

                let fn_ty = if let Some(target_func) =
                    mir_module.functions.iter().find(|f| f.name == hash_name)
                {
                    self.llvm_function_type(target_func)?
                } else {
                    bail!("missing hash function {}", hash_name);
                };

                let mut args = vec![key_ptr];
                let call_name = CString::new("hash.call")?;
                Ok(unsafe {
                    LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        callee,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        call_name.as_ptr(),
                    )
                })
            }
            _ => bail!("unsupported Map key type for hashing: {:?}", key_type),
        }
    }

    fn map_bucket_name(&self, key_type: &Type, value_type: &Type) -> String {
        format!(
            "MapBucket${}__{}",
            self.type_key(key_type),
            self.type_key(value_type)
        )
    }

    fn map_bucket_name_from_map(&self, map_name: &str) -> Result<String> {
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

    fn codegen_map_allocate_buckets(
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

    fn codegen_map_bucket_index(
        &mut self,
        map: LocalId,
        key_type: &Type,
        key: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let llvm_map_ty = self.get_struct_type(&struct_name)?;
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap.ptr")?.as_ptr(),
            )
        };
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap")?.as_ptr(),
            )
        };

        let hash_val =
            self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?;
        let hash_val = self.ensure_usize(hash_val, usize_ty)?;
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };
        let rem = unsafe {
            LLVMBuildURem(
                self.builder,
                hash_val,
                cap_val,
                CString::new("map.bucket.idx")?.as_ptr(),
            )
        };
        Ok(unsafe {
            LLVMBuildSelect(
                self.builder,
                cap_is_zero,
                zero,
                rem,
                CString::new("map.bucket.sel")?.as_ptr(),
            )
        })
    }

    fn codegen_map_stub(&mut self, rvalue: &Rvalue) -> Result<LLVMValueRef> {
        let llvm_ty = match rvalue {
            Rvalue::MapAdd { .. } | Rvalue::MapUpdate { .. } => {
                let inst_name = format!(
                    "{}${}",
                    self.sanitize("Result"),
                    vec![Type::Void, Type::Named("Err".into())]
                        .iter()
                        .map(|ty| self.type_key(ty))
                        .collect::<Vec<_>>()
                        .join("__")
                );
                self.get_enum_type(&inst_name)?
            }
            Rvalue::MapDel { value_type, .. } => {
                let inst_name = format!(
                    "{}${}",
                    self.sanitize("Result"),
                    vec![value_type.clone(), Type::Named("Err".into())]
                        .iter()
                        .map(|ty| self.type_key(ty))
                        .collect::<Vec<_>>()
                        .join("__")
                );
                self.get_enum_type(&inst_name)?
            }
            Rvalue::MapGet { value_type, .. } => {
                let inst_name = format!(
                    "{}${}",
                    self.sanitize("Option"),
                    vec![value_type.clone()]
                        .iter()
                        .map(|ty| self.type_key(ty))
                        .collect::<Vec<_>>()
                        .join("__")
                );
                self.get_enum_type(&inst_name)?
            }
            Rvalue::MapHas { .. } => unsafe { LLVMInt1TypeInContext(self.context) },
            Rvalue::MapKeys { key_type, .. } => {
                let inst_name = format!("Vec${}", self.type_key(key_type));
                self.get_struct_type(&inst_name)?
            }
            Rvalue::MapVals { value_type, .. } => {
                let inst_name = format!("Vec${}", self.type_key(value_type));
                self.get_struct_type(&inst_name)?
            }
            _ => unsafe { LLVMVoidTypeInContext(self.context) },
        };
        Ok(unsafe { LLVMConstNull(llvm_ty) })
    }

    fn codegen_map_add(
        &mut self,
        map: LocalId,
        key_type: &Type,
        key: &MirValue,
        value_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;

        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                2,
                CString::new("map.len").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let alloc_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.ensure.alloc")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.ensure.cont")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, alloc_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, alloc_bb) };
        let init_cap = unsafe { LLVMConstInt(usize_ty, MAP_INITIAL_BUCKETS, 0) };
        self.codegen_map_allocate_buckets(buckets_ptr, cap_ptr, init_cap, key_type, value_type)?;
        unsafe { LLVMBuildBr(self.builder, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let bucket_index = self
            .codegen_map_bucket_index(map, key_type, key, func, local_map, functions, mir_module)?;

        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![bucket_index].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };

        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.curr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let input_key_val = match key_type {
            Type::Named(_) => None,
            _ => Some(self.codegen_value(key, func, local_map)?),
        };
        let input_hash = match key_type {
            Type::Named(_) => {
                Some(self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?)
            }
            _ => None,
        };
        let key_val = self.codegen_value(key, func, local_map)?;
        let value_val = self.codegen_value(value, func, local_map)?;

        let check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.check")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.body")?.as_ptr(),
            )
        };
        let next_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.next")?.as_ptr(),
            )
        };
        let found_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.found")?.as_ptr(),
            )
        };
        let insert_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.insert")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.add.done")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, insert_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_match = match key_type {
            Type::Named(_) => {
                let bucket_hash =
                    self.codegen_map_hash_from_ptr(key_type, key_ptr, functions, mir_module)?;
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_hash,
                        input_hash.expect("hash for named key"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
            _ => {
                let key_llvm_ty = self.get_llvm_type(key_type)?;
                let bucket_key_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        key_llvm_ty,
                        key_ptr,
                        CString::new("map.key.load")?.as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_key_val,
                        input_key_val.expect("key value"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
        };
        unsafe { LLVMBuildCondBr(self.builder, key_match, found_bb, next_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, next_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, found_bb) };
        let err_val = self.codegen_err_value("key exists")?;
        let err_ty = Type::Named("Err".into());
        let ok_ty = Type::Void;
        let exists_val = self.codegen_result_err(&ok_ty, &err_ty, Some(err_val))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, insert_bb) };
        let bucket_size = unsafe { LLVMSizeOf(llvm_bucket_ty) };
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let mut malloc_args = vec![bucket_size];
        let raw_ptr = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("map.bucket.malloc")?.as_ptr(),
            )
        };
        let new_bucket = unsafe {
            LLVMBuildBitCast(
                self.builder,
                raw_ptr,
                bucket_head_llvm_ty,
                CString::new("map.bucket.cast")?.as_ptr(),
            )
        };
        let new_key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                new_bucket,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let new_val_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                new_bucket,
                1,
                CString::new("map.bucket.val")?.as_ptr(),
            )
        };
        let new_next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                new_bucket,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, key_val, new_key_ptr) };
        unsafe { LLVMBuildStore(self.builder, value_val, new_val_ptr) };
        unsafe { LLVMBuildStore(self.builder, head_val, new_next_ptr) };
        unsafe { LLVMBuildStore(self.builder, new_bucket, bucket_slot_ptr) };

        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("map.len.load")?.as_ptr(),
            )
        };
        let new_len = unsafe {
            LLVMBuildAdd(
                self.builder,
                len_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("map.len.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_len, len_ptr) };
        let ok_val = self.codegen_result_ok(&ok_ty, &err_ty, None)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let result_ty = self.get_enum_type(&format!(
            "Result${}__{}",
            self.type_key(&ok_ty),
            self.type_key(&err_ty)
        ))?;
        let phi = unsafe {
            let phi_name = CString::new("map.add.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut incoming_vals = vec![exists_val, ok_val];
            let mut incoming_bbs = vec![found_bb, insert_bb];
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

    fn codegen_map_update(
        &mut self,
        map: LocalId,
        key_type: &Type,
        key: &MirValue,
        value_type: &Type,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let missing_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.missing")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.cont")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, missing_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, missing_bb) };
        let err_val = self.codegen_err_value("key missing")?;
        let err_ty = Type::Named("Err".into());
        let ok_ty = Type::Void;
        let missing_val = self.codegen_result_err(&ok_ty, &err_ty, Some(err_val))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let bucket_index = self
            .codegen_map_bucket_index(map, key_type, key, func, local_map, functions, mir_module)?;

        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![bucket_index].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };

        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.curr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let input_key_val = match key_type {
            Type::Named(_) => None,
            _ => Some(self.codegen_value(key, func, local_map)?),
        };
        let input_hash = match key_type {
            Type::Named(_) => {
                Some(self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?)
            }
            _ => None,
        };
        let value_val = self.codegen_value(value, func, local_map)?;

        let check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.check")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.body")?.as_ptr(),
            )
        };
        let next_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.next")?.as_ptr(),
            )
        };
        let found_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.update.found")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, missing_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_match = match key_type {
            Type::Named(_) => {
                let bucket_hash =
                    self.codegen_map_hash_from_ptr(key_type, key_ptr, functions, mir_module)?;
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_hash,
                        input_hash.expect("hash for named key"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
            _ => {
                let key_llvm_ty = self.get_llvm_type(key_type)?;
                let bucket_key_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        key_llvm_ty,
                        key_ptr,
                        CString::new("map.key.load")?.as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_key_val,
                        input_key_val.expect("key value"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
        };
        unsafe { LLVMBuildCondBr(self.builder, key_match, found_bb, next_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, next_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, found_bb) };
        let value_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                1,
                CString::new("map.bucket.val")?.as_ptr(),
            )
        };
        self.codegen_drop_elem_slot(value_ptr, value_type)?;
        unsafe { LLVMBuildStore(self.builder, value_val, value_ptr) };
        let ok_val = self.codegen_result_ok(&ok_ty, &err_ty, None)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let result_ty = self.get_enum_type(&format!(
            "Result${}__{}",
            self.type_key(&ok_ty),
            self.type_key(&err_ty)
        ))?;
        let phi = unsafe {
            let phi_name = CString::new("map.update.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut incoming_vals = vec![missing_val, ok_val];
            let mut incoming_bbs = vec![missing_bb, found_bb];
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

    fn codegen_map_del(
        &mut self,
        map: LocalId,
        key_type: &Type,
        value_type: &Type,
        key: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                2,
                CString::new("map.len").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let missing_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.missing")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.cont")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, missing_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, missing_bb) };
        let err_val = self.codegen_err_value("key missing")?;
        let err_ty = Type::Named("Err".into());
        let ok_ty = value_type.clone();
        let missing_val = self.codegen_result_err(&ok_ty, &err_ty, Some(err_val))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let bucket_index = self
            .codegen_map_bucket_index(map, key_type, key, func, local_map, functions, mir_module)?;

        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![bucket_index].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };

        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.curr")?.as_ptr(),
            )
        };
        let prev_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.prev")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };
        unsafe { LLVMBuildStore(self.builder, LLVMConstNull(bucket_head_llvm_ty), prev_slot) };

        let input_key_val = match key_type {
            Type::Named(_) => None,
            _ => Some(self.codegen_value(key, func, local_map)?),
        };
        let input_hash = match key_type {
            Type::Named(_) => {
                Some(self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?)
            }
            _ => None,
        };

        let check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.check")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.body")?.as_ptr(),
            )
        };
        let next_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.next")?.as_ptr(),
            )
        };
        let found_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.found")?.as_ptr(),
            )
        };
        let unlink_head_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.unlink.head")?.as_ptr(),
            )
        };
        let unlink_prev_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.unlink.prev")?.as_ptr(),
            )
        };
        let unlink_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.del.unlink.done")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, missing_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_match = match key_type {
            Type::Named(_) => {
                let bucket_hash =
                    self.codegen_map_hash_from_ptr(key_type, key_ptr, functions, mir_module)?;
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_hash,
                        input_hash.expect("hash for named key"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
            _ => {
                let key_llvm_ty = self.get_llvm_type(key_type)?;
                let bucket_key_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        key_llvm_ty,
                        key_ptr,
                        CString::new("map.key.load")?.as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_key_val,
                        input_key_val.expect("key value"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
        };
        unsafe { LLVMBuildCondBr(self.builder, key_match, found_bb, next_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, next_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, current_val, prev_slot) };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, found_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        let prev_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                prev_slot,
                CString::new("map.prev.load")?.as_ptr(),
            )
        };
        let prev_is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                prev_val,
                null_bucket,
                CString::new("map.prev.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, prev_is_null, unlink_head_bb, unlink_prev_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, unlink_head_bb) };
        unsafe { LLVMBuildStore(self.builder, next_val, bucket_slot_ptr) };
        unsafe { LLVMBuildBr(self.builder, unlink_done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, unlink_prev_bb) };
        let prev_next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                prev_val,
                2,
                CString::new("map.prev.next")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, prev_next_ptr) };
        unsafe { LLVMBuildBr(self.builder, unlink_done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, unlink_done_bb) };
        let value_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                1,
                CString::new("map.bucket.val")?.as_ptr(),
            )
        };
        let value_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(value_type)?,
                value_ptr,
                CString::new("map.bucket.val.load")?.as_ptr(),
            )
        };
        self.codegen_drop_elem_slot(key_ptr, key_type)?;
        self.codegen_free(current_val)?;
        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("map.len.load")?.as_ptr(),
            )
        };
        let new_len = unsafe {
            LLVMBuildSub(
                self.builder,
                len_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("map.len.dec")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_len, len_ptr) };
        let ok_val = self.codegen_result_ok(&ok_ty, &err_ty, Some(value_val))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let result_ty = self.get_enum_type(&format!(
            "Result${}__{}",
            self.type_key(&ok_ty),
            self.type_key(&err_ty)
        ))?;
        let phi = unsafe {
            let phi_name = CString::new("map.del.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut incoming_vals = vec![missing_val, ok_val];
            let mut incoming_bbs = vec![missing_bb, unlink_done_bb];
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

    fn codegen_map_get(
        &mut self,
        map: LocalId,
        key_type: &Type,
        value_type: &Type,
        key: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let none_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.none")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.cont")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, none_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, none_bb) };
        let none_val = self.codegen_option_none(value_type, mir_module)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let bucket_index = self
            .codegen_map_bucket_index(map, key_type, key, func, local_map, functions, mir_module)?;

        let bucket_name = self.map_bucket_name(key_type, value_type);
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![bucket_index].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };

        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.curr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let input_key_val = match key_type {
            Type::Named(_) => None,
            _ => Some(self.codegen_value(key, func, local_map)?),
        };
        let input_hash = match key_type {
            Type::Named(_) => {
                Some(self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?)
            }
            _ => None,
        };

        let check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.check")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.body")?.as_ptr(),
            )
        };
        let next_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.next")?.as_ptr(),
            )
        };
        let found_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.get.found")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, none_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_match = match key_type {
            Type::Named(_) => {
                let bucket_hash =
                    self.codegen_map_hash_from_ptr(key_type, key_ptr, functions, mir_module)?;
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_hash,
                        input_hash.expect("hash for named key"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
            _ => {
                let key_llvm_ty = self.get_llvm_type(key_type)?;
                let bucket_key_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        key_llvm_ty,
                        key_ptr,
                        CString::new("map.key.load")?.as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_key_val,
                        input_key_val.expect("key value"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
        };
        unsafe { LLVMBuildCondBr(self.builder, key_match, found_bb, next_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, next_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, found_bb) };
        let value_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                1,
                CString::new("map.bucket.val")?.as_ptr(),
            )
        };
        let value_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(value_type)?,
                value_ptr,
                CString::new("map.bucket.val.load")?.as_ptr(),
            )
        };
        let some_val = self.codegen_option_some(value_type, value_val, mir_module)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let option_name = format!("Option${}", self.type_key(value_type));
        let llvm_option_ty = self.get_enum_type(&option_name)?;
        let phi = unsafe {
            let phi_name = CString::new("map.get.result")?;
            let phi_node = LLVMBuildPhi(self.builder, llvm_option_ty, phi_name.as_ptr());
            let mut incoming_vals = vec![none_val, some_val];
            let mut incoming_bbs = vec![none_bb, found_bb];
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

    fn codegen_map_has(
        &mut self,
        map: LocalId,
        key_type: &Type,
        key: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let false_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.false")?.as_ptr(),
            )
        };
        let cont_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.cont")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, cap_is_zero, false_bb, cont_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, false_bb) };
        let false_val = unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, cont_bb) };
        let bucket_index = self
            .codegen_map_bucket_index(map, key_type, key, func, local_map, functions, mir_module)?;

        let bucket_name = self.map_bucket_name_from_map(&struct_name)?;
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;

        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![bucket_index].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };

        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.curr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let input_key_val = match key_type {
            Type::Named(_) => None,
            _ => Some(self.codegen_value(key, func, local_map)?),
        };
        let input_hash = match key_type {
            Type::Named(_) => {
                Some(self.codegen_map_hash(key_type, key, func, local_map, functions, mir_module)?)
            }
            _ => None,
        };

        let check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.check")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.body")?.as_ptr(),
            )
        };
        let next_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.next")?.as_ptr(),
            )
        };
        let found_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.has.found")?.as_ptr(),
            )
        };

        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, false_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_match = match key_type {
            Type::Named(_) => {
                let bucket_hash =
                    self.codegen_map_hash_from_ptr(key_type, key_ptr, functions, mir_module)?;
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_hash,
                        input_hash.expect("hash for named key"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
            _ => {
                let key_llvm_ty = self.get_llvm_type(key_type)?;
                let bucket_key_val = unsafe {
                    LLVMBuildLoad2(
                        self.builder,
                        key_llvm_ty,
                        key_ptr,
                        CString::new("map.key.load")?.as_ptr(),
                    )
                };
                unsafe {
                    LLVMBuildICmp(
                        self.builder,
                        llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                        bucket_key_val,
                        input_key_val.expect("key value"),
                        CString::new("map.key.eq")?.as_ptr(),
                    )
                }
            }
        };
        unsafe { LLVMBuildCondBr(self.builder, key_match, found_bb, next_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, next_bb) };
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, found_bb) };
        let true_val = unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("map.has.result")?;
            let phi_node = LLVMBuildPhi(
                self.builder,
                LLVMInt1TypeInContext(self.context),
                phi_name.as_ptr(),
            );
            let mut incoming_vals = vec![false_val, true_val];
            let mut incoming_bbs = vec![false_bb, found_bb];
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

    fn codegen_map_keys(
        &mut self,
        map: LocalId,
        key_type: &Type,
        _value_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        _functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                2,
                CString::new("map.len").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("map.len.load")?.as_ptr(),
            )
        };

        let vec_val = self.codegen_vec_init_with_capacity_value(key_type, len_val)?;
        let vec_name = format!("Vec${}", self.type_key(key_type));
        let llvm_vec_ty = self.get_struct_type(&vec_name)?;
        let vec_ptr = unsafe {
            let alloca = LLVMBuildAlloca(
                self.builder,
                llvm_vec_ty,
                CString::new("map.keys.vec")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, vec_val, alloca);
            alloca
        };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let len_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                len_val,
                zero,
                CString::new("map.len.zero")?.as_ptr(),
            )
        };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };
        let skip_loop = unsafe {
            LLVMBuildOr(
                self.builder,
                len_is_zero,
                cap_is_zero,
                CString::new("map.keys.skip")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let loop_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.loop")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, skip_loop, done_bb, loop_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_bb) };
        let bucket_name = self.map_bucket_name_from_map(&struct_name)?;
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;
        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };

        let idx_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                usize_ty,
                CString::new("map.keys.idx")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, zero, idx_slot) };
        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.keys.curr")?.as_ptr(),
            )
        };

        let outer_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.check")?.as_ptr(),
            )
        };
        let outer_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.body")?.as_ptr(),
            )
        };
        let outer_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.outer.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, outer_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_check_bb) };
        let idx_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("map.keys.idx.load")?.as_ptr(),
            )
        };
        let idx_in_bounds = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                cap_val,
                CString::new("map.keys.idx.lt")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, idx_in_bounds, outer_body_bb, outer_done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_body_bb) };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let inner_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.inner.check")?.as_ptr(),
            )
        };
        let inner_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.inner.body")?.as_ptr(),
            )
        };
        let inner_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.keys.inner.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, inner_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.keys.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let inner_done = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.keys.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, inner_done, inner_done_bb, inner_body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let key_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                0,
                CString::new("map.bucket.key")?.as_ptr(),
            )
        };
        let key_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(key_type)?,
                key_ptr,
                CString::new("map.bucket.key.load")?.as_ptr(),
            )
        };
        self.codegen_vec_push_no_grow(vec_ptr, key_type, key_val)?;
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, inner_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_done_bb) };
        let new_idx = unsafe {
            LLVMBuildAdd(
                self.builder,
                idx_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("map.keys.idx.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_idx, idx_slot) };
        unsafe { LLVMBuildBr(self.builder, outer_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_done_bb) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let vec_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                CString::new("map.keys.load")?.as_ptr(),
            )
        };
        Ok(vec_val)
    }

    fn codegen_map_vals(
        &mut self,
        map: LocalId,
        _key_type: &Type,
        value_type: &Type,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        _functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (struct_name, map_ptr) = self.struct_pointer_for_local(map, func, local_map)?;
        let (llvm_map_ty, usize_ty) = self
            .get_map_layout(&struct_name)
            .ok_or_else(|| anyhow!("missing map layout for {}", struct_name))?;
        let buckets_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                0,
                CString::new("map.buckets").unwrap().as_ptr(),
            )
        };
        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                1,
                CString::new("map.cap").unwrap().as_ptr(),
            )
        };
        let len_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_map_ty,
                map_ptr,
                2,
                CString::new("map.len").unwrap().as_ptr(),
            )
        };
        let cap_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                cap_ptr,
                CString::new("map.cap.load")?.as_ptr(),
            )
        };
        let len_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("map.len.load")?.as_ptr(),
            )
        };

        let vec_val = self.codegen_vec_init_with_capacity_value(value_type, len_val)?;
        let vec_name = format!("Vec${}", self.type_key(value_type));
        let llvm_vec_ty = self.get_struct_type(&vec_name)?;
        let vec_ptr = unsafe {
            let alloca = LLVMBuildAlloca(
                self.builder,
                llvm_vec_ty,
                CString::new("map.vals.vec")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, vec_val, alloca);
            alloca
        };

        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };
        let len_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                len_val,
                zero,
                CString::new("map.len.zero")?.as_ptr(),
            )
        };
        let cap_is_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cap_val,
                zero,
                CString::new("map.cap.zero")?.as_ptr(),
            )
        };
        let skip_loop = unsafe {
            LLVMBuildOr(
                self.builder,
                len_is_zero,
                cap_is_zero,
                CString::new("map.vals.skip")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let loop_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.loop")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, skip_loop, done_bb, loop_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_bb) };
        let bucket_name = self.map_bucket_name_from_map(&struct_name)?;
        let bucket_head_ty = Type::RawPtr(Box::new(Type::Named(bucket_name.clone())));
        let bucket_head_llvm_ty = self.get_llvm_type(&bucket_head_ty)?;
        let bucket_array_ty = Type::RawPtr(Box::new(bucket_head_ty.clone()));
        let bucket_array_llvm_ty = self.get_llvm_type(&bucket_array_ty)?;
        let bucket_array = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_array_llvm_ty,
                buckets_ptr,
                CString::new("map.buckets.load")?.as_ptr(),
            )
        };

        let idx_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                usize_ty,
                CString::new("map.vals.idx")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, zero, idx_slot) };
        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                bucket_head_llvm_ty,
                CString::new("map.vals.curr")?.as_ptr(),
            )
        };

        let outer_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.check")?.as_ptr(),
            )
        };
        let outer_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.body")?.as_ptr(),
            )
        };
        let outer_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.outer.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, outer_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_check_bb) };
        let idx_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                idx_slot,
                CString::new("map.vals.idx.load")?.as_ptr(),
            )
        };
        let idx_in_bounds = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                idx_val,
                cap_val,
                CString::new("map.vals.idx.lt")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, idx_in_bounds, outer_body_bb, outer_done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_body_bb) };
        let bucket_slot_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_array,
                vec![idx_val].as_mut_ptr(),
                1,
                CString::new("map.bucket.slot")?.as_ptr(),
            )
        };
        let head_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                bucket_slot_ptr,
                CString::new("map.bucket.head")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, head_val, current_slot) };

        let inner_check_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.inner.check")?.as_ptr(),
            )
        };
        let inner_body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.inner.body")?.as_ptr(),
            )
        };
        let inner_done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("map.vals.inner.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, inner_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_check_bb) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                current_slot,
                CString::new("map.vals.curr.load")?.as_ptr(),
            )
        };
        let null_bucket = unsafe { LLVMConstNull(bucket_head_llvm_ty) };
        let inner_done = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                current_val,
                null_bucket,
                CString::new("map.vals.curr.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, inner_done, inner_done_bb, inner_body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_body_bb) };
        let llvm_bucket_ty = self.get_struct_type(&bucket_name)?;
        let val_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                1,
                CString::new("map.bucket.val")?.as_ptr(),
            )
        };
        let val_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(value_type)?,
                val_ptr,
                CString::new("map.bucket.val.load")?.as_ptr(),
            )
        };
        self.codegen_vec_push_no_grow(vec_ptr, value_type, val_val)?;
        let next_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_bucket_ty,
                current_val,
                2,
                CString::new("map.bucket.next")?.as_ptr(),
            )
        };
        let next_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                bucket_head_llvm_ty,
                next_ptr,
                CString::new("map.bucket.next.load")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_val, current_slot) };
        unsafe { LLVMBuildBr(self.builder, inner_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, inner_done_bb) };
        let new_idx = unsafe {
            LLVMBuildAdd(
                self.builder,
                idx_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("map.vals.idx.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_idx, idx_slot) };
        unsafe { LLVMBuildBr(self.builder, outer_check_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, outer_done_bb) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let vec_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                CString::new("map.vals.load")?.as_ptr(),
            )
        };
        Ok(vec_val)
    }

    fn codegen_drop_map(
        &mut self,
        _map: LocalId,
        _key_type: &Type,
        _value_type: &Type,
        _func: &MirFunction,
        _local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<()> {
        Ok(())
    }

    fn get_extern_function(
        &self,
        functions: &HashMap<String, LLVMValueRef>,
        name: &str,
    ) -> Result<LLVMValueRef> {
        functions
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("missing extern function {}", name))
    }

    fn file_handle_ptr(
        &mut self,
        file: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
    ) -> Result<(LLVMTypeRef, LLVMValueRef)> {
        let (struct_name, file_ptr) = self.struct_pointer_for_local(file, func, local_map)?;
        let llvm_file_ty = self.get_struct_type(&struct_name)?;
        let handle_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_file_ty,
                file_ptr,
                0,
                CString::new("file.handle")?.as_ptr(),
            )
        };
        let handle_ty = unsafe { LLVMGetElementType(LLVMTypeOf(handle_ptr)) };
        Ok((handle_ty, handle_ptr))
    }

    fn codegen_file_open(
        &mut self,
        path: &MirValue,
        create: bool,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let path_val = self.codegen_value(path, func, local_map)?;
        let mode_str = if create { "w" } else { "r" };
        let mode_val = self.codegen_string_literal(mode_str, &format!(".str.file.{}", mode_str))?;
        let fopen_fn = self.get_extern_function(functions, "fopen")?;
        let mut args = vec![path_val, mode_val];
        let fn_ty = unsafe { LLVMGetElementType(LLVMTypeOf(fopen_fn)) };
        let call = unsafe {
            LLVMBuildCall2(
                self.builder,
                fn_ty,
                fopen_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("file.open")?.as_ptr(),
            )
        };
        let null_ptr = unsafe { LLVMConstNull(LLVMTypeOf(call)) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                call,
                null_ptr,
                CString::new("file.open.null")?.as_ptr(),
            )
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let ok_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.open.ok")?.as_ptr(),
            )
        };
        let err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.open.err")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.open.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, err_bb, ok_bb) };

        let ok_type = Type::Named("File".into());
        let err_type = Type::Named("Err".into());
        let result_name = format!(
            "Result${}__{}",
            self.type_key(&ok_type),
            self.type_key(&err_type)
        );
        let result_ty = self.get_enum_type(&result_name)?;

        unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_bb) };
        let file_ty = self.get_struct_type("File")?;
        let file_alloca =
            unsafe { LLVMBuildAlloca(self.builder, file_ty, CString::new("file.tmp")?.as_ptr()) };
        let handle_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                file_ty,
                file_alloca,
                0,
                CString::new("file.handle")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, call, handle_ptr) };
        let file_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                file_ty,
                file_alloca,
                CString::new("file.val")?.as_ptr(),
            )
        };
        let ok_val = self.codegen_result_ok(&ok_type, &err_type, Some(file_val))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_bb) };
        let err_msg = if create {
            "failed to create file"
        } else {
            "failed to open file"
        };
        let err_payload = self.codegen_err_value(err_msg)?;
        let err_val = self.codegen_result_err(&ok_type, &err_type, Some(err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("file.open.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut vals = vec![err_val, ok_val];
            let mut bbs = vec![err_bb, ok_bb];
            LLVMAddIncoming(
                phi_node,
                vals.as_mut_ptr(),
                bbs.as_mut_ptr(),
                vals.len() as u32,
            );
            phi_node
        };
        Ok(phi)
    }

    fn codegen_file_read_to_string(
        &mut self,
        file: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (handle_ty, handle_ptr) = self.file_handle_ptr(file, func, local_map)?;
        let handle_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                handle_ty,
                handle_ptr,
                CString::new("file.handle.load")?.as_ptr(),
            )
        };
        let null_ptr = unsafe { LLVMConstNull(handle_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                handle_val,
                null_ptr,
                CString::new("file.null")?.as_ptr(),
            )
        };

        let ok_type = Type::String;
        let err_type = Type::Named("Err".into());
        let result_name = format!(
            "Result${}__{}",
            self.type_key(&ok_type),
            self.type_key(&err_type)
        );
        let result_ty = self.get_enum_type(&result_name)?;

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let err_null_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.err.null")?.as_ptr(),
            )
        };
        let seek_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.seek")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, err_null_bb, seek_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_null_bb) };
        let err_payload = self.codegen_err_value("file is closed")?;
        let err_val = self.codegen_result_err(&ok_type, &err_type, Some(err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, seek_bb) };
        let fseek_fn = self.get_extern_function(functions, "fseek")?;
        let ftell_fn = self.get_extern_function(functions, "ftell")?;
        let rewind_fn = self.get_extern_function(functions, "rewind")?;
        let fread_fn = self.get_extern_function(functions, "fread")?;
        let i32_ty = unsafe { LLVMInt32TypeInContext(self.context) };
        let i64_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let zero_i64 = unsafe { LLVMConstInt(i64_ty, 0, 0) };
        let seek_end = unsafe { LLVMConstInt(i32_ty, 2, 0) };
        let mut seek_args = vec![handle_val, zero_i64, seek_end];
        let seek_ty = unsafe { LLVMGetElementType(LLVMTypeOf(fseek_fn)) };
        let seek_res = unsafe {
            LLVMBuildCall2(
                self.builder,
                seek_ty,
                fseek_fn,
                seek_args.as_mut_ptr(),
                seek_args.len() as u32,
                CString::new("file.fseek")?.as_ptr(),
            )
        };
        let seek_ok = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                seek_res,
                LLVMConstInt(i32_ty, 0, 0),
                CString::new("file.seek.ok")?.as_ptr(),
            )
        };

        let seek_err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.seek.err")?.as_ptr(),
            )
        };
        let tell_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.tell")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, seek_ok, tell_bb, seek_err_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, seek_err_bb) };
        let seek_err_payload = self.codegen_err_value("failed to seek file")?;
        let seek_err_val = self.codegen_result_err(&ok_type, &err_type, Some(seek_err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, tell_bb) };
        let mut tell_args = vec![handle_val];
        let tell_ty = unsafe { LLVMGetElementType(LLVMTypeOf(ftell_fn)) };
        let size_val = unsafe {
            LLVMBuildCall2(
                self.builder,
                tell_ty,
                ftell_fn,
                tell_args.as_mut_ptr(),
                tell_args.len() as u32,
                CString::new("file.ftell")?.as_ptr(),
            )
        };
        let size_neg = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                size_val,
                LLVMConstInt(i64_ty, 0, 1),
                CString::new("file.size.neg")?.as_ptr(),
            )
        };
        let size_err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.size.err")?.as_ptr(),
            )
        };
        let alloc_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.alloc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, size_neg, size_err_bb, alloc_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, size_err_bb) };
        let size_err_payload = self.codegen_err_value("failed to read file")?;
        let size_err_val = self.codegen_result_err(&ok_type, &err_type, Some(size_err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, alloc_bb) };
        unsafe {
            let mut rewind_args = vec![handle_val];
            let rewind_ty = LLVMGetElementType(LLVMTypeOf(rewind_fn));
            LLVMBuildCall2(
                self.builder,
                rewind_ty,
                rewind_fn,
                rewind_args.as_mut_ptr(),
                rewind_args.len() as u32,
                CString::new("file.rewind")?.as_ptr(),
            );
        }

        let size_plus_one = unsafe {
            LLVMBuildAdd(
                self.builder,
                size_val,
                LLVMConstInt(i64_ty, 1, 0),
                CString::new("file.size.plus")?.as_ptr(),
            )
        };
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let mut malloc_args = vec![size_plus_one];
        let buf_ptr = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("file.buf")?.as_ptr(),
            )
        };
        let buf_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                buf_ptr,
                LLVMConstNull(LLVMTypeOf(buf_ptr)),
                CString::new("file.buf.null")?.as_ptr(),
            )
        };
        let buf_err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.buf.err")?.as_ptr(),
            )
        };
        let read_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.body")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, buf_null, buf_err_bb, read_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, buf_err_bb) };
        let buf_err_payload = self.codegen_err_value("failed to allocate buffer")?;
        let buf_err_val = self.codegen_result_err(&ok_type, &err_type, Some(buf_err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, read_bb) };
        let size_u32 = unsafe {
            LLVMBuildTrunc(
                self.builder,
                size_val,
                LLVMInt32TypeInContext(self.context),
                CString::new("file.size.u32")?.as_ptr(),
            )
        };
        let one_u32 = unsafe { LLVMConstInt(LLVMInt32TypeInContext(self.context), 1, 0) };
        let mut fread_args = vec![buf_ptr, one_u32, size_u32, handle_val];
        let fread_ty = unsafe { LLVMGetElementType(LLVMTypeOf(fread_fn)) };
        let read_count = unsafe {
            LLVMBuildCall2(
                self.builder,
                fread_ty,
                fread_fn,
                fread_args.as_mut_ptr(),
                fread_args.len() as u32,
                CString::new("file.fread")?.as_ptr(),
            )
        };
        let size_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                size_u32,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("file.size.zero")?.as_ptr(),
            )
        };
        let read_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                read_count,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("file.read.zero")?.as_ptr(),
            )
        };
        let read_err = unsafe {
            LLVMBuildAnd(
                self.builder,
                read_zero,
                LLVMBuildNot(
                    self.builder,
                    size_zero,
                    CString::new("file.size.notzero")?.as_ptr(),
                ),
                CString::new("file.read.err")?.as_ptr(),
            )
        };
        let read_err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.fail")?.as_ptr(),
            )
        };
        let read_ok_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.read.ok")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, read_err, read_err_bb, read_ok_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, read_err_bb) };
        self.codegen_free(buf_ptr)?;
        let read_err_payload = self.codegen_err_value("failed to read file")?;
        let read_err_val = self.codegen_result_err(&ok_type, &err_type, Some(read_err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, read_ok_bb) };
        let read_count64 = unsafe {
            LLVMBuildZExt(
                self.builder,
                read_count,
                i64_ty,
                CString::new("file.read.count64")?.as_ptr(),
            )
        };
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let null_ptr = unsafe { LLVMPointerType(i8_ty, 0) };
        let buf_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                buf_ptr,
                null_ptr,
                CString::new("file.buf.cast")?.as_ptr(),
            )
        };
        let null_idx_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![read_count64].as_mut_ptr(),
                1,
                CString::new("file.buf.term")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), null_idx_ptr) };
        let ok_val = self.codegen_result_ok(&ok_type, &err_type, Some(buf_ptr))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("file.read.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut vals = vec![
                err_val,
                seek_err_val,
                size_err_val,
                buf_err_val,
                read_err_val,
                ok_val,
            ];
            let mut bbs = vec![
                err_null_bb,
                seek_err_bb,
                size_err_bb,
                buf_err_bb,
                read_err_bb,
                read_ok_bb,
            ];
            LLVMAddIncoming(
                phi_node,
                vals.as_mut_ptr(),
                bbs.as_mut_ptr(),
                vals.len() as u32,
            );
            phi_node
        };
        Ok(phi)
    }

    fn codegen_file_write_string(
        &mut self,
        file: LocalId,
        contents: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (handle_ty, handle_ptr) = self.file_handle_ptr(file, func, local_map)?;
        let handle_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                handle_ty,
                handle_ptr,
                CString::new("file.handle.load")?.as_ptr(),
            )
        };
        let null_ptr = unsafe { LLVMConstNull(handle_ty) };

        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                handle_val,
                null_ptr,
                CString::new("file.null")?.as_ptr(),
            )
        };

        let ok_type = Type::U32;
        let err_type = Type::Named("Err".into());
        let result_name = format!(
            "Result${}__{}",
            self.type_key(&ok_type),
            self.type_key(&err_type)
        );
        let result_ty = self.get_enum_type(&result_name)?;

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.write.err")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.write.body")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.write.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, err_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_bb) };
        let err_payload = self.codegen_err_value("file is closed")?;
        let err_val = self.codegen_result_err(&ok_type, &err_type, Some(err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let contents_val = self.codegen_value(contents, func, local_map)?;
        let strlen_fn = self.get_extern_function(functions, "strlen")?;
        let mut strlen_args = vec![contents_val];
        let strlen_ty = unsafe { LLVMGetElementType(LLVMTypeOf(strlen_fn)) };
        let len_val = unsafe {
            LLVMBuildCall2(
                self.builder,
                strlen_ty,
                strlen_fn,
                strlen_args.as_mut_ptr(),
                strlen_args.len() as u32,
                CString::new("file.strlen")?.as_ptr(),
            )
        };
        let len_u32 = unsafe {
            LLVMBuildTrunc(
                self.builder,
                len_val,
                LLVMInt32TypeInContext(self.context),
                CString::new("file.len.u32")?.as_ptr(),
            )
        };
        let fwrite_fn = self.get_extern_function(functions, "fwrite")?;
        let one_u32 = unsafe { LLVMConstInt(LLVMInt32TypeInContext(self.context), 1, 0) };
        let mut fwrite_args = vec![contents_val, one_u32, len_u32, handle_val];
        let fwrite_ty = unsafe { LLVMGetElementType(LLVMTypeOf(fwrite_fn)) };
        let wrote = unsafe {
            LLVMBuildCall2(
                self.builder,
                fwrite_ty,
                fwrite_fn,
                fwrite_args.as_mut_ptr(),
                fwrite_args.len() as u32,
                CString::new("file.fwrite")?.as_ptr(),
            )
        };
        let wrote_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                wrote,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("file.write.zero")?.as_ptr(),
            )
        };
        let len_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                len_u32,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("file.len.zero")?.as_ptr(),
            )
        };
        let write_err = unsafe {
            LLVMBuildAnd(
                self.builder,
                wrote_zero,
                LLVMBuildNot(
                    self.builder,
                    len_zero,
                    CString::new("file.len.notzero")?.as_ptr(),
                ),
                CString::new("file.write.err")?.as_ptr(),
            )
        };
        let ok_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.write.ok")?.as_ptr(),
            )
        };
        let err_write_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.write.fail")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, write_err, err_write_bb, ok_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_write_bb) };
        let err_write_payload = self.codegen_err_value("failed to write file")?;
        let err_write_val =
            self.codegen_result_err(&ok_type, &err_type, Some(err_write_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_bb) };
        let ok_val = self.codegen_result_ok(&ok_type, &err_type, Some(wrote))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("file.write.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut vals = vec![err_val, err_write_val, ok_val];
            let mut bbs = vec![err_bb, err_write_bb, ok_bb];
            LLVMAddIncoming(
                phi_node,
                vals.as_mut_ptr(),
                bbs.as_mut_ptr(),
                vals.len() as u32,
            );
            phi_node
        };
        Ok(phi)
    }

    fn codegen_file_close(
        &mut self,
        file: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
        _mir_module: &MirModule,
    ) -> Result<LLVMValueRef> {
        let (handle_ty, handle_ptr) = self.file_handle_ptr(file, func, local_map)?;
        let handle_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                handle_ty,
                handle_ptr,
                CString::new("file.handle.load")?.as_ptr(),
            )
        };
        let null_ptr = unsafe { LLVMConstNull(handle_ty) };
        let is_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                handle_val,
                null_ptr,
                CString::new("file.null")?.as_ptr(),
            )
        };

        let ok_type = Type::I32;
        let err_type = Type::Named("Err".into());
        let result_name = format!(
            "Result${}__{}",
            self.type_key(&ok_type),
            self.type_key(&err_type)
        );
        let result_ty = self.get_enum_type(&result_name)?;

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let err_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.close.err")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.close.body")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.close.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, is_null, err_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_bb) };
        let err_payload = self.codegen_err_value("file already closed")?;
        let err_val = self.codegen_result_err(&ok_type, &err_type, Some(err_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let fclose_fn = self.get_extern_function(functions, "fclose")?;
        let mut fclose_args = vec![handle_val];
        let fclose_ty = unsafe { LLVMGetElementType(LLVMTypeOf(fclose_fn)) };
        let close_res = unsafe {
            LLVMBuildCall2(
                self.builder,
                fclose_ty,
                fclose_fn,
                fclose_args.as_mut_ptr(),
                fclose_args.len() as u32,
                CString::new("file.fclose")?.as_ptr(),
            )
        };
        let close_ok = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                close_res,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("file.close.ok")?.as_ptr(),
            )
        };
        let ok_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.close.ok.bb")?.as_ptr(),
            )
        };
        let err_close_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("file.close.fail")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, close_ok, ok_bb, err_close_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, err_close_bb) };
        let err_close_payload = self.codegen_err_value("failed to close file")?;
        let err_close_val =
            self.codegen_result_err(&ok_type, &err_type, Some(err_close_payload))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, ok_bb) };
        unsafe { LLVMBuildStore(self.builder, null_ptr, handle_ptr) };
        let ok_val = self.codegen_result_ok(&ok_type, &err_type, Some(close_res))?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("file.close.result")?;
            let phi_node = LLVMBuildPhi(self.builder, result_ty, phi_name.as_ptr());
            let mut vals = vec![err_val, err_close_val, ok_val];
            let mut bbs = vec![err_bb, err_close_bb, ok_bb];
            LLVMAddIncoming(
                phi_node,
                vals.as_mut_ptr(),
                bbs.as_mut_ptr(),
                vals.len() as u32,
            );
            phi_node
        };
        Ok(phi)
    }

    fn codegen_string_len_value(
        &mut self,
        value: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let strlen_fn = self.get_extern_function(functions, "strlen")?;
        let mut args = vec![value];
        let fn_ty = self.function_type_for("strlen")?;
        Ok(unsafe {
            LLVMBuildCall2(
                self.builder,
                fn_ty,
                strlen_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("str.len")?.as_ptr(),
            )
        })
    }

    fn codegen_string_copy_from_ptr_len(
        &mut self,
        ptr: LLVMValueRef,
        len: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let size_plus = unsafe {
            LLVMBuildAdd(
                self.builder,
                len,
                LLVMConstInt(LLVMTypeOf(len), 1, 0),
                CString::new("str.len.plus")?.as_ptr(),
            )
        };
        let mut malloc_args = vec![size_plus];
        let buf = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("str.alloc")?.as_ptr(),
            )
        };
        let memcpy_fn = self.get_extern_function(functions, "memcpy")?;
        let mut memcpy_args = vec![buf, ptr, len];
        let memcpy_ty = self.function_type_for("memcpy")?;
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args.as_mut_ptr(),
                memcpy_args.len() as u32,
                CString::new("str.memcpy")?.as_ptr(),
            );
        }
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let buf_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                buf,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.buf.cast")?.as_ptr(),
            )
        };
        let term_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![len].as_mut_ptr(),
                1,
                CString::new("str.term")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), term_ptr) };
        Ok(buf)
    }

    fn codegen_string_empty(
        &mut self,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let zero = unsafe { LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0) };
        let empty_ptr = self.codegen_string_literal("", ".str.empty")?;
        self.codegen_string_copy_from_ptr_len(empty_ptr, zero, functions)
    }

    fn codegen_string_len(
        &mut self,
        base: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        self.codegen_string_len_value(base_val, functions)
    }

    fn codegen_string_concat(
        &mut self,
        base: LocalId,
        value: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let left_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let right_val = self.codegen_value(value, func, local_map)?;
        let left_len = self.codegen_string_len_value(left_val, functions)?;
        let right_len = self.codegen_string_len_value(right_val, functions)?;
        let total_len = unsafe {
            LLVMBuildAdd(
                self.builder,
                left_len,
                right_len,
                CString::new("str.concat.len")?.as_ptr(),
            )
        };
        let malloc_fn = self.ensure_malloc_fn()?;
        let malloc_ty = self.malloc_function_type();
        let size_plus = unsafe {
            LLVMBuildAdd(
                self.builder,
                total_len,
                LLVMConstInt(LLVMTypeOf(total_len), 1, 0),
                CString::new("str.concat.size")?.as_ptr(),
            )
        };
        let mut malloc_args = vec![size_plus];
        let buf = unsafe {
            LLVMBuildCall2(
                self.builder,
                malloc_ty,
                malloc_fn,
                malloc_args.as_mut_ptr(),
                malloc_args.len() as u32,
                CString::new("str.concat.alloc")?.as_ptr(),
            )
        };
        let memcpy_fn = self.get_extern_function(functions, "memcpy")?;
        let memcpy_ty = self.function_type_for("memcpy")?;
        let mut memcpy_args = vec![buf, left_val, left_len];
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args.as_mut_ptr(),
                memcpy_args.len() as u32,
                CString::new("str.concat.copy1")?.as_ptr(),
            );
        }
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let buf_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                buf,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.concat.cast")?.as_ptr(),
            )
        };
        let right_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![left_len].as_mut_ptr(),
                1,
                CString::new("str.concat.ptr")?.as_ptr(),
            )
        };
        let mut memcpy_args2 = vec![right_ptr, right_val, right_len];
        unsafe {
            LLVMBuildCall2(
                self.builder,
                memcpy_ty,
                memcpy_fn,
                memcpy_args2.as_mut_ptr(),
                memcpy_args2.len() as u32,
                CString::new("str.concat.copy2")?.as_ptr(),
            );
        }
        let term_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                buf_i8,
                vec![total_len].as_mut_ptr(),
                1,
                CString::new("str.concat.term")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, LLVMConstInt(i8_ty, 0, 0), term_ptr) };
        Ok(buf)
    }

    fn codegen_string_slice(
        &mut self,
        base: LocalId,
        start: &MirValue,
        len: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let start_val = self.codegen_value(start, func, local_map)?;
        let len_val = self.codegen_value(len, func, local_map)?;
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let start_val = self.ensure_usize(start_val, usize_ty)?;
        let len_val = self.ensure_usize(len_val, usize_ty)?;
        let base_len = self.codegen_string_len_value(base_val, functions)?;

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let empty_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.slice.empty")?.as_ptr(),
            )
        };
        let body_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.slice.body")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.slice.done")?.as_ptr(),
            )
        };
        let start_ge = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGE,
                start_val,
                base_len,
                CString::new("str.slice.start.ge")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, start_ge, empty_bb, body_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, empty_bb) };
        let empty_val = self.codegen_string_empty(functions)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
        let remaining = unsafe {
            LLVMBuildSub(
                self.builder,
                base_len,
                start_val,
                CString::new("str.slice.rem")?.as_ptr(),
            )
        };
        let len_gt = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                len_val,
                remaining,
                CString::new("str.slice.len.gt")?.as_ptr(),
            )
        };
        let actual_len = unsafe {
            LLVMBuildSelect(
                self.builder,
                len_gt,
                remaining,
                len_val,
                CString::new("str.slice.len")?.as_ptr(),
            )
        };
        let i8_ty = unsafe { LLVMInt8TypeInContext(self.context) };
        let base_i8 = unsafe {
            LLVMBuildBitCast(
                self.builder,
                base_val,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.slice.cast")?.as_ptr(),
            )
        };
        let slice_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                i8_ty,
                base_i8,
                vec![start_val].as_mut_ptr(),
                1,
                CString::new("str.slice.ptr")?.as_ptr(),
            )
        };
        let slice_val = self.codegen_string_copy_from_ptr_len(slice_ptr, actual_len, functions)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let phi = unsafe {
            let phi_name = CString::new("str.slice.result")?;
            let phi_node = LLVMBuildPhi(
                self.builder,
                self.get_llvm_type(&Type::String)?,
                phi_name.as_ptr(),
            );
            let mut vals = vec![empty_val, slice_val];
            let mut bbs = vec![empty_bb, body_bb];
            LLVMAddIncoming(
                phi_node,
                vals.as_mut_ptr(),
                bbs.as_mut_ptr(),
                vals.len() as u32,
            );
            phi_node
        };
        Ok(phi)
    }

    fn codegen_string_trim(
        &mut self,
        base: LocalId,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let len_val = self.codegen_string_len_value(base_val, functions)?;
        let usize_ty = unsafe { LLVMInt64TypeInContext(self.context) };
        let zero = unsafe { LLVMConstInt(usize_ty, 0, 0) };

        let result = unsafe {
            let parent_bb = LLVMGetInsertBlock(self.builder);
            let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
            let empty_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.trim.empty")?.as_ptr(),
                )
            };
            let start_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.trim.start")?.as_ptr(),
                )
            };
            let done_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.trim.done")?.as_ptr(),
                )
            };
            let len_zero = unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    len_val,
                    zero,
                    CString::new("str.trim.len.zero")?.as_ptr(),
                )
            };
            unsafe { LLVMBuildCondBr(self.builder, len_zero, empty_bb, start_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, empty_bb) };
            let empty_val = self.codegen_string_empty(functions)?;
            unsafe { LLVMBuildBr(self.builder, done_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, start_bb) };
            let start_slot = LLVMBuildAlloca(
                self.builder,
                usize_ty,
                CString::new("str.trim.start.idx")?.as_ptr(),
            );
            let end_slot = LLVMBuildAlloca(
                self.builder,
                usize_ty,
                CString::new("str.trim.end.idx")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, zero, start_slot);
            LLVMBuildStore(self.builder, len_val, end_slot);

            let i8_ty = LLVMInt8TypeInContext(self.context);
            let base_i8 = LLVMBuildBitCast(
                self.builder,
                base_val,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.trim.cast")?.as_ptr(),
            );
            let isspace_fn = self.get_extern_function(functions, "isspace")?;
            let space_ty = self.function_type_for("isspace")?;

            let loop_start_check = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.start.check")?.as_ptr(),
            );
            let loop_start_body = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.start.body")?.as_ptr(),
            );
            let loop_start_done = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.start.done")?.as_ptr(),
            );
            LLVMBuildBr(self.builder, loop_start_check);

            LLVMPositionBuilderAtEnd(self.builder, loop_start_check);
            let start_val = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                start_slot,
                CString::new("str.trim.start.load")?.as_ptr(),
            );
            let end_val = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                end_slot,
                CString::new("str.trim.end.load")?.as_ptr(),
            );
            let start_lt = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntULT,
                start_val,
                end_val,
                CString::new("str.trim.start.lt")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, start_lt, loop_start_body, loop_start_done);

            LLVMPositionBuilderAtEnd(self.builder, loop_start_body);
            let char_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ty,
                base_i8,
                vec![start_val].as_mut_ptr(),
                1,
                CString::new("str.trim.char")?.as_ptr(),
            );
            let ch = LLVMBuildLoad2(
                self.builder,
                i8_ty,
                char_ptr,
                CString::new("str.trim.ch")?.as_ptr(),
            );
            let ch_i32 = LLVMBuildSExt(
                self.builder,
                ch,
                LLVMInt32TypeInContext(self.context),
                CString::new("str.trim.ch.i32")?.as_ptr(),
            );
            let mut space_args = vec![ch_i32];
            let space_res = LLVMBuildCall2(
                self.builder,
                space_ty,
                isspace_fn,
                space_args.as_mut_ptr(),
                space_args.len() as u32,
                CString::new("str.trim.space")?.as_ptr(),
            );
            let is_space = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntNE,
                space_res,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("str.trim.is_space")?.as_ptr(),
            );
            let inc_start_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.start.inc")?.as_ptr(),
            );
            let keep_start_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.start.keep")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, is_space, inc_start_bb, keep_start_bb);

            LLVMPositionBuilderAtEnd(self.builder, inc_start_bb);
            let start_next = LLVMBuildAdd(
                self.builder,
                start_val,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("str.trim.start.next")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, start_next, start_slot);
            LLVMBuildBr(self.builder, loop_start_check);

            LLVMPositionBuilderAtEnd(self.builder, keep_start_bb);
            LLVMBuildBr(self.builder, loop_start_done);

            LLVMPositionBuilderAtEnd(self.builder, loop_start_done);
            let loop_end_check = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.end.check")?.as_ptr(),
            );
            let loop_end_body = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.end.body")?.as_ptr(),
            );
            let loop_end_done = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.end.done")?.as_ptr(),
            );
            LLVMBuildBr(self.builder, loop_end_check);

            LLVMPositionBuilderAtEnd(self.builder, loop_end_check);
            let start_val2 = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                start_slot,
                CString::new("str.trim.start.load2")?.as_ptr(),
            );
            let end_val2 = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                end_slot,
                CString::new("str.trim.end.load2")?.as_ptr(),
            );
            let end_gt = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                end_val2,
                start_val2,
                CString::new("str.trim.end.gt")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, end_gt, loop_end_body, loop_end_done);

            LLVMPositionBuilderAtEnd(self.builder, loop_end_body);
            let end_minus = LLVMBuildSub(
                self.builder,
                end_val2,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("str.trim.end.minus")?.as_ptr(),
            );
            let end_char_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ty,
                base_i8,
                vec![end_minus].as_mut_ptr(),
                1,
                CString::new("str.trim.end.char")?.as_ptr(),
            );
            let end_ch = LLVMBuildLoad2(
                self.builder,
                i8_ty,
                end_char_ptr,
                CString::new("str.trim.end.ch")?.as_ptr(),
            );
            let end_ch_i32 = LLVMBuildSExt(
                self.builder,
                end_ch,
                LLVMInt32TypeInContext(self.context),
                CString::new("str.trim.end.ch.i32")?.as_ptr(),
            );
            let mut end_space_args = vec![end_ch_i32];
            let end_space_res = LLVMBuildCall2(
                self.builder,
                space_ty,
                isspace_fn,
                end_space_args.as_mut_ptr(),
                end_space_args.len() as u32,
                CString::new("str.trim.end.space")?.as_ptr(),
            );
            let end_is_space = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntNE,
                end_space_res,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("str.trim.end.is_space")?.as_ptr(),
            );
            let dec_end_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.end.dec")?.as_ptr(),
            );
            let keep_end_bb = LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.trim.end.keep")?.as_ptr(),
            );
            LLVMBuildCondBr(self.builder, end_is_space, dec_end_bb, keep_end_bb);

            LLVMPositionBuilderAtEnd(self.builder, dec_end_bb);
            LLVMBuildStore(self.builder, end_minus, end_slot);
            LLVMBuildBr(self.builder, loop_end_check);

            LLVMPositionBuilderAtEnd(self.builder, keep_end_bb);
            LLVMBuildBr(self.builder, loop_end_done);

            LLVMPositionBuilderAtEnd(self.builder, loop_end_done);
            let final_start = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                start_slot,
                CString::new("str.trim.start.final")?.as_ptr(),
            );
            let final_end = LLVMBuildLoad2(
                self.builder,
                usize_ty,
                end_slot,
                CString::new("str.trim.end.final")?.as_ptr(),
            );
            let final_len = LLVMBuildSub(
                self.builder,
                final_end,
                final_start,
                CString::new("str.trim.len")?.as_ptr(),
            );
            let trim_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ty,
                base_i8,
                vec![final_start].as_mut_ptr(),
                1,
                CString::new("str.trim.ptr")?.as_ptr(),
            );
            let trim_val = self.codegen_string_copy_from_ptr_len(trim_ptr, final_len, functions)?;
            LLVMBuildBr(self.builder, done_bb);

            LLVMPositionBuilderAtEnd(self.builder, done_bb);
            let phi = LLVMBuildPhi(
                self.builder,
                self.get_llvm_type(&Type::String)?,
                CString::new("str.trim.result")?.as_ptr(),
            );
            let mut vals = vec![empty_val, trim_val];
            let mut bbs = vec![empty_bb, loop_end_done];
            LLVMAddIncoming(phi, vals.as_mut_ptr(), bbs.as_mut_ptr(), vals.len() as u32);
            Ok(phi)
        };
        result
    }

    fn codegen_string_split(
        &mut self,
        base: LocalId,
        sep: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let sep_val = self.codegen_value(sep, func, local_map)?;
        let base_len = self.codegen_string_len_value(base_val, functions)?;
        let sep_len = self.codegen_string_len_value(sep_val, functions)?;

        let cap = unsafe {
            LLVMBuildAdd(
                self.builder,
                base_len,
                LLVMConstInt(LLVMTypeOf(base_len), 1, 0),
                CString::new("str.split.cap")?.as_ptr(),
            )
        };
        let vec_val = self.codegen_vec_init_with_capacity_value(&Type::String, cap)?;
        let vec_name = format!("Vec${}", self.type_key(&Type::String));
        let llvm_vec_ty = self.get_struct_type(&vec_name)?;
        let vec_ptr = unsafe {
            let alloca = LLVMBuildAlloca(
                self.builder,
                llvm_vec_ty,
                CString::new("str.split.vec")?.as_ptr(),
            );
            LLVMBuildStore(self.builder, vec_val, alloca);
            alloca
        };

        let parent_bb = unsafe { LLVMGetInsertBlock(self.builder) };
        let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
        let sep_zero = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                sep_len,
                LLVMConstInt(LLVMTypeOf(sep_len), 0, 0),
                CString::new("str.split.sep.zero")?.as_ptr(),
            )
        };
        let sep_zero_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.sep.zero.bb")?.as_ptr(),
            )
        };
        let loop_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.loop")?.as_ptr(),
            )
        };
        let done_bb = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.done")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, sep_zero, sep_zero_bb, loop_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, sep_zero_bb) };
        let dup_val = self.codegen_string_copy_from_ptr_len(base_val, base_len, functions)?;
        self.codegen_vec_push_no_grow(vec_ptr, &Type::String, dup_val)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_bb) };
        let current_slot = unsafe {
            LLVMBuildAlloca(
                self.builder,
                self.get_llvm_type(&Type::String)?,
                CString::new("str.split.curr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, base_val, current_slot) };

        let loop_check = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.check")?.as_ptr(),
            )
        };
        let loop_body = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.body")?.as_ptr(),
            )
        };
        let loop_end = unsafe {
            LLVMAppendBasicBlockInContext(
                self.context,
                parent_fn,
                CString::new("str.split.end")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildBr(self.builder, loop_check) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_check) };
        let current_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                self.get_llvm_type(&Type::String)?,
                current_slot,
                CString::new("str.split.curr.load")?.as_ptr(),
            )
        };
        let strstr_fn = self.get_extern_function(functions, "strstr")?;
        let mut strstr_args = vec![current_val, sep_val];
        let strstr_ty = self.function_type_for("strstr")?;
        let found = unsafe {
            LLVMBuildCall2(
                self.builder,
                strstr_ty,
                strstr_fn,
                strstr_args.as_mut_ptr(),
                strstr_args.len() as u32,
                CString::new("str.split.find")?.as_ptr(),
            )
        };
        let null_ptr = unsafe { LLVMConstNull(LLVMTypeOf(found)) };
        let found_null = unsafe {
            LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                found,
                null_ptr,
                CString::new("str.split.found.null")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildCondBr(self.builder, found_null, loop_end, loop_body) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_body) };
        let current_int = unsafe {
            LLVMBuildPtrToInt(
                self.builder,
                current_val,
                LLVMInt64TypeInContext(self.context),
                CString::new("str.split.curr.int")?.as_ptr(),
            )
        };
        let found_int = unsafe {
            LLVMBuildPtrToInt(
                self.builder,
                found,
                LLVMInt64TypeInContext(self.context),
                CString::new("str.split.found.int")?.as_ptr(),
            )
        };
        let seg_len = unsafe {
            LLVMBuildSub(
                self.builder,
                found_int,
                current_int,
                CString::new("str.split.seg.len")?.as_ptr(),
            )
        };
        let seg_val = self.codegen_string_copy_from_ptr_len(current_val, seg_len, functions)?;
        self.codegen_vec_push_no_grow(vec_ptr, &Type::String, seg_val)?;
        let next_ptr = unsafe {
            LLVMBuildGEP2(
                self.builder,
                LLVMInt8TypeInContext(self.context),
                found,
                vec![sep_len].as_mut_ptr(),
                1,
                CString::new("str.split.next")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, next_ptr, current_slot) };
        unsafe { LLVMBuildBr(self.builder, loop_check) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, loop_end) };
        let tail_len = self.codegen_string_len_value(current_val, functions)?;
        let tail_val = self.codegen_string_copy_from_ptr_len(current_val, tail_len, functions)?;
        self.codegen_vec_push_no_grow(vec_ptr, &Type::String, tail_val)?;
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let vec_val = unsafe {
            LLVMBuildLoad2(
                self.builder,
                llvm_vec_ty,
                vec_ptr,
                CString::new("str.split.load")?.as_ptr(),
            )
        };
        Ok(vec_val)
    }

    fn codegen_string_compare(
        &mut self,
        left: LLVMValueRef,
        right: LLVMValueRef,
        len: LLVMValueRef,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let memcmp_fn = self.get_extern_function(functions, "memcmp")?;
        let mut args = vec![left, right, len];
        let memcmp_ty = self.function_type_for("memcmp")?;
        let cmp_val = unsafe {
            LLVMBuildCall2(
                self.builder,
                memcmp_ty,
                memcmp_fn,
                args.as_mut_ptr(),
                args.len() as u32,
                CString::new("str.memcmp")?.as_ptr(),
            )
        };
        Ok(cmp_val)
    }

    fn codegen_string_starts_with(
        &mut self,
        base: LocalId,
        needle: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let needle_val = self.codegen_value(needle, func, local_map)?;
        let base_len = self.codegen_string_len_value(base_val, functions)?;
        let needle_len = self.codegen_string_len_value(needle_val, functions)?;
        let result = unsafe {
            let too_long = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                needle_len,
                base_len,
                CString::new("str.starts.gt")?.as_ptr(),
            );
            let parent_bb = LLVMGetInsertBlock(self.builder);
            let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
            let fail_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.starts.fail")?.as_ptr(),
                )
            };
            let body_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.starts.body")?.as_ptr(),
                )
            };
            let done_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.starts.done")?.as_ptr(),
                )
            };
            unsafe { LLVMBuildCondBr(self.builder, too_long, fail_bb, body_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, fail_bb) };
            let false_val = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
            unsafe { LLVMBuildBr(self.builder, done_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
            let cmp_val =
                self.codegen_string_compare(base_val, needle_val, needle_len, functions)?;
            let cmp_zero = unsafe {
                LLVMBuildICmp(
                    self.builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    cmp_val,
                    LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                    CString::new("str.starts.eq")?.as_ptr(),
                )
            };
            unsafe { LLVMBuildBr(self.builder, done_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
            let phi = unsafe {
                let phi_name = CString::new("str.starts.result")?;
                let phi_node = LLVMBuildPhi(
                    self.builder,
                    LLVMInt1TypeInContext(self.context),
                    phi_name.as_ptr(),
                );
                let mut vals = vec![false_val, cmp_zero];
                let mut bbs = vec![fail_bb, body_bb];
                LLVMAddIncoming(
                    phi_node,
                    vals.as_mut_ptr(),
                    bbs.as_mut_ptr(),
                    vals.len() as u32,
                );
                phi_node
            };
            Ok(phi)
        };
        result
    }

    fn codegen_string_ends_with(
        &mut self,
        base: LocalId,
        needle: &MirValue,
        func: &MirFunction,
        local_map: &HashMap<LocalId, LLVMValueRef>,
        functions: &HashMap<String, LLVMValueRef>,
    ) -> Result<LLVMValueRef> {
        let base_val = self.codegen_value(&MirValue::Local(base), func, local_map)?;
        let needle_val = self.codegen_value(needle, func, local_map)?;
        let base_len = self.codegen_string_len_value(base_val, functions)?;
        let needle_len = self.codegen_string_len_value(needle_val, functions)?;
        let result = unsafe {
            let too_long = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntUGT,
                needle_len,
                base_len,
                CString::new("str.ends.gt")?.as_ptr(),
            );
            let parent_bb = LLVMGetInsertBlock(self.builder);
            let parent_fn = unsafe { LLVMGetBasicBlockParent(parent_bb) };
            let fail_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.ends.fail")?.as_ptr(),
                )
            };
            let body_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.ends.body")?.as_ptr(),
                )
            };
            let done_bb = unsafe {
                LLVMAppendBasicBlockInContext(
                    self.context,
                    parent_fn,
                    CString::new("str.ends.done")?.as_ptr(),
                )
            };
            unsafe { LLVMBuildCondBr(self.builder, too_long, fail_bb, body_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, fail_bb) };
            let false_val = LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0);
            unsafe { LLVMBuildBr(self.builder, done_bb) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, body_bb) };
            let i8_ty = LLVMInt8TypeInContext(self.context);
            let base_i8 = LLVMBuildBitCast(
                self.builder,
                base_val,
                LLVMPointerType(i8_ty, 0),
                CString::new("str.ends.cast")?.as_ptr(),
            );
            let offset = LLVMBuildSub(
                self.builder,
                base_len,
                needle_len,
                CString::new("str.ends.offset")?.as_ptr(),
            );
            let tail_ptr = LLVMBuildGEP2(
                self.builder,
                i8_ty,
                base_i8,
                vec![offset].as_mut_ptr(),
                1,
                CString::new("str.ends.ptr")?.as_ptr(),
            );
            let cmp_val =
                self.codegen_string_compare(tail_ptr, needle_val, needle_len, functions)?;
            let cmp_zero = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                cmp_val,
                LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                CString::new("str.ends.eq")?.as_ptr(),
            );
            LLVMBuildBr(self.builder, done_bb);

            LLVMPositionBuilderAtEnd(self.builder, done_bb);
            let phi = LLVMBuildPhi(
                self.builder,
                LLVMInt1TypeInContext(self.context),
                CString::new("str.ends.result")?.as_ptr(),
            );
            let mut vals = vec![false_val, cmp_zero];
            let mut bbs = vec![fail_bb, body_bb];
            LLVMAddIncoming(phi, vals.as_mut_ptr(), bbs.as_mut_ptr(), vals.len() as u32);
            Ok(phi)
        };
        result
    }

    fn codegen_vec_init_with_capacity_value(
        &mut self,
        elem_type: &Type,
        cap_val: LLVMValueRef,
    ) -> Result<LLVMValueRef> {
        let inst_name = format!("Vec${}", self.type_key(elem_type));
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&inst_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", inst_name))?;

        let alloca_name = CString::new("vec.tmp")?;
        let alloca = unsafe { LLVMBuildAlloca(self.builder, llvm_vec_ty, alloca_name.as_ptr()) };

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

        let cap_ptr = unsafe {
            LLVMBuildStructGEP2(
                self.builder,
                llvm_vec_ty,
                alloca,
                2,
                CString::new("vec.cap").unwrap().as_ptr(),
            )
        };
        let cap_val = self.ensure_usize(cap_val, usize_ty)?;
        unsafe { LLVMBuildStore(self.builder, cap_val, cap_ptr) };

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

        unsafe { LLVMPositionBuilderAtEnd(self.builder, zero_bb) };
        let null_data = unsafe { LLVMConstNull(elem_ptr_ty) };
        unsafe { LLVMBuildStore(self.builder, null_data, data_field_ptr) };
        unsafe { LLVMBuildBr(self.builder, done_bb) };

        unsafe { LLVMPositionBuilderAtEnd(self.builder, done_bb) };
        let load_name = CString::new("vec.value")?;
        Ok(unsafe { LLVMBuildLoad2(self.builder, llvm_vec_ty, alloca, load_name.as_ptr()) })
    }

    fn codegen_vec_push_no_grow(
        &mut self,
        vec_ptr: LLVMValueRef,
        elem_type: &Type,
        value: LLVMValueRef,
    ) -> Result<()> {
        let inst_name = format!("Vec${}", self.type_key(elem_type));
        let (llvm_vec_ty, usize_ty) = self
            .get_vec_layout(&inst_name)
            .ok_or_else(|| anyhow!("missing vec layout for {}", inst_name))?;

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
        let current_len = unsafe {
            LLVMBuildLoad2(
                self.builder,
                usize_ty,
                len_ptr,
                CString::new("vec.len.load")?.as_ptr(),
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
                vec![current_len].as_mut_ptr(),
                1,
                CString::new("vec.elem.ptr")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, value, elem_ptr) };
        let new_len = unsafe {
            LLVMBuildAdd(
                self.builder,
                current_len,
                LLVMConstInt(usize_ty, 1, 0),
                CString::new("vec.len.inc")?.as_ptr(),
            )
        };
        unsafe { LLVMBuildStore(self.builder, new_len, len_ptr) };
        Ok(())
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
            Type::Named(name) if name == "File" => self.codegen_drop_file_slot(slot),
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

    fn ensure_strdup_fn(&mut self) -> Result<LLVMValueRef> {
        if let Some(func) = self.strdup_fn {
            return Ok(func);
        }
        unsafe {
            let name = CString::new("strdup")?;
            let existing = LLVMGetNamedFunction(self.module, name.as_ptr());
            if !existing.is_null() {
                self.strdup_fn = Some(existing);
                return Ok(existing);
            }
            let fn_ty = self.strdup_function_type();
            let func = LLVMAddFunction(self.module, name.as_ptr(), fn_ty);
            self.strdup_fn = Some(func);
            Ok(func)
        }
    }

    fn strdup_function_type(&self) -> LLVMTypeRef {
        unsafe {
            let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let mut params = vec![i8_ptr];
            LLVMFunctionType(i8_ptr, params.as_mut_ptr(), params.len() as u32, 0)
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
            let call_name = CString::new("")?;
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

fn tuple_struct_name_codegen(elem_types: &[Type]) -> String {
    if elem_types.is_empty() {
        return "unit".to_string();
    }

    let type_names: Vec<String> = elem_types.iter().map(type_key_simple_codegen).collect();
    format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
}

fn type_key_simple_codegen(ty: &Type) -> String {
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
        Type::Named(n) => n.replace("::", "_"),
        Type::Enum(n) => format!("enum_{}", n.replace("::", "_")),
        Type::Param(p) => format!("P_{}", p),
        Type::Ref(inner, _) => format!("ref_{}", type_key_simple_codegen(inner)),
        Type::Array(inner, size) => format!("arr{}_{}", size, type_key_simple_codegen(inner)),
        Type::Own(inner) => format!("own_{}", type_key_simple_codegen(inner)),
        Type::RawPtr(inner) => format!("rawptr_{}", type_key_simple_codegen(inner)),
        Type::Shared(inner) => format!("shared_{}", type_key_simple_codegen(inner)),
        Type::App { base, args } => {
            let args: Vec<String> = args.iter().map(type_key_simple_codegen).collect();
            format!("app_{}_{}", base.replace("::", "_"), args.join("__"))
        }
        Type::Tuple(elem_types) => {
            if elem_types.is_empty() {
                "unit".into()
            } else {
                let type_names: Vec<String> = elem_types.iter().map(type_key_simple_codegen).collect();
                format!("__Tuple{}_{}", elem_types.len(), type_names.join("_"))
            }
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

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use std::collections::HashMap;

use crate::codegen::BuilderBackend;
use crate::types::Type;

pub struct LlvmGenerator<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    named_values: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> LlvmGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            named_values: HashMap::new(),
        }
    }

    fn get_llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::I8 | Type::U8 => self.context.i8_type().as_basic_type_enum(),
            Type::I16 | Type::U16 => self.context.i16_type().as_basic_type_enum(),
            Type::I32 | Type::U32 => self.context.i32_type().as_basic_type_enum(),
            Type::I64 | Type::U64 => self.context.i64_type().as_basic_type_enum(),
            Type::F32 => self.context.f32_type().as_basic_type_enum(),
            Type::F64 => self.context.f64_type().as_basic_type_enum(),
            Type::Bool => self.context.bool_type().as_basic_type_enum(),
            Type::String => {
                let i8_ptr = self.context.ptr_type(inkwell::AddressSpace::from(0));
                let i32_type = self.context.i32_type();

                self.context
                    .struct_type(&[i8_ptr.into(), i32_type.into()], false)
                    .as_basic_type_enum()
            }
            Type::Void => panic!("Void type cannot represent raw values."),
            Type::Char => self.context.i8_type().as_basic_type_enum(),

            Type::Ref(_) => self
                .context
                .ptr_type(AddressSpace::from(0))
                .as_basic_type_enum(),
        }
    }
}

impl<'ctx> BuilderBackend for LlvmGenerator<'ctx> {
    type Value = BasicValueEnum<'ctx>;
    type TypeRepresentation = BasicTypeEnum<'ctx>;
    type BasicBlock = inkwell::basic_block::BasicBlock<'ctx>;

    fn is_block_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .and_then(|b| b.get_terminator())
            .is_some()
    }

    fn get_variable_ptr(&self, name: &str) -> Self::Value {
        self.named_values
            .get(name)
            .unwrap_or_else(|| panic!("Undefined variable: {}", name))
            .as_basic_value_enum()
    }

    fn append_basic_block(&self, name: &str) -> Self::BasicBlock {
        let current_fn = self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_parent())
            .expect("Cannot create basic blocks outside of a function context.");

        self.context.append_basic_block(current_fn, name)
    }

    fn position_at_end(&self, block: &Self::BasicBlock) {
        self.builder.position_at_end(*block);
    }

    fn build_conditional_branch(
        &self,
        cond: Self::Value,
        then_block: &Self::BasicBlock,
        else_block: &Self::BasicBlock,
    ) {
        let int_cond = cond.into_int_value();
        self.builder
            .build_conditional_branch(int_cond, *then_block, *else_block)
            .unwrap();
    }

    fn build_unconditional_branch(&self, target_block: &Self::BasicBlock) {
        self.builder
            .build_unconditional_branch(*target_block)
            .unwrap();
    }

    fn begin_function(
        &mut self,
        name: &str,
        parameters: &[(String, Type)],
        is_extern: bool,
        is_variadic: bool,
        return_type: &Type,
    ) -> Vec<Self::Value> {
        let param_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = parameters
            .iter()
            .map(|(_, ty)| {
                if is_extern && matches!(ty, Type::String) {
                    self.context
                        .ptr_type(inkwell::AddressSpace::from(0))
                        .as_basic_type_enum()
                } else {
                    self.get_llvm_type(ty)
                }
            })
            .collect();

        let param_metadata: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            param_types.iter().map(|ty| (*ty).into()).collect();

        let fn_type = match return_type {
            Type::Void => self
                .context
                .void_type()
                .fn_type(&param_metadata, is_variadic),
            _ => {
                let llvm_ret = self.get_llvm_type(return_type);
                llvm_ret.fn_type(&param_metadata, is_variadic)
            }
        };

        let function = self.module.add_function(name, fn_type, None);

        if is_extern {
            function.set_linkage(Linkage::External);
        } else {
            let entry_block = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry_block);
        }

        function.get_param_iter().map(|param| param).collect()
    }

    fn end_function(&mut self) {
        if let Some(current_fn) = self.builder.get_insert_block().and_then(|b| b.get_parent()) {
            if !current_fn.verify(true) {
                panic!(
                    "LLVM Function verification failed for: {:?}",
                    current_fn.get_name()
                );
            }
        }

        self.named_values.clear();
    }

    fn build_add(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "fadd")
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_sub(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "fsub")
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_mul(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "fmul")
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_div(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "fdiv")
                .unwrap()
                .as_basic_value_enum()
        } else if ty.is_unsigned_integer() {
            self.builder
                .build_int_unsigned_div(lhs.into_int_value(), rhs.into_int_value(), "udiv")
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "sdiv")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_eq(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::OEQ,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "feq",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "eq",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_neq(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::ONE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "fneq",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "neq",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_lt(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::OLT,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "flt",
                )
                .unwrap()
                .as_basic_value_enum()
        } else if ty.is_unsigned_integer() {
            self.builder
                .build_int_compare(
                    IntPredicate::ULT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ult",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::SLT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "slt",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_lte(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::OLE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "fle",
                )
                .unwrap()
                .as_basic_value_enum()
        } else if ty.is_unsigned_integer() {
            self.builder
                .build_int_compare(
                    IntPredicate::ULE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ule",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "sle",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_gt(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::OGT,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "fgt",
                )
                .unwrap()
                .as_basic_value_enum()
        } else if ty.is_unsigned_integer() {
            self.builder
                .build_int_compare(
                    IntPredicate::UGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ugt",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::SGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "sgt",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_gte(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_compare(
                    FloatPredicate::OGE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "fge",
                )
                .unwrap()
                .as_basic_value_enum()
        } else if ty.is_unsigned_integer() {
            self.builder
                .build_int_compare(
                    IntPredicate::UGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "uge",
                )
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_compare(
                    IntPredicate::SGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "sge",
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_not(&self, val: Self::Value) -> Self::Value {
        self.builder
            .build_not(val.into_int_value(), "not")
            .unwrap()
            .as_basic_value_enum()
    }

    fn build_neg(&self, val: Self::Value, ty: &Type) -> Self::Value {
        if ty.is_float() {
            self.builder
                .build_float_neg(val.into_float_value(), "fneg")
                .unwrap()
                .as_basic_value_enum()
        } else {
            self.builder
                .build_int_neg(val.into_int_value(), "neg")
                .unwrap()
                .as_basic_value_enum()
        }
    }

    fn build_and(&self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.builder
            .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
            .unwrap()
            .as_basic_value_enum()
    }

    fn build_or(&self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.builder
            .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
            .unwrap()
            .as_basic_value_enum()
    }

    fn build_alloca(&mut self, name: &str, ty: &Type) {
        let llvm_type = self.get_llvm_type(ty);
        let alloca = self.builder.build_alloca(llvm_type, name).unwrap();
        self.named_values.insert(name.to_string(), alloca);
    }

    fn build_store(&mut self, name: &str, value: Self::Value) {
        let ptr = self
            .named_values
            .get(name)
            .expect("Variables must be allocated before assignment");
        self.builder.build_store(*ptr, value).unwrap();
    }

    fn build_load(&self, name: &str, ty: &Type) -> Self::Value {
        let ptr = self
            .named_values
            .get(name)
            .expect("Undefined variable allocation lookup");
        let llvm_type = self.get_llvm_type(ty);
        self.builder.build_load(llvm_type, *ptr, name).unwrap()
    }

    fn build_call(&self, name: &str, args: Vec<Self::Value>, return_type: &Type) -> Self::Value {
        let function = self
            .module
            .get_function(name)
            .unwrap_or_else(|| panic!("Undefined function: {}", name));

        let is_extern = function.get_linkage() == inkwell::module::Linkage::External;

        let processed_args: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = args
            .iter()
            .enumerate()
            .map(|(_, arg)| {
                if is_extern && arg.is_struct_value() {
                    let struct_val = arg.into_struct_value();
                    let raw_ptr = self
                        .builder
                        .build_extract_value(struct_val, 0, "abi_str_ptr")
                        .unwrap();
                    raw_ptr.into()
                } else {
                    (*arg).into()
                }
            })
            .collect();

        let call_site = self
            .builder
            .build_call(function, &processed_args, "call")
            .unwrap();

        match return_type {
            Type::Void => {
                // Return an integer 0 or similar dummy value if your trait mandates returning a Self::Value
                self.context
                    .i32_type()
                    .const_int(0, false)
                    .as_basic_value_enum()
            }
            _ => call_site.try_as_basic_value().unwrap_basic(),
        }
    }

    fn build_return(&mut self, value: Option<Self::Value>) {
        match value {
            Some(llvm_val) => {
                self.builder
                    .build_return(Some(&llvm_val))
                    .expect("Error: Failed to emit value return instruction.");
            }
            None => {
                self.builder
                    .build_return(None)
                    .expect("Error: Failed to emit void return instruction.");
            }
        }
    }

    fn const_number(&self, val: f64, ty: &Type) -> Self::Value {
        if ty.is_float() {
            let float_type = self.get_llvm_type(ty).into_float_type();
            BasicValueEnum::FloatValue(float_type.const_float(val))
        } else {
            let int_type = self.get_llvm_type(ty).into_int_type();
            BasicValueEnum::IntValue(int_type.const_int(val as u64, false))
        }
    }

    fn const_string(&self, val: String) -> Self::Value {
        let context = &self.context;
        let i32_type = context.i32_type();

        let global_string = self
            .builder
            .build_global_string_ptr(val.as_str(), "str")
            .unwrap();
        let ptr_value = global_string.as_pointer_value();

        let len_value = i32_type.const_int(val.len() as u64, false);

        let struct_type =
            context.struct_type(&[ptr_value.get_type().into(), i32_type.into()], false);

        let struct_undef = struct_type.get_undef();

        let struct_with_ptr = self
            .builder
            .build_insert_value(struct_undef, ptr_value, 0, "str_struct_ptr")
            .unwrap();

        let final_struct = self
            .builder
            .build_insert_value(struct_with_ptr, len_value, 1, "str_struct_len")
            .unwrap();

        final_struct.into_struct_value().as_basic_value_enum()
    }

    fn const_bool(&self, val: bool) -> Self::Value {
        BasicValueEnum::IntValue(self.context.bool_type().const_int(val as u64, false))
    }

    fn const_void(&self) -> Self::Value {
        self.context.i32_type().const_zero().as_basic_value_enum()
    }

    fn const_char(&self, val: char) -> Self::Value {
        self.context
            .i8_type()
            .const_int(val as u64, false)
            .as_basic_value_enum()
    }
}

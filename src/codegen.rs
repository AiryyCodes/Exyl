use crate::types::Type;

pub trait BuilderBackend {
    type Value;
    type TypeRepresentation;
    type BasicBlock;

    fn is_block_terminated(&self) -> bool;

    fn get_variable_ptr(&self, name: &str) -> Self::Value;

    // Control flow
    fn append_basic_block(&self, name: &str) -> Self::BasicBlock;
    fn position_at_end(&self, block: &Self::BasicBlock);
    fn build_conditional_branch(
        &self,
        cond: Self::Value,
        then_block: &Self::BasicBlock,
        else_block: &Self::BasicBlock,
    );
    fn build_unconditional_branch(&self, target_block: &Self::BasicBlock);

    fn begin_function(
        &mut self,
        name: &str,
        parameters: &[(String, Type)],
        is_extern: bool,
        is_variadic: bool,
        return_type: &Type,
    ) -> Vec<Self::Value>;

    fn end_function(&mut self);

    // Primitive math abstractions
    fn build_add(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_sub(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_mul(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_div(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_eq(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_neq(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_lt(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_lte(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_gt(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;
    fn build_gte(&self, lhs: Self::Value, rhs: Self::Value, ty: &Type) -> Self::Value;

    fn build_not(&self, val: Self::Value) -> Self::Value;
    fn build_neg(&self, val: Self::Value, ty: &Type) -> Self::Value;

    fn build_and(&self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;
    fn build_or(&self, lhs: Self::Value, rhs: Self::Value) -> Self::Value;

    // Memory operations
    fn build_alloca(&mut self, name: &str, ty: &Type);
    fn build_store(&mut self, name: &str, value: Self::Value);
    fn build_load(&self, name: &str, ty: &Type) -> Self::Value;

    fn build_call(&self, name: &str, args: Vec<Self::Value>, return_type: &Type) -> Self::Value;
    fn build_return(&mut self, value: Option<Self::Value>);

    // Constant generation
    fn const_number(&self, val: f64, ty: &Type) -> Self::Value;
    fn const_string(&self, val: String) -> Self::Value;
    fn const_bool(&self, val: bool) -> Self::Value;
    fn const_void(&self) -> Self::Value;
    fn const_char(&self, val: char) -> Self::Value;
}

pub trait Emit<B: BuilderBackend> {
    type Output;
    fn emit(&self, backend: &mut B) -> Self::Output;
}

use crate::ast::Type;

pub struct BuiltinFunction {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

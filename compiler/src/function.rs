use crate::ast::Type;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub param_types: Vec<Type>,
    pub inferred_return: Type,
    pub return_type_annotation: Option<Type>, // what user explicitly wrote
    pub is_builtin: bool,
    pub is_variadic: bool,
}

impl FunctionInfo {
    pub fn new_user(
        name: String,
        param_types: Vec<Type>,
        return_type_annotation: Option<Type>,
        inferred_return: Type,
        is_variadic: bool,
    ) -> Self {
        Self {
            name,
            param_types,
            inferred_return,
            return_type_annotation,
            is_builtin: false,
            is_variadic,
        }
    }

    pub fn new_builtin(
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
        is_variadic: bool,
    ) -> Self {
        Self {
            name,
            param_types,
            inferred_return: return_type.clone(),
            return_type_annotation: Some(return_type),
            is_builtin: true,
            is_variadic,
        }
    }
}

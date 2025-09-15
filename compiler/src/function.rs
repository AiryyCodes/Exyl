use crate::ast::Type;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,                         // function name
    pub param_types: Vec<Type>,               // argument types
    pub inferred_return: Type,                // return type inferred from body
    pub return_type_annotation: Option<Type>, // what user explicitly wrote
    pub is_builtin: bool,                     // true for builtins
}

impl FunctionInfo {
    pub fn new_user(
        name: String,
        param_types: Vec<Type>,
        return_type_annotation: Option<Type>,
        inferred_return: Type,
    ) -> Self {
        Self {
            name,
            param_types,
            inferred_return,
            return_type_annotation,
            is_builtin: false,
        }
    }

    pub fn new_builtin(name: String, param_types: Vec<Type>, return_type: Type) -> Self {
        Self {
            name,
            param_types,
            inferred_return: return_type.clone(),
            return_type_annotation: Some(return_type),
            is_builtin: true,
        }
    }
}

use crate::{
    codegen::{BuilderBackend, Emit},
    token::TokenKind,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    String,
    Void,
    Char,

    Ref(Box<Type>),

    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },

    Array(Box<Type>, usize),
}

impl Type {
    /// Checks if this type is assignable to another
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        match (self, target) {
            (source, target) => source == target,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    pub fn is_unsigned_integer(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }

    pub fn is_integer(&self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn get_field(&self, field: &str) -> Option<(usize, &Type)> {
        if let Type::Struct { fields, .. } = self {
            fields
                .iter()
                .enumerate()
                .find(|(_, (n, _))| n == field)
                .map(|(i, (_, t))| (i, t))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct TypedProgram {
    pub nodes: Vec<TypedStmt>,
}

#[derive(Debug)]
pub enum TypedStmt {
    Let {
        name: String,
        ty: Type,
        value: TypedExpr,
    },
    Fun {
        name: String,
        parameters: Vec<(String, Type)>,
        is_variadic: bool,
        return_type: Type,
        is_extern: bool,
        body: Option<Box<TypedStmt>>, // Must be a TypedStmt::Block
    },
    Return {
        value: Option<TypedExpr>,
    },
    If {
        condition: TypedExpr,
        then_branch: Box<TypedStmt>,
        else_branch: Option<Box<TypedStmt>>,
    },
    While {
        condition: TypedExpr,
        body: Box<TypedStmt>,
    },

    Block(Vec<TypedStmt>),
    Expr(TypedExpr),

    Struct {
        name: String,
        ty: Type,
    },
    Impl {
        target: String,
        methods: Vec<TypedStmt>,
    },
}

impl<B: BuilderBackend> Emit<B> for TypedStmt {
    type Output = ();

    fn emit(&self, backend: &mut B) -> Self::Output {
        match self {
            TypedStmt::Let { name, ty, value } => {
                let val_code = value.emit(backend);
                backend.build_alloca(name, ty);
                backend.build_store(name, val_code);
            }

            TypedStmt::Expr(expr) => {
                expr.emit(backend);
            }

            TypedStmt::Block(statements) => {
                for stmt in statements {
                    stmt.emit(backend);
                }
            }

            TypedStmt::Return { value } => {
                let llvm_val = value.as_ref().map(|v| v.emit(backend));
                backend.build_return(llvm_val);
            }

            TypedStmt::Fun {
                name,
                parameters,
                is_variadic,
                return_type,
                is_extern,
                body,
            } => {
                let param_values =
                    backend.begin_function(name, parameters, *is_extern, *is_variadic, return_type);

                if *is_extern {
                    return;
                }

                for ((param_name, param_type), raw_value) in parameters.iter().zip(param_values) {
                    if param_name == "self" || param_name == "const self" {
                        backend.store_raw_param(param_name, raw_value);
                    } else {
                        backend.build_alloca(param_name, param_type);
                        backend.build_store(param_name, raw_value);
                    }
                }

                if let Some(body) = body {
                    body.emit(backend);
                }

                if !backend.is_block_terminated() {
                    match return_type {
                        Type::Void => backend.build_return(None),
                        _ => panic!("Function '{}' is missing a return statement", name),
                    }
                }

                backend.end_function();
            }

            TypedStmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_val = condition.emit(backend);

                let then_block = backend.append_basic_block("if.then");
                let merge_block = backend.append_basic_block("if.merge");

                let else_block_storage;
                let else_block = if else_branch.is_some() {
                    else_block_storage = backend.append_basic_block("if.else");
                    &else_block_storage
                } else {
                    &merge_block
                };
                backend.build_conditional_branch(cond_val, &then_block, &else_block);

                backend.position_at_end(&then_block);
                then_branch.emit(backend);

                if !backend.is_block_terminated() {
                    backend.build_unconditional_branch(&merge_block);
                }

                if let Some(else_stmt) = else_branch {
                    backend.position_at_end(&else_block);
                    else_stmt.emit(backend);

                    // If the else_branch didn't return, jump to merge
                    if !backend.is_block_terminated() {
                        backend.build_unconditional_branch(&merge_block);
                    }
                }

                backend.position_at_end(&merge_block);
            }

            TypedStmt::While { condition, body } => {
                let cond_block = backend.append_basic_block("loop.cond");
                let body_block = backend.append_basic_block("loop.body");
                let exit_block = backend.append_basic_block("loop.exit");

                // Fall into the condition check
                backend.build_unconditional_branch(&cond_block);

                // Condition block
                backend.position_at_end(&cond_block);
                let cond_val = condition.emit(backend);
                backend.build_conditional_branch(cond_val, &body_block, &exit_block);

                // Body block
                backend.position_at_end(&body_block);
                body.emit(backend);
                if !backend.is_block_terminated() {
                    backend.build_unconditional_branch(&cond_block);
                }

                // Exit block
                backend.position_at_end(&exit_block);
            }

            TypedStmt::Struct { .. } => {
                // Nothing to emit — struct types are resolved on demand
                // by get_llvm_type when fields/variables are encountered.
                // No LLVM IR is emitted for a struct declaration itself.
            }

            TypedStmt::Impl { methods, .. } => {
                // Methods are just regular functions with a mangled name.
                // Emit each one normally.
                for method in methods {
                    method.emit(backend);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum TypedExpr {
    Number(f64, Type),
    String(String),
    Char(char),
    Bool(bool),
    Identifier(String, Type),
    Assignment {
        name: String,
        value: Box<TypedExpr>,
        ty: Type,
    },
    Binary {
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
        operator: TokenKind,
        ty: Type,
    },
    Call {
        callee: Box<TypedExpr>,
        arguments: Vec<TypedExpr>,
        return_type: Type,
    },
    Unary {
        operator: TokenKind,
        right: Box<TypedExpr>,
        ty: Type,
    },

    AddressOf(Box<TypedExpr>, Type),
    Deref(Box<TypedExpr>, Type),

    FieldAccess {
        object: Box<TypedExpr>,
        field: String,
        field_index: usize,
        ty: Type,
    },
    FieldAssignment {
        object_name: String, // flattened — we only support `ident.field = val` for now
        field_index: usize,
        value: Box<TypedExpr>,
        object_ty: Type, // the struct type, needed for GEP
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, TypedExpr)>,
        ty: Type,
    },
    StructLiteralPositional {
        name: String,
        args: Vec<TypedExpr>,
        ty: Type,
    },
    StaticCall {
        mangled_name: String,
        arguments: Vec<TypedExpr>,
        return_type: Type,
    },
    MethodCall {
        mangled_name: String,
        self_arg: Box<TypedExpr>,
        arguments: Vec<TypedExpr>,
        return_type: Type,
    },

    // Arrays
    ArrayLiteral {
        elements: Vec<TypedExpr>,
        ty: Type,
    },
    Index {
        object: Box<TypedExpr>,
        index: Box<TypedExpr>,
        ty: Type,
    },
    IndexAssignment {
        object_name: String,
        index: Box<TypedExpr>,
        value: Box<TypedExpr>,
        elem_ty: Type,
    },
}

impl TypedExpr {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpr::Number(_, ty) => ty.clone(),
            TypedExpr::Identifier(_, ty) => ty.clone(),
            TypedExpr::Binary { ty, .. } => ty.clone(),
            TypedExpr::Unary { ty, .. } => ty.clone(),
            TypedExpr::Call { return_type, .. } => return_type.clone(),
            TypedExpr::Assignment { ty, .. } => ty.clone(),

            TypedExpr::String(_) => Type::String,
            TypedExpr::Char(_) => Type::Char,
            TypedExpr::Bool(_) => Type::Bool,

            TypedExpr::AddressOf(_, ty) => ty.clone(),
            TypedExpr::Deref(_, ty) => ty.clone(),

            TypedExpr::FieldAccess { ty, .. } => ty.clone(),
            TypedExpr::FieldAssignment { .. } => Type::Void,
            TypedExpr::StructLiteral { ty, .. } => ty.clone(),
            TypedExpr::StructLiteralPositional { ty, .. } => ty.clone(),
            TypedExpr::StaticCall { return_type, .. } => return_type.clone(),
            TypedExpr::MethodCall { return_type, .. } => return_type.clone(),

            TypedExpr::ArrayLiteral { ty, .. } => ty.clone(),
            TypedExpr::Index { ty, .. } => ty.clone(),
            TypedExpr::IndexAssignment { .. } => Type::Void,
        }
    }
}

impl<B: BuilderBackend> Emit<B> for TypedExpr {
    type Output = B::Value;

    fn emit(&self, backend: &mut B) -> Self::Output {
        match self {
            TypedExpr::Number(val, ty) => backend.const_number(*val, ty),
            TypedExpr::String(val) => backend.const_string(val.clone()),
            TypedExpr::Char(val) => backend.const_char(*val),
            TypedExpr::Bool(val) => backend.const_bool(*val),

            TypedExpr::Identifier(name, ty) => match ty {
                Type::Ref(_) => backend.get_variable_ptr(name),
                _ => backend.build_load(name, ty),
            },

            TypedExpr::Assignment { name, value, .. } => {
                let r_value = value.emit(backend);

                backend.build_store(name, r_value);

                backend.const_void()
            }

            TypedExpr::Binary {
                left,
                right,
                operator,
                ty,
            } => {
                let lhs = left.emit(backend);
                let rhs = right.emit(backend);

                match operator {
                    TokenKind::Plus => backend.build_add(lhs, rhs, ty),
                    TokenKind::Minus => backend.build_sub(lhs, rhs, ty),
                    TokenKind::Star => backend.build_mul(lhs, rhs, ty),
                    TokenKind::Slash => backend.build_div(lhs, rhs, ty),
                    TokenKind::EqualEqual => backend.build_eq(lhs, rhs, ty),
                    TokenKind::BangEqual => backend.build_neq(lhs, rhs, ty),
                    TokenKind::Less => backend.build_lt(lhs, rhs, ty),
                    TokenKind::LessEqual => backend.build_lte(lhs, rhs, ty),
                    TokenKind::Greater => backend.build_gt(lhs, rhs, ty),
                    TokenKind::GreaterEqual => backend.build_gte(lhs, rhs, ty),
                    TokenKind::AmpersandAmpersand => backend.build_and(lhs, rhs),
                    TokenKind::PipePipe => backend.build_or(lhs, rhs),
                    _ => unreachable!(),
                }
            }
            TypedExpr::Unary {
                operator,
                right,
                ty,
            } => {
                let val = right.emit(backend);
                match operator {
                    TokenKind::Bang => backend.build_not(val),
                    TokenKind::Minus => backend.build_neg(val, ty),
                    _ => panic!("Invalid unary operator"),
                }
            }
            TypedExpr::Call {
                callee,
                arguments,
                return_type,
            } => {
                let name = match &**callee {
                    TypedExpr::Identifier(name, _) => name,
                    _ => panic!("Indirect function calls are not supported."),
                };

                let args_compiled: Vec<B::Value> =
                    arguments.iter().map(|arg| arg.emit(backend)).collect();

                backend.build_call(name, args_compiled, return_type)
            }

            TypedExpr::AddressOf(inner, _) => match inner.as_ref() {
                TypedExpr::Identifier(name, ..) => {
                    let ptr = backend.get_variable_ptr(name);
                    ptr
                }
                TypedExpr::FieldAccess {
                    object,
                    field_index,
                    ..
                } => {
                    let struct_ptr = match object.as_ref() {
                        TypedExpr::Identifier(name, _) => backend.get_variable_ptr(name),
                        _ => panic!("AddressOf field access only supported on direct variables"),
                    };
                    let obj_ty = object.get_type();
                    backend.build_struct_field_ptr(struct_ptr, *field_index, &obj_ty)
                }

                other => panic!("Cannot take address of non-lvalue expression: {:?}", other),
            },
            TypedExpr::Deref(inner, ty) => {
                let ptr = inner.emit(backend);
                backend.build_load_ptr(ptr, ty)
            }

            TypedExpr::FieldAccess {
                object,
                field_index,
                ty,
                ..
            } => {
                let obj_type = object.get_type();

                let struct_ptr = match &obj_type {
                    Type::Ref(_) => object.emit(backend),

                    Type::Struct { .. } => match object.as_ref() {
                        TypedExpr::Identifier(name, _) => backend.get_variable_ptr(name),

                        // Chained field access: r.origin.x
                        // Emit the inner expression (loads the struct value),
                        // spill it to a temporary alloca, then GEP through that
                        _ => {
                            let val = object.emit(backend);
                            backend.build_temp_alloca(val, &obj_type)
                        }
                    },

                    _ => panic!("FieldAccess on non-struct type {:?}", obj_type),
                };

                let struct_ty = match &obj_type {
                    Type::Ref(inner) => *inner.clone(),
                    other => other.clone(),
                };

                let field_ptr =
                    backend.build_struct_field_ptr(struct_ptr, *field_index, &struct_ty);
                backend.build_load_ptr(field_ptr, ty)
            }
            TypedExpr::FieldAssignment {
                object_name,
                field_index,
                value,
                object_ty,
            } => {
                let struct_ptr = backend.get_variable_ptr(object_name);
                let field_ptr = backend.build_struct_field_ptr(struct_ptr, *field_index, object_ty);
                let val = value.emit(backend);
                backend.build_store_ptr(field_ptr, val);
                backend.const_void()
            }

            TypedExpr::StructLiteral { fields, ty, .. } => {
                let field_values: Vec<B::Value> =
                    fields.iter().map(|(_, expr)| expr.emit(backend)).collect();
                backend.build_struct_literal(field_values, ty)
            }

            TypedExpr::StructLiteralPositional { args, ty, .. } => {
                let field_values: Vec<B::Value> =
                    args.iter().map(|expr| expr.emit(backend)).collect();
                backend.build_struct_literal(field_values, ty)
            }

            TypedExpr::StaticCall {
                mangled_name,
                arguments,
                return_type,
            } => {
                let args: Vec<B::Value> = arguments.iter().map(|a| a.emit(backend)).collect();
                backend.build_call(mangled_name, args, return_type)
            }

            TypedExpr::MethodCall {
                mangled_name,
                self_arg,
                arguments,
                return_type,
            } => {
                // Always pass a pointer to self — never a loaded value
                let self_ptr = match self_arg.as_ref() {
                    TypedExpr::Identifier(name, _) => backend.get_variable_ptr(name),
                    _ => panic!("Method call self must be a direct variable for now"),
                };

                let mut args = vec![self_ptr];
                args.extend(arguments.iter().map(|a| a.emit(backend)));
                backend.build_call(mangled_name, args, return_type)
            }

            TypedExpr::ArrayLiteral { elements, ty } => {
                let field_values: Vec<B::Value> =
                    elements.iter().map(|e| e.emit(backend)).collect();
                backend.build_array_literal(field_values, ty)
            }

            TypedExpr::Index { object, index, ty } => {
                let arr_ptr = match object.as_ref() {
                    TypedExpr::Identifier(name, _) => backend.get_variable_ptr(name),
                    _ => {
                        let v = object.emit(backend);
                        backend.build_temp_alloca(v, &object.get_type())
                    }
                };
                let idx = index.emit(backend);
                let elem_ptr = backend.build_array_gep(arr_ptr, idx, ty);
                backend.build_load_ptr(elem_ptr, ty)
            }

            TypedExpr::IndexAssignment {
                object_name,
                index,
                value,
                elem_ty,
            } => {
                let arr_ptr = backend.get_variable_ptr(object_name);
                let idx = index.emit(backend);
                let elem_ptr = backend.build_array_gep(arr_ptr, idx, elem_ty);
                let val = value.emit(backend);
                backend.build_store_ptr(elem_ptr, val);
                backend.const_void()
            }
        }
    }
}

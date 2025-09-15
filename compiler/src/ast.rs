use inkwell::{
    AddressSpace,
    types::{BasicTypeEnum, VoidType},
};

use crate::codegen::CodeGen;

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Type>,
        value: Expr,
    },
    Func {
        name: String,
        return_type: Option<Type>,
        inferred_return: Type,
        arguments: Vec<(String, Type)>,
        body: Option<Vec<Stmt>>,
        is_extern: bool,
    },
    Return(Option<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    NumberInt(i64),
    NumberFloat(f64),
    StringLiteral(String),
    Identifier(String),
    FunctionCall { name: String, args: Vec<Expr> },

    Typed(Box<Expr>, Type),
}

impl Expr {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::NumberInt(_) => Some(Type::I64),
            Expr::NumberFloat(_) => Some(Type::F64),
            Expr::StringLiteral(_) => Some(Type::String),
            Expr::Identifier(_) => None, // only known after type check
            Expr::FunctionCall { .. } => None, // only known after type check
            Expr::Typed(_, ty) => Some(ty.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I64,
    F64,
    String,
    Void,
}

pub enum ExylLLVMType<'ctx> {
    Basic(BasicTypeEnum<'ctx>),
    Void(VoidType<'ctx>),
}

impl<'ctx> CodeGen<'ctx> {
    pub fn llvm_type(&self, ty: &Type) -> ExylLLVMType<'ctx> {
        match ty {
            Type::I64 => ExylLLVMType::Basic(self.context.i64_type().into()),
            Type::F64 => ExylLLVMType::Basic(self.context.f64_type().into()),
            Type::String => {
                ExylLLVMType::Basic(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Void => ExylLLVMType::Void(self.context.void_type()),
        }
    }
}

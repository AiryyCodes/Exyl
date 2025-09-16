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
        is_variadic: bool,
    },
    Return(Option<Expr>),
    Block(Vec<Stmt>), // For nested scopes
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    NumberInt(i64),
    NumberFloat(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Identifier(String),
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },

    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    Typed(Box<Expr>, Type),
}

impl Expr {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::NumberInt(_) => Some(Type::I64),
            Expr::NumberFloat(_) => Some(Type::F64),
            Expr::BoolLiteral(_) => Some(Type::Bool),
            Expr::StringLiteral(_) => Some(Type::String),
            Expr::Identifier(_) => None, // only known after type check
            Expr::FunctionCall { .. } => None, // only known after type check

            Expr::Binary { op, left, right } => match (left.get_type(), right.get_type()) {
                (Some(Type::I64), Some(Type::I64)) => Some(Type::I64),
                (Some(Type::F64), Some(Type::F64)) => Some(Type::F64),
                (Some(Type::Bool), Some(Type::Bool))
                    if matches!(
                        op,
                        BinaryOp::Equal
                            | BinaryOp::NotEqual
                            | BinaryOp::LessThan
                            | BinaryOp::LessThanOrEqual
                            | BinaryOp::GreaterThan
                            | BinaryOp::GreaterThanOrEqual
                    ) =>
                {
                    Some(Type::Bool)
                }
                _ => None,
            },

            Expr::Unary { .. } => None,

            Expr::Typed(_, ty) => Some(ty.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Modulo,   // %

    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=

    LogicalAnd, // &&
    LogicalOr,  // ||
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,    // !
    Negate, // - for numbers
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I64,
    F64,
    Bool,
    String,
    Void,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::I64 | Type::F64)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Type::I64)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F64)
    }
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
            Type::Bool => ExylLLVMType::Basic(self.context.bool_type().into()),
            Type::String => {
                ExylLLVMType::Basic(self.context.ptr_type(AddressSpace::default()).into())
            }
            Type::Void => ExylLLVMType::Void(self.context.void_type()),
        }
    }
}

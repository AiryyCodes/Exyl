#include "CodeGen.h"
#include "Parser.h"
#include <cstdio>
#include <iostream>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

void CodeGen::emit_program(ASTNode *root)
{
    assert(root->Type == NodeType::Program);

    // First pass: declare functions (prototypes)
    for (auto &child : root->Children)
    {
        if (child->Type == NodeType::FuncDecl)
        {
            auto &func = std::get<FuncDeclNode>(child->Data);
            declare_function(func);
        }
    }

    // Second pass: emit function bodies
    for (auto &child : root->Children)
    {
        if (child->Type == NodeType::FuncDecl)
        {
            auto &func = std::get<FuncDeclNode>(child->Data);
            emit_func(child.get(), func);
        }
    }
}

void CodeGen::print()
{
    std::string ir;
    llvm::raw_string_ostream stream(ir);
    m_Module.print(stream, nullptr);
    stream.flush();

    std::cout << ir << "\n";
}

void CodeGen::declare_function(FuncDeclNode &func)
{
    if (m_Module.getFunction(func.Name))
        return; // already declared

    std::vector<llvm::Type *> params;
    for (auto t : func.Params)
        params.push_back(to_llvm_type(t.ResolvedType));

    llvm::FunctionType *fnType =
        llvm::FunctionType::get(
            to_llvm_type(func.ReturnType),
            params,
            false);

    llvm::Function::Create(
        fnType,
        llvm::Function::ExternalLinkage,
        func.Name,
        m_Module);
}

void CodeGen::emit_func(ASTNode *node, FuncDeclNode &func)
{
    llvm::Function *fn = m_Module.getFunction(func.Name);
    assert(fn && "function not declared");

    if (!fn->empty())
        return;

    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(m_Context, "entry", fn);

    m_Builder.SetInsertPoint(entry);
    m_ValueMap.clear();

    // Params → allocas
    size_t i = 0;
    for (auto &arg : fn->args())
    {
        arg.setName(func.Params[i].Name);

        llvm::AllocaInst *alloca =
            m_Builder.CreateAlloca(arg.getType(), nullptr, arg.getName());

        m_Builder.CreateStore(&arg, alloca);
        m_ValueMap[arg.getName().str()] = alloca;

        i++;
    }

    // Body
    for (auto &stmt : node->Children)
        emit_stmt(stmt.get());

    // Implicit return for void
    if (func.ReturnType == &Types::Void)
        m_Builder.CreateRetVoid();
}

llvm::Value *CodeGen::emit_func_call(FuncCallNode &call)
{
    llvm::Function *fn = m_Module.getFunction(call.Name);
    assert(fn && "function not declared");

    std::vector<llvm::Value *> args;
    args.reserve(call.Args.size());

    for (auto &arg : call.Args)
    {
        llvm::Value *val = emit_expr(arg.get());
        assert(val && "function argument is null");
        args.push_back(val);
    }

    // If function returns void, LLVM wants an empty name
    if (fn->getReturnType()->isVoidTy())
    {
        return m_Builder.CreateCall(fn, args);
    }

    return m_Builder.CreateCall(fn, args, "calltmp");
}

void CodeGen::emit_return_stmt(ReturnStmtNode &ret)
{
    llvm::Value *retValue = nullptr;
    if (ret.Expr)
        retValue = emit_expr(ret.Expr.get());

    if (retValue)
    {
        m_Builder.CreateRet(retValue);
    }
    else
    {
        m_Builder.CreateRetVoid();
    }
}

llvm::Type *CodeGen::to_llvm_type(Type *type)
{
    auto *builtin = dynamic_cast<BuiltinType *>(type);
    if (!builtin)
        return nullptr;

    switch (builtin->kind)
    {
    case BuiltinType::Kind::I32:
        return llvm::Type::getInt32Ty(m_Context);
    case BuiltinType::Kind::I64:
        return llvm::Type::getInt64Ty(m_Context);
    case BuiltinType::Kind::F32:
        return llvm::Type::getFloatTy(m_Context);
    case BuiltinType::Kind::F64:
        return llvm::Type::getDoubleTy(m_Context);
    case BuiltinType::Kind::Bool:
        return llvm::Type::getInt1Ty(m_Context);
    case BuiltinType::Kind::Void:
        return llvm::Type::getVoidTy(m_Context);
    case BuiltinType::Kind::String:
        return get_string_type();
    default:
        return nullptr;
    }
}

void CodeGen::emit_stmt(ASTNode *node)
{
    switch (node->Type)
    {
    case NodeType::VarDecl:
        emit_var_decl(std::get<VarDeclNode>(node->Data));
        break;

    case NodeType::ReturnStmt:
        emit_return_stmt(std::get<ReturnStmtNode>(node->Data));
        break;

    case NodeType::FuncCall:
        // Evaluate the call, ignore the result if non-void
        emit_func_call(std::get<FuncCallNode>(node->Data));
        break;

    default:
        printf("Code generation for node '%d' is not implemented yet.\n", node->Type);
        break;
    }
}

llvm::Value *CodeGen::emit_expr(ASTNode *node)
{
    switch (node->Type)
    {
    case NodeType::LiteralExpr:
        return emit_literal(std::get<LiteralExprNode>(node->Data));

    case NodeType::IdentifierExpr:
        return emit_identifier(std::get<IdentifierExprNode>(node->Data));

    case NodeType::BinaryExpr:
        return emit_binary_expr(std::get<BinaryExprNode>(node->Data));

    case NodeType::FuncCall:
        return emit_func_call(std::get<FuncCallNode>(node->Data));
        break;

    default:
        return nullptr;
    }
}

llvm::Value *CodeGen::emit_identifier(IdentifierExprNode &id)
{
    llvm::Value *ptr = m_ValueMap[id.Name];
    assert(ptr && "unknown variable");

    llvm::Type *ty = to_llvm_type(id.ExprType);
    return m_Builder.CreateLoad(
        ty,
        ptr,
        id.Name + ".val");
}

llvm::Value *CodeGen::emit_literal(LiteralExprNode &lit)
{
    auto *builtin = dynamic_cast<BuiltinType *>(lit.ExprType);
    assert(builtin && "Literal has non-builtin type");

    llvm::Type *llvmTy = to_llvm_type(lit.ExprType);
    assert(llvmTy && "Failed to convert type to LLVM type");

    switch (builtin->kind)
    {
    case BuiltinType::Kind::I32:
        return llvm::ConstantInt::get(
            llvmTy,
            std::stoll(lit.Value),
            /*isSigned=*/true);

    case BuiltinType::Kind::I64:
        return llvm::ConstantInt::get(
            llvmTy,
            std::stoll(lit.Value),
            /*isSigned=*/true);

    case BuiltinType::Kind::F32:
        return llvm::ConstantFP::get(
            llvmTy,
            std::stof(lit.Value));

    case BuiltinType::Kind::F64:
        return llvm::ConstantFP::get(
            llvmTy,
            std::stod(lit.Value));

    case BuiltinType::Kind::Bool:
        return llvm::ConstantInt::get(
            llvmTy,
            lit.Value == "true");

    case BuiltinType::Kind::String:
        return emit_string_literal(lit.Value);

    case BuiltinType::Kind::Void:
    case BuiltinType::Kind::Unknown:
    case BuiltinType::Kind::Error:
        break;
    }

    assert(false && "Invalid builtin literal type");
    return nullptr;
}

llvm::Value *CodeGen::emit_string_literal(const std::string &value)
{
    // Create global constant: [N x i8]
    auto *strConst = llvm::ConstantDataArray::getString(
        m_Context,
        value,
        /* Null termination */ false);

    auto *global = new llvm::GlobalVariable(
        m_Module,
        strConst->getType(),
        true,
        llvm::GlobalValue::PrivateLinkage,
        strConst,
        ".str");

    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    global->setAlignment(llvm::Align(1));

    // i8* pointer to first character
    llvm::Value *zero = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(m_Context), 0);

    llvm::Constant *arrayPtr =
        llvm::ConstantExpr::getInBoundsGetElementPtr(
            strConst->getType(),
            global,
            {zero, zero});

    llvm::Constant *len = llvm::ConstantInt::get(
        llvm::Type::getInt64Ty(m_Context),
        value.size());

    // Construct { i8*, i64 }
    llvm::Constant *fields[] = {arrayPtr, len};

    return llvm::ConstantStruct::get(
        get_string_type(),
        fields);
}

llvm::Value *CodeGen::emit_binary_expr(BinaryExprNode &bin)
{
    llvm::Value *lhs = emit_expr(bin.LHS.get());
    llvm::Value *rhs = emit_expr(bin.RHS.get());
    assert(lhs && rhs);

    switch (bin.ResolvedOp)
    {
    case BinaryExprNode::OpKind::IntAdd:
        return m_Builder.CreateAdd(lhs, rhs, "iadd");

    case BinaryExprNode::OpKind::IntSub:
        return m_Builder.CreateSub(lhs, rhs, "isub");

    case BinaryExprNode::OpKind::IntMul:
        return m_Builder.CreateMul(lhs, rhs, "imul");

    case BinaryExprNode::OpKind::IntDiv:
        return m_Builder.CreateSDiv(lhs, rhs, "idiv");

    case BinaryExprNode::OpKind::FloatAdd:
        return m_Builder.CreateFAdd(lhs, rhs, "fadd");

    case BinaryExprNode::OpKind::FloatSub:
        return m_Builder.CreateFSub(lhs, rhs, "fsub");

    case BinaryExprNode::OpKind::FloatMul:
        return m_Builder.CreateFMul(lhs, rhs, "fmul");

    case BinaryExprNode::OpKind::FloatDiv:
        return m_Builder.CreateFDiv(lhs, rhs, "fdiv");
    }

    assert(false && "unhandled binary op");
    return nullptr;
}

void CodeGen::emit_var_decl(VarDeclNode &var)
{
    llvm::Function *fn = m_Builder.GetInsertBlock()->getParent();
    assert(fn && "variable declaration outside of function");

    if (var.VarType == &Types::Void || var.VarType == nullptr)
    {
        printf("Skipping variable '%s' of unsupported type\n", var.Name.c_str());
        return;
    }

    llvm::IRBuilder<> entryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());

    llvm::Type *llvmType = to_llvm_type(var.VarType);
    if (!llvmType || llvmType->isVoidTy())
    {
        printf("Skipping variable '%s' with unknown LLVM type\n", var.Name.c_str());
        return;
    }

    llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(llvmType, nullptr, var.Name);

    m_ValueMap[var.Name] = alloca;

    if (var.Initializer)
    {
        llvm::Value *initVal = emit_expr(var.Initializer.get());
        if (initVal)
            m_Builder.CreateStore(initVal, alloca);
    }
}

llvm::StructType *CodeGen::get_string_type()
{
    if (m_StringType)
        return m_StringType;

    auto *ptrTy = llvm::PointerType::get(m_Context, 0);
    auto *i64 = llvm::Type::getInt64Ty(m_Context);

    m_StringType = llvm::StructType::create(
        m_Context,
        {ptrTy, i64},
        "string");

    return m_StringType;
}

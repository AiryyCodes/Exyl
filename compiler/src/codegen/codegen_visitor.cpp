#include "codegen/codegen_visitor.h"
#include "ast.h"

#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Format.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Type.h>
#include <string>

void LLVMCodeGenVisitor::Visit(VariableDeclaration &node)
{
    llvm::Type *type = getLLVMType(node.type);
    if (type == nullptr)
    {
        printf("Error: Invalid type for %s\n", node.name.c_str());
    }

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
        module,
        type,
        false,
        llvm::GlobalValue::ExternalLinkage,
        getLLVMValue(node.value),
        node.name);
}

void LLVMCodeGenVisitor::Visit(FunctionDeclaration &node)
{
    // TODO: Fill the vector the correct arguments
    std::vector<llvm::Type *> args;
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(node.type), args, false);

    llvm::Function *func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, node.name, module);

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    for (const auto &child : node.body->body)
    {
        if (auto callStmt = dynamic_cast<FunctionCall *>(child.get()))
        {
            std::vector<llvm::Value *> calledArgs;
            for (const auto &arg : callStmt->args)
            {
                if (auto literal = dynamic_cast<Literal *>(arg.get()))
                {
                    // TODO: Create a function to make creating variables easier
                    llvm::Constant *value = getLLVMValue(literal->value);

                    llvm::MD5 md5;
                    md5.update(literal->value.GetValueString() + literal->name);
                    llvm::MD5::MD5Result result;
                    md5.final(result);

                    llvm::SmallString<32> uniqueName;
                    for (auto byte : result)
                    {
                        llvm::raw_svector_ostream(uniqueName) << llvm::format_hex(byte, 2);
                    }

                    llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(module, value->getType(), true, llvm::GlobalValue::PrivateLinkage, value, uniqueName.str());
                    calledArgs.push_back(globalStr);
                }
                else if (auto variable = dynamic_cast<VariableExpression *>(arg.get()))
                {
                    llvm::GlobalVariable *globalVar = module.getGlobalVariable(variable->name);
                    calledArgs.push_back(globalVar);
                }
            }

            llvm::Function *calledFunc = module.getFunction(callStmt->callee);
            builder.CreateCall(calledFunc, calledArgs);
        }
    }

    builder.CreateRetVoid();
}

void LLVMCodeGenVisitor::Visit(ExternStatement &node)
{
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(node.type), true);

    llvm::Function *func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        node.name,
        module);
}

llvm::Type *LLVMCodeGenVisitor::getLLVMType(const Type &type)
{
    if (type.GetKind() == Type::Kind::Void)
    {
        return llvm::Type::getVoidTy(context);
    }
    else if (type.GetKind() == Type::Kind::String)
    {
        return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
    }
    else if (type.GetKind() == Type::Kind::Int32)
    {
        return llvm::Type::getInt32Ty(context);
    }

    return nullptr;
}

llvm::Constant *LLVMCodeGenVisitor::getLLVMValue(Value &value)
{
    return std::visit([this](const auto &val) -> llvm::Constant *
                      {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::string>)
            return llvm::ConstantDataArray::getString(context, val, true);
        else if constexpr (std::is_same_v<T, int32_t>)
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), val, true);
        else
            return nullptr; }, value.GetValue());
}

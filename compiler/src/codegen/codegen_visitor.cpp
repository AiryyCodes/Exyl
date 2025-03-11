#include "codegen/codegen_visitor.h"
#include "ast.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Format.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Type.h>
#include <string>

void LLVMCodeGenVisitor::Visit(Literal &node)
{
    llvm::Constant *value = getLLVMValue(node.value);

    llvm::MD5 md5;
    md5.update(node.value.GetValueString() + node.name);
    llvm::MD5::MD5Result result;
    md5.final(result);

    llvm::SmallString<32> uniqueName;
    for (auto byte : result)
    {
        llvm::raw_svector_ostream(uniqueName) << llvm::format_hex(byte, 2);
    }

    value->getType()->print(llvm::outs());

    llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(module, value->getType(), true, llvm::GlobalValue::PrivateLinkage, value, uniqueName.str());

    currentValue = globalVar;
}

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

    currentValue = globalVar;
}

void LLVMCodeGenVisitor::Visit(VariableExpression &node)
{
    llvm::GlobalVariable *globalVar = module.getGlobalVariable(node.name);
    currentValue = globalVar;
}

void LLVMCodeGenVisitor::Visit(FunctionDeclaration &node)
{
    // TODO: Fill the vector the correct arguments
    std::vector<llvm::Type *> args;
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(node.type), args, false);

    llvm::Function *func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, node.name, module);

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    node.body->Accept(*this);

    builder.CreateRetVoid();

    currentValue = func;
}

void LLVMCodeGenVisitor::Visit(FunctionBody &node)
{
    for (const auto &child : node.body)
    {
        child->Accept(*this);
    }
}

void LLVMCodeGenVisitor::Visit(FunctionCall &node)
{
    std::vector<llvm::Value *> calledArgs;
    for (const auto &arg : node.args)
    {
        currentValue = nullptr;
        arg->Accept(*this);

        if (currentValue)
        {
            calledArgs.push_back(currentValue);
        }
        else
        {
            printf("CodeGen Error: Argument did not produce a value.");
            exit(1);
        }
    }

    llvm::Function *calledFunc = module.getFunction(node.callee);
    if (!calledFunc)
    {
        printf("CodeGen Error: Function %s not found.", node.callee.c_str());
        exit(1);
    }

    currentValue = builder.CreateCall(calledFunc, calledArgs);
}

void LLVMCodeGenVisitor::Visit(ExternStatement &node)
{
    llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(node.type), true);

    llvm::Function *func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        node.name,
        module);

    currentValue = func;
}

llvm::Type *LLVMCodeGenVisitor::getLLVMType(const Type &type)
{
    switch (type.GetKind())
    {
    case Type::Kind::Unknown:
        return nullptr;
    case Type::Kind::Void:
        return llvm::Type::getVoidTy(context);
    case Type::Kind::Int8:
        return llvm::Type::getInt8Ty(context);
    case Type::Kind::Int16:
        return llvm::Type::getInt16Ty(context);
    case Type::Kind::Int32:
        return llvm::Type::getInt32Ty(context);
    case Type::Kind::Int64:
        return llvm::Type::getInt64Ty(context);
    case Type::Kind::String:
        return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
    case Type::Kind::Float:
        return llvm::Type::getFloatTy(context);
    case Type::Kind::Double:
        return llvm::Type::getDoubleTy(context);
    }

    return nullptr;
}

llvm::Constant *LLVMCodeGenVisitor::getLLVMValue(Value &value)
{
    return std::visit([this](const auto &val) -> llvm::Constant *
                      {
        using T = std::decay_t<decltype(val)>;

        if constexpr (std::is_same_v<T, int8_t>)
            return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), val, true);
        else if constexpr (std::is_same_v<T, int16_t>)
            return llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), val, true);
        else if constexpr (std::is_same_v<T, int32_t>)
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), val, true);
        else if constexpr (std::is_same_v<T, int64_t>)
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), val, true);
        else if constexpr (std::is_same_v<T, std::string>)
            return llvm::ConstantDataArray::getString(context, val, true);
        else if constexpr (std::is_same_v<T, float>)
            return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), val);
        else if constexpr (std::is_same_v<T, double>)
            return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), val);
        else
            return nullptr; }, value.GetValue());
}

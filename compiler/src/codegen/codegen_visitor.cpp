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
    // TODO: Parse the function body ast and generate ir from it
    if (node.name == "main")
    {
        std::vector<llvm::Type *> mainArgs;
        llvm::FunctionType *mainType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), mainArgs, false);

        llvm::Function *mainFunc = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", module);

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
        builder.SetInsertPoint(entry);

        for (const auto &child : node.body->body)
        {
            if (auto callStmt = dynamic_cast<FunctionCall *>(child.get()))
            {
                llvm::Function *func = module.getFunction(callStmt->callee);

                std::vector<llvm::Value *> args;
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
                        args.push_back(globalStr);
                    }
                    else if (auto variable = dynamic_cast<VariableExpression *>(arg.get()))
                    {
                        llvm::GlobalVariable *globalVar = module.getGlobalVariable(variable->name);
                        args.push_back(globalVar);
                    }
                }

                builder.CreateCall(func, args);
            }
        }

        builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
    }
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

llvm::Type *LLVMCodeGenVisitor::getLLVMType(const std::string &type)
{
    if (type == "void")
    {
        return llvm::Type::getVoidTy(context);
    }
    else if (type == "string")
    {
        return llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
    }
    else if (type == "int" || type == "int32")
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
             return nullptr; }, value.value);
}

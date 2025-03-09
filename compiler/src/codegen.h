#pragma once

#include "ast.h"
#include "type.h"

#include <cstdint>
#include <cstdio>
#include <llvm-18/llvm/ADT/SmallString.h>
#include <llvm-18/llvm/IR/GlobalValue.h>
#include <llvm-18/llvm/Support/Casting.h>
#include <llvm-18/llvm/Support/MD5.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <variant>
#include <vector>

class CodeGen
{
public:
    CodeGen(std::string moduleName)
        : module(new llvm::Module(moduleName, context)), context(), builder(context) {}
    ~CodeGen() { delete module; }

    void generate(std::vector<std::shared_ptr<ASTNode>> &nodes)
    {
        // using namespace llvm;

        for (const auto &node : nodes)
        {
            if (auto varDecl = dynamic_cast<VariableDeclaration *>(node.get()))
            {
                llvm::Type *type = getLLVMType(varDecl->m_Type);
                if (type == nullptr)
                {
                    printf("Error: Invalid type for %s\n", varDecl->m_Name.c_str());
                }

                llvm::GlobalVariable *globalVar = new llvm::GlobalVariable(
                    *module,
                    type,
                    false,
                    llvm::GlobalValue::ExternalLinkage,
                    getLLVMValue(varDecl->m_Value),
                    varDecl->m_Name);
            }
            else if (auto funcDecl = dynamic_cast<FunctionDeclaration *>(node.get()))
            {
                // TODO: Parse the function body ast and generate ir from it
                if (funcDecl->m_Name == "main")
                {
                    std::vector<llvm::Type *> mainArgs;
                    llvm::FunctionType *mainType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), mainArgs, false);

                    llvm::Function *mainFunc = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", module);

                    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", mainFunc);
                    builder.SetInsertPoint(entry);

                    for (const auto &child : funcDecl->m_Body->m_Body)
                    {
                        if (auto callStmt = dynamic_cast<FunctionCall *>(child.get()))
                        {
                            llvm::Function *func = module->getFunction(callStmt->callee);

                            std::vector<llvm::Value *> args;
                            for (const auto &arg : callStmt->args)
                            {
                                if (auto literal = dynamic_cast<Literal *>(arg.get()))
                                {
                                    // TODO: Create a function to make creating variables easier
                                    // args.push_back(getLLVMValue(literal->m_Value));
                                    llvm::Constant *value = getLLVMValue(literal->m_Value);

                                    llvm::MD5 md5;
                                    md5.update(literal->m_Value.GetValueString() + literal->m_Name);
                                    llvm::MD5::MD5Result result;
                                    md5.final(result);

                                    llvm::SmallString<32> uniqueName;
                                    for (auto byte : result)
                                    {
                                        llvm::raw_svector_ostream(uniqueName) << llvm::format_hex(byte, 2);
                                    }

                                    llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(*module, value->getType(), true, llvm::GlobalValue::PrivateLinkage, value, uniqueName.str());
                                    args.push_back(globalStr);
                                }
                                else if (auto variable = dynamic_cast<VariableExpression *>(arg.get()))
                                {
                                    llvm::GlobalVariable *globalVar = module->getGlobalVariable(variable->name);
                                    args.push_back(globalVar);
                                }
                            }

                            builder.CreateCall(func, args);
                        }
                    }

                    builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
                }
            }
            else if (auto externStmt = dynamic_cast<ExternStatement *>(node.get()))
            {
                llvm::FunctionType *funcType = llvm::FunctionType::get(getLLVMType(externStmt->type), true);

                llvm::Function *func = llvm::Function::Create(
                    funcType,
                    llvm::Function::ExternalLinkage,
                    externStmt->name,
                    module);
            }
        }
    }

    void compile(const std::string &output)
    {
        using namespace llvm;

        module->print(outs(), nullptr);

        std::error_code errorCode;
        llvm::raw_fd_ostream dest(output, errorCode, sys::fs::OpenFlags::OF_None);

        if (errorCode)
        {
            errs() << "Could not open file: " << errorCode.message();
            return;
        }

        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();

        auto TargetTriple = sys::getDefaultTargetTriple();
        std::string Error;
        const Target *TheTarget = TargetRegistry::lookupTarget(TargetTriple, Error);

        if (!TheTarget)
        {
            errs() << "Target not found: " << Error;
            return;
        }

        auto CPU = "generic";
        auto Features = "";
        TargetOptions opt;
        auto RM = std::optional<Reloc::Model>(Reloc::PIC_);
        TargetMachine *targetMachine = TheTarget->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

        module->setDataLayout(targetMachine->createDataLayout());
        module->setTargetTriple(TargetTriple);

        legacy::PassManager pass;
        if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, CodeGenFileType::ObjectFile))
        {
            errs() << "Target machine can't emit object file\n";
            return;
        }

        pass.run(*module);
        dest.flush();
    }

private:
    llvm::Type *getLLVMType(const std::string &type)
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

    llvm::Constant *getLLVMValue(Value &value)
    {
        return std::visit([this](const auto &val) -> llvm::Constant *
                          {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, std::string>)
                return llvm::ConstantDataArray::getString(context, val, true);
            else if constexpr (std::is_same_v<T, int32_t>)
                return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), val, true);
            else
             return nullptr; }, value.m_Value);
    }

private:
    llvm::LLVMContext context;
    llvm::Module *module;
    llvm::IRBuilder<> builder;
};

#pragma once

#include "type.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Type.h>

class VariableDeclaration;
class FunctionDeclaration;
class ExternStatement;

class CodeGenVisitor
{
public:
    virtual void Visit(VariableDeclaration &node) = 0;
    virtual void Visit(FunctionDeclaration &node) = 0;
    virtual void Visit(ExternStatement &node) = 0;
};

class LLVMCodeGenVisitor : public CodeGenVisitor
{
public:
    LLVMCodeGenVisitor(const std::string moduleName)
        : module(moduleName, context), builder(context) {}

    void Visit(VariableDeclaration &node) override;
    void Visit(FunctionDeclaration &node) override;
    void Visit(ExternStatement &node) override;

    llvm::Module &GetModule() { return module; }

private:
    llvm::Type *getLLVMType(const Type &type);
    llvm::Constant *getLLVMValue(Value &value);

private:
    llvm::LLVMContext context;
    llvm::Module module;
    llvm::IRBuilder<> builder;
};

#pragma once

#include "type.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Type.h>

class Literal;
class VariableDeclaration;
class VariableExpression;
class FunctionDeclaration;
class FunctionBody;
class FunctionCall;
class ExternStatement;

class CodeGenVisitor
{
public:
    virtual void Visit(Literal &node) = 0;
    virtual void Visit(VariableDeclaration &node) = 0;
    virtual void Visit(VariableExpression &node) = 0;
    virtual void Visit(FunctionDeclaration &node) = 0;
    virtual void Visit(FunctionBody &node) = 0;
    virtual void Visit(FunctionCall &node) = 0;
    virtual void Visit(ExternStatement &node) = 0;
};

class LLVMCodeGenVisitor : public CodeGenVisitor
{
public:
    LLVMCodeGenVisitor(const std::string moduleName)
        : module(moduleName, context), builder(context) {}

    void Visit(Literal &node) override;
    void Visit(VariableDeclaration &node) override;
    void Visit(VariableExpression &node) override;
    void Visit(FunctionDeclaration &node) override;
    void Visit(FunctionBody &node) override;
    void Visit(FunctionCall &node) override;
    void Visit(ExternStatement &node) override;

    llvm::Module &GetModule() { return module; }

private:
    llvm::Type *getLLVMType(const Type &type);
    llvm::Constant *getLLVMValue(Value &value);

private:
    llvm::LLVMContext context;
    llvm::Module module;
    llvm::IRBuilder<> builder;

    llvm::Value *currentValue;
};

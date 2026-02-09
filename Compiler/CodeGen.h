#pragma once

#include "Parser.h"
#include "Type.h"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <string>
#include <unordered_map>

class CodeGen
{
public:
    CodeGen()
        : m_Context(), m_Module("exyl", m_Context), m_Builder(m_Context)
    {
    }

    void emit_program(ASTNode *root);

    llvm::Value *emit_expr(ASTNode *node);
    void emit_stmt(ASTNode *node);

    void print();

private:
    llvm::Value *emit_identifier(IdentifierExprNode &id);

    llvm::Value *emit_literal(LiteralExprNode &lit);
    llvm::Value *emit_string_literal(const std::string &value);

    llvm::Value *emit_binary_expr(BinaryExprNode &bin);
    llvm::Value *emit_func_call(FuncCallNode &call);

    void emit_var_decl(VarDeclNode &var);

    void declare_function(FuncDeclNode &func);
    void emit_func(ASTNode *node, FuncDeclNode &func);
    void emit_return_stmt(ReturnStmtNode &ret);

    llvm::Type *to_llvm_type(Type *type);
    llvm::StructType *get_string_type();

private:
    llvm::LLVMContext m_Context;
    llvm::Module m_Module;
    llvm::IRBuilder<> m_Builder;

    std::unordered_map<std::string, llvm::Value *> m_ValueMap;

    llvm::StructType *m_StringType = nullptr;
};

#pragma once

#include "Parser.h"
#include "Type.h"

#include <format>
#include <memory>
#include <string>
#include <unordered_map>

enum class SymbolKind
{
    Variable,
    Function
};

struct Symbol
{
    std::string Name;
    SymbolKind Kind;
    Type *TypeInfo;
};

struct Scope
{
    std::unordered_map<std::string, std::unique_ptr<Symbol>> Symbols;
    std::unique_ptr<Scope> Parent;
};

class SymbolTable
{
public:
    void push_scope()
    {
        auto scope = std::make_unique<Scope>();
        scope->Parent = std::move(Current);
        Current = std::move(scope);
    }

    void pop_scope()
    {
        if (!Current)
            return;

        Current = std::move(Current->Parent);
    }

    bool declare(std::unique_ptr<Symbol> sym)
    {
        auto &symbols = Current->Symbols;

        if (symbols.contains(sym->Name))
            return false;

        symbols.emplace(sym->Name, std::move(sym));
        return true;
    }

    Symbol *lookup(const std::string &name) const
    {
        for (Scope *s = Current.get(); s; s = s->Parent.get())
        {
            auto it = s->Symbols.find(name);
            if (it != s->Symbols.end())
                return it->second.get();
        }
        return nullptr;
    }

private:
    std::unique_ptr<Scope> Current;
};

class SemanticAnalyzer
{
public:
    void analyze(ASTNode *root);

    bool has_errors() const { return m_ErrorCount > 0; }

private:
    SymbolTable m_Symbols;

    int m_ErrorCount = 0;

    Type *m_CurrentFunctionReturn = nullptr;
    Type *m_InferredReturnType = nullptr;

    template <typename... Args>
    void error(std::format_string<Args...> fmt, Args &&...args)
    {
        m_ErrorCount++;
        std::string msg = std::format(fmt, std::forward<Args>(args)...);
        fprintf(stderr, "Semantic error: %s\n", msg.c_str());
    }

    void visit(ASTNode *node);

    void visit_program(ASTNode *node);
    void visit_func_decl(ASTNode *node, FuncDeclNode &func);
    void visit_var_decl(ASTNode *node, VarDeclNode &var);

    Type *visit_expr(ASTNode *node);
    Type *visit_literal_expr(LiteralExprNode &node);
    Type *visit_identifier_expr(IdentifierExprNode &node);
    Type *visit_binary_expr(BinaryExprNode &node);
    Type *visit_return_stmt(ASTNode *node);

    Type *analyze_literal(Literal &literal);

    Type *resolve_type(const std::string &name);

    bool is_assignable(Type *target, Type *value);
};

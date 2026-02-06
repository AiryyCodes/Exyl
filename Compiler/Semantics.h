#pragma once

#include "Parser.h"
#include "Type.h"

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
    std::unordered_map<std::string, Symbol *> Symbols;
    Scope *Parent = nullptr;
};

class SymbolTable
{
public:
    void push_scope()
    {
        Current = new Scope{.Symbols = {}, .Parent = Current};
    }

    void pop_scope()
    {
        Scope *old = Current;
        Current = Current->Parent;
        delete old;
    }

    bool declare(Symbol *sym)
    {
        if (Current->Symbols.contains(sym->Name))
            return false;

        Current->Symbols[sym->Name] = sym;
        return true;
    }

    Symbol *lookup(const std::string &name)
    {
        for (Scope *s = Current; s; s = s->Parent)
        {
            auto it = s->Symbols.find(name);
            if (it != s->Symbols.end())
                return it->second;
        }
        return nullptr;
    }

private:
    Scope *Current = nullptr;
};

class SemanticAnalyzer
{
public:
    void analyze(ASTNode *root);

private:
    SymbolTable m_Symbols;

    void visit(ASTNode *node);
    void visit_program(ASTNode *node);
    void visit_func_decl(ASTNode *node, FuncDeclNode &func);
    void visit_var_decl(ASTNode *node, VarDeclNode &var);

    Type *analyze_literal(Literal &literal);

    Type *resolve_type(const std::string &name);
};

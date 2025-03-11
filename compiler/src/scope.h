#pragma once

#include "type.h"

#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

struct FunctionSymbol
{
    FunctionSymbol() = default;
    FunctionSymbol(const std::string &name, const Type &returnType, std::vector<std::pair<std::string, Type>> parameters = {})
        : name(name), returnType(returnType), parameters(parameters) {}

    std::string name;
    Type returnType;
    std::vector<std::pair<std::string, Type>> parameters;
};

class Scope
{
public:
    explicit Scope(std::shared_ptr<Scope> parent = nullptr)
        : parent(std::move(parent)) {}

    bool DefineVar(const std::string &name, const Type &type)
    {
        if (symbols.find(name) != symbols.end())
        {
            return false;
        }
        symbols[name] = type;
        return true;
    }

    std::optional<Type> LookupVar(const std::string &name)
    {
        auto it = symbols.find(name);
        if (it != symbols.end())
        {
            return it->second;
        }
        return parent ? parent->LookupVar(name) : std::nullopt;
    }

    bool DefineFunc(const std::string &name, FunctionSymbol func)
    {
        if (functions.find(name) != functions.end())
        {
            return false; // Function already exists in this scope
        }
        functions[name] = func;
        return true;
    }

    FunctionSymbol *LookupFunc(const std::string &name)
    {
        auto it = functions.find(name);
        if (it != functions.end())
        {
            return &it->second;
        }
        return parent ? parent->LookupFunc(name) : nullptr;
    }

private:
    std::unordered_map<std::string, Type> symbols;
    std::unordered_map<std::string, FunctionSymbol> functions;
    std::shared_ptr<Scope> parent;

    friend class ScopeManager;
};

class ScopeManager
{
public:
    ScopeManager()
    {
        EnterScope();
    }

    void EnterScope()
    {
        currentScope = std::make_shared<Scope>(currentScope);
    }

    void ExitScope()
    {
        if (currentScope->parent)
        {
            currentScope = currentScope->parent;
        }
    }

    bool DefineVar(const std::string &name, const Type &type)
    {
        return currentScope->DefineVar(name, type);
    }

    std::optional<Type> LookupVar(const std::string &name)
    {
        return currentScope->LookupVar(name);
    }

    bool DefineFunc(const std::string &name, FunctionSymbol func)
    {
        return currentScope->DefineFunc(name, func);
    }

    FunctionSymbol *LookupFunc(const std::string &name)
    {
        return currentScope->LookupFunc(name);
    }

private:
    std::shared_ptr<Scope> currentScope;
};

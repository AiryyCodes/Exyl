#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

struct FunctionSymbol
{
    std::string name;
    std::string returnType;
    std::vector<std::pair<std::string, std::string>> parameters;
};

class Scope
{
public:
    explicit Scope(std::shared_ptr<Scope> parent = nullptr)
        : parent(std::move(parent)) {}

    bool defineVar(const std::string &name, const std::string &type)
    {
        if (symbols.find(name) != symbols.end())
        {
            return false;
        }
        symbols[name] = type;
        return true;
    }

    std::string lookupVar(const std::string &name)
    {
        auto it = symbols.find(name);
        if (it != symbols.end())
        {
            return it->second;
        }
        return parent ? parent->lookupVar(name) : "";
    }

    bool defineFunc(const std::string &name, FunctionSymbol func)
    {
        if (functions.find(name) != functions.end())
        {
            return false; // Function already exists in this scope
        }
        functions[name] = std::move(func);
        return true;
    }

    FunctionSymbol *lookupFunc(const std::string &name)
    {
        auto it = functions.find(name);
        if (it != functions.end())
        {
            return &it->second;
        }
        return parent ? parent->lookupFunc(name) : nullptr;
    }

private:
    std::unordered_map<std::string, std::string> symbols;
    std::unordered_map<std::string, FunctionSymbol> functions;
    std::shared_ptr<Scope> parent;

    friend class ScopeManager;
};

class ScopeManager
{
public:
    ScopeManager()
    {
        enterScope();
    }

    void enterScope()
    {
        currentScope = std::make_shared<Scope>(currentScope);
    }

    void exitScope()
    {
        if (currentScope->parent)
        {
            currentScope = currentScope->parent;
        }
    }

    bool defineVar(const std::string &name, const std::string &type)
    {
        return currentScope->defineVar(name, type);
    }

    std::string lookupVar(const std::string &name)
    {
        return currentScope->lookupVar(name);
    }

    bool defineFunc(const std::string &name, FunctionSymbol func)
    {
        return currentScope->defineFunc(name, func);
    }

    FunctionSymbol *lookupFunc(const std::string &name)
    {
        return currentScope->lookupFunc(name);
    }

private:
    std::shared_ptr<Scope> currentScope;
};

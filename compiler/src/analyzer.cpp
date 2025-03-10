#include "analyzer.h"
#include "ast.h"

#include <cctype>
#include <cstdio>

void Analyzer::Analyze()
{
    scopeManager.EnterScope();

    for (const auto &node : ast)
    {
        analyzeNode(node.get());
    }

    scopeManager.ExitScope();
}

void Analyzer::analyzeNode(ASTNode *node)
{
    if (auto varDecl = dynamic_cast<VariableDeclaration *>(node))
    {
        analyzeVariableDeclaration(varDecl);
    }
    else if (auto funcDecl = dynamic_cast<FunctionDeclaration *>(node))
    {
        analyzeFunctionDeclaration(funcDecl);
    }
}

void Analyzer::analyzeVariableDeclaration(VariableDeclaration *varDecl)
{
    if (!scopeManager.DefineVar(varDecl->name, varDecl->type))
    {
        printf("Variable '%s' is already defined in this scope.\n", varDecl->name.c_str());
        exit(1);
    }

    std::string typeLowercase;
    for (const auto &c : TypeToString(varDecl->value.type))
    {
        typeLowercase += std::tolower(c);
    }

    // TODO: Make this more modular for type aliases and such
    if (typeLowercase == "int32" && varDecl->type != "int32")
    {
        typeLowercase.erase(typeLowercase.find("32"), 2);
    }

    if (typeLowercase != varDecl->type)
    {
        printf("Error: Expected type '%s' but got '%s'\n", varDecl->type.c_str(), typeLowercase.c_str());
        exit(1);
    }
}

void Analyzer::analyzeFunctionDeclaration(FunctionDeclaration *funcDecl)
{
    FunctionSymbol funcSymbol;
    funcSymbol.name = funcDecl->name;
    funcSymbol.returnType = funcDecl->type;

    for (const auto &param : funcDecl->parameters)
    {
        funcSymbol.parameters.emplace_back(param->name, param->type);
    }

    // TODO: For now only allow top-level functions
    // Register the function in the global scope
    if (!scopeManager.DefineFunc(funcDecl->name, funcSymbol))
    {
        printf("Function '%s' is already defined.\n", funcDecl->name.c_str());
        exit(1);
    }

    // Analyze function body if it exists
    if (funcDecl->body)
    {
        analyzeFunctionBody(funcDecl);
    }
}

void Analyzer::analyzeFunctionBody(FunctionDeclaration *funcDecl)
{
    scopeManager.EnterScope(); // Create a new function scope

    // Define function parameters inside the new scope
    for (const auto &param : funcDecl->parameters)
    {
        if (!scopeManager.DefineVar(param->name, param->type))
        {
            printf("Parameter '%s' is already defined.\n", param->name.c_str());
        }
    }

    // Analyze function body statements
    for (const auto &stmt : funcDecl->body->body)
    {
        analyzeNode(stmt.get());
    }

    scopeManager.ExitScope(); // Exit function scope
}

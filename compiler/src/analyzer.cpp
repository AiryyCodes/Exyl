#include "analyzer.h"
#include "ast.h"

#include <cctype>
#include <cstdio>

void Analyzer::Analyze()
{
    scopeManager.enterScope();

    for (const auto &node : ast)
    {
        analyzeNode(node.get());
    }

    scopeManager.exitScope();
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
    if (!scopeManager.defineVar(varDecl->m_Name, varDecl->m_Type))
    {
        printf("Variable '%s' is already defined in this scope.\n", varDecl->m_Name.c_str());
        exit(1);
    }

    std::string typeLowercase;
    for (const auto &c : typeToString(varDecl->m_Value.m_Type))
    {
        typeLowercase += std::tolower(c);
    }

    // TODO: Make this more modular for type aliases and such
    if (typeLowercase == "int32" && varDecl->m_Type != "int32")
    {
        typeLowercase.erase(typeLowercase.find("32"), 2);
    }

    if (typeLowercase != varDecl->m_Type)
    {
        printf("Error: Expected type '%s' but got '%s'\n", varDecl->m_Type.c_str(), typeLowercase.c_str());
        exit(1);
    }
}

void Analyzer::analyzeFunctionDeclaration(FunctionDeclaration *funcDecl)
{
    FunctionSymbol funcSymbol;
    funcSymbol.name = funcDecl->m_Name;
    funcSymbol.returnType = funcDecl->m_Type;

    for (const auto &param : funcDecl->m_Parameters)
    {
        funcSymbol.parameters.emplace_back(param->m_Name, param->m_Type);
    }

    // TODO: For now only allow top-level functions
    // Register the function in the global scope
    if (!scopeManager.defineFunc(funcDecl->m_Name, funcSymbol))
    {
        printf("Function '%s' is already defined.\n", funcDecl->m_Name.c_str());
        exit(1);
    }

    // Analyze function body if it exists
    if (funcDecl->m_Body)
    {
        analyzeFunctionBody(funcDecl);
    }
}

void Analyzer::analyzeFunctionBody(FunctionDeclaration *funcDecl)
{
    scopeManager.enterScope(); // Create a new function scope

    // Define function parameters inside the new scope
    for (const auto &param : funcDecl->m_Parameters)
    {
        if (!scopeManager.defineVar(param->m_Name, param->m_Type))
        {
            printf("Parameter '%s' is already defined.\n", param->m_Name.c_str());
        }
    }

    // Analyze function body statements
    for (const auto &stmt : funcDecl->m_Body->m_Body)
    {
        analyzeNode(stmt.get());
    }

    scopeManager.exitScope(); // Exit function scope
}

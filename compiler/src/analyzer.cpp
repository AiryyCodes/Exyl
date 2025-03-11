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
    else if (auto externStmt = dynamic_cast<ExternStatement *>(node))
    {
        analyzeExternStatement(externStmt);
    }
    else if (auto funcCall = dynamic_cast<FunctionCall *>(node))
    {
        analyzeFuncCall(funcCall);
    }
}

void Analyzer::analyzeVariableDeclaration(VariableDeclaration *varDecl)
{
    if (!scopeManager.DefineVar(varDecl->name, varDecl->type))
    {
        printf("Variable '%s' is already defined in the current scope.\n", varDecl->name.c_str());
        exit(1);
    }

    if (varDecl->value.GetType() != varDecl->type)
    {
        printf("Error: Expected type '%s' but got '%s'\n", varDecl->type.ToString().c_str(), varDecl->value.GetType().ToString().c_str());
        exit(1);
    }
}

void Analyzer::analyzeFunctionDeclaration(FunctionDeclaration *funcDecl)
{
    FunctionSymbol funcSymbol(funcDecl->name, funcDecl->type);

    for (const auto &param : funcDecl->parameters)
    {
        funcSymbol.parameters.emplace_back(param->name, param->type);
    }

    // TODO: For now only allow top-level functions
    // Register the function in the global scope
    if (!scopeManager.DefineFunc(funcDecl->name, funcSymbol))
    {
        printf("Function '%s' is already defined in the current scope.\n", funcDecl->name.c_str());
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
            printf("Error: Parameter '%s' is already defined in the current scope.\n", param->name.c_str());
            exit(1);
        }
    }

    // Analyze function body statements
    for (const auto &stmt : funcDecl->body->body)
    {
        analyzeNode(stmt.get());
    }

    scopeManager.ExitScope(); // Exit function scope
}

void Analyzer::analyzeExternStatement(ExternStatement *externStmt)
{
    FunctionSymbol funcSymbol(externStmt->name, externStmt->type);

    for (const auto &param : externStmt->parameters)
    {
        Type::Kind kind = Type::FromString(param);
        if (kind == Type::Kind::Unknown)
        {
            printf("Error: Invalid or unrecognized type '%s'.\n", param.c_str());
            exit(1);
        }
        Type type(kind);
        funcSymbol.parameters.emplace_back("", type);
    }

    if (!scopeManager.DefineFunc(externStmt->name, funcSymbol))
    {
        printf("Function '%s' is already defined in the current scope.\n", externStmt->name.c_str());
        exit(1);
    }
}

void Analyzer::analyzeFuncCall(FunctionCall *funcCall)
{
    FunctionSymbol *symbol = scopeManager.LookupFunc(funcCall->callee);
    if (!symbol)
    {
        printf("Error: Function '%s' is not defined or does not exist in the current scope.\n", funcCall->callee.c_str());
        exit(1);
    }

    if (funcCall->args.size() != symbol->parameters.size())
    {
        printf("Error: Function '%s' expects %zu parameters, but %zu were provided.\n", funcCall->callee.c_str(),
               symbol->parameters.size(), funcCall->args.size());
        exit(1);
    }

    // Check if each argument type matches the corresponding parameter type
    for (size_t i = 0; i < funcCall->args.size(); ++i)
    {
        // Assuming you have a way to get the type of each argument (e.g., through analysis of the argument nodes)
        Type argType = funcCall->args[i]->GetType(); // You may need to implement GetType() based on your AST nodes
        Type paramType = symbol->parameters[i].second;

        if (argType != paramType)
        {
            printf("Error: Argument %zu for function '%s' is of type '%s', but the parameter expects type '%s'.\n",
                   i + 1, funcCall->callee.c_str(), argType.ToString().c_str(), paramType.ToString().c_str());
            exit(1);
        }
    }
}

#pragma once

#include "ast.h"
#include "scope.h"

#include <memory>
#include <vector>

class Analyzer
{
public:
    Analyzer(std::vector<std::shared_ptr<ASTNode>> ast) : ast(std::move(ast)) {}

    void Analyze();

private:
    void analyzeNode(ASTNode *node);

    void analyzeVariableDeclaration(VariableDeclaration *varDecl);
    void analyzeFunctionDeclaration(FunctionDeclaration *funcDecl);
    void analyzeFunctionBody(FunctionDeclaration *funcDecl);
    void analyzeExternStatement(ExternStatement *externStmt);
    void analyzeFuncCall(FunctionCall *funcCall);

private:
    ScopeManager &scopeManager = ScopeManager::Get();

    std::vector<std::shared_ptr<ASTNode>> ast;
};

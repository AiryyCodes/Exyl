#pragma once

#include "ast.h"
#include "lexer.h"
#include "token.h"

#include <cstdio>
#include <memory>
#include <vector>

class Parser
{
public:
    Parser(Lexer &lexer)
        : lexer(lexer), currentToken(lexer.NextToken()) {}

    std::vector<std::shared_ptr<ASTNode>> Parse();

private:
    Token &CurrentToken()
    {
        return currentToken;
    }

    Token &NextToken()
    {
        currentToken = lexer.NextToken();
        return currentToken;
    }

    Token PeekToken()
    {
        Lexer tmpLexer = lexer;
        return tmpLexer.NextToken();
    }

    void Expect(TokenType type, const std::string &value = "")
    {
        if (value.empty() && CurrentToken().type != type)
        {
            std::printf("Syntax Error: Expected token '%s' but found '%s'\n", TokenTypeToString(type).c_str(), CurrentToken().value.c_str());
            exit(1);
        }

        if (CurrentToken().type != type || (value != "" && CurrentToken().value != value))
        {
            std::printf("Syntax Error: Expected '%s' but found '%s'\n", value.c_str(), CurrentToken().value.c_str());
            exit(1);
        }
    }

private:
    std::shared_ptr<VariableDeclaration> parseVariableDecl();
    std::shared_ptr<FunctionDeclaration> parseFunctionDecl();
    std::unique_ptr<FunctionBody> parseFunctionBody();
    std::shared_ptr<ASTNode> parseStatement();
    std::shared_ptr<ExternStatement> parseExternStatement();
    std::shared_ptr<ASTNode> parseExpression();
    std::shared_ptr<FunctionCall> parseFuncCall(const std::string &callee);

private:
    Lexer &lexer;

    Token currentToken;
};

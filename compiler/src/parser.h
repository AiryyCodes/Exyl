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
        : m_Lexer(lexer), m_CurrentToken(lexer.NextToken()) {}

    std::vector<std::unique_ptr<ASTNode>> Parse();

private:
    Token &currentToken()
    {
        return m_CurrentToken;
    }

    Token &nextToken()
    {
        m_CurrentToken = m_Lexer.NextToken();
        return m_CurrentToken;
    }

    Token peekToken()
    {
        Lexer tmpLexer = m_Lexer;
        return tmpLexer.NextToken();
    }

    void expect(TokenType type, const std::string &value = "")
    {
        if (value.empty() && currentToken().type != type)
        {
            std::printf("Syntax Error: Expected token '%s' but found '%s'\n", tokenTypeToString(type).c_str(), currentToken().value.c_str());
            exit(1);
        }

        if (currentToken().type != type || (value != "" && currentToken().value != value))
        {
            std::printf("Syntax Error: Expected '%s' but found '%s'\n", value.c_str(), currentToken().value.c_str());
            exit(1);
        }
    }

    std::unique_ptr<VariableDeclaration> parseVariableDecl();
    std::unique_ptr<FunctionDeclaration> parseFunctionDecl();

private:
    Lexer &m_Lexer;

    Token m_CurrentToken;
};

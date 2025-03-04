#pragma once

#include <string>

enum class TokenType
{
    UNKNOWN,
    TK_EOF,
    KEYWORD,
    IDENTIFIER,
    NUMBER,
    EQUALS,
    COLON,
    SEMICOLON,
    TYPE,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
};

struct Token
{
    TokenType type;
    std::string value;
    int position;
};

static std::string tokenTypeToString(TokenType type)
{
    switch (type)
    {
    case TokenType::KEYWORD:
        return "KEYWORD";
    case TokenType::IDENTIFIER:
        return "IDENTIFIER";
    case TokenType::NUMBER:
        return "NUMBER";
    case TokenType::EQUALS:
        return "EQUALS";
    case TokenType::TYPE:
        return "TYPE";
    case TokenType::SEMICOLON:
        return "SEMICOLON";
    case TokenType::COLON:
        return "COLON";
    case TokenType::TK_EOF:
        return "EOF";
    case TokenType::LPAREN:
        return "LPAREN";
    case TokenType::RPAREN:
        return "RPAREN";
    case TokenType::LBRACE:
        return "LBRACE";
    case TokenType::RBRACE:
        return "RBRACE";
    case TokenType::COMMA:
        return "COMMA";
    default:
        return "UNKNOWN";
    }
}

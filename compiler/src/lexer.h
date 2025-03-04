#pragma once

#include "token.h"

#include <string>

class Lexer
{
public:
    Lexer(std::string input)
        : m_Input(input), m_CurrentPos(0) {}

    Token NextToken();
    bool HasNextToken();

private:
    Token readIdentifier();
    Token readNumber();

private:
    std::string m_Input;
    int m_CurrentPos;

    bool m_ReturnTypeNext = false;
};

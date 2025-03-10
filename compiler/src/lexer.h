#pragma once

#include "token.h"

#include <string>

class Lexer
{
public:
    Lexer(std::string input)
        : input(input), currentPos(0) {}

    Token NextToken();
    bool HasNextToken();

private:
    Token readIdentifier();
    Token readNumber(bool isNegative);
    Token readString();

private:
    std::string input;
    int currentPos;

    bool returnTypeNext = false;
};

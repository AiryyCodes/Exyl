#include "lexer.h"

#include <cctype>
#include <cstdio>

Token Lexer::NextToken()
{
    while (currentPos < input.size() && std::isspace(input[currentPos]))
    {
        currentPos++;
    }

    if (currentPos == input.size())
    {
        return {TokenType::TK_EOF, "EOF", currentPos};
    }

    // Skip singleline comments
    if (currentPos + 1 < input.size() && input[currentPos] == '/' && input[currentPos + 1] == '/')
    {
        // Skip until newline terminator
        while (currentPos < input.size() && input[currentPos] != '\n')
        {
            currentPos++;
        }
        return NextToken();
    }

    // Skip multiline comments
    if (currentPos + 1 < input.size() && input[currentPos] == '/' && input[currentPos + 1] == '*')
    {
        // Skip over the opening "/*"
        currentPos += 2;

        // Skip until "*/" is found
        while (currentPos + 1 < input.size() && !(input[currentPos] == '*' && input[currentPos + 1] == '/'))
        {
            currentPos++;
        }

        // If "*/" is found, move past it
        if (currentPos + 1 < input.size())
        {
            currentPos += 2;
        }

        return NextToken();
    }

    if (returnTypeNext)
    {
        Token type = readIdentifier();

        returnTypeNext = false;
        return {TokenType::TYPE, type.value, currentPos};
    }

    char currentChar = input[currentPos];

    if (std::isalpha(currentChar))
    {
        Token identifier = readIdentifier();
        return identifier;
    }

    if (std::isdigit(currentChar))
    {
        Token number = readNumber(false);
        return number;
    }

    if (currentChar == '"')
    {
        Token string = readString();
        return string;
    }

    if (currentChar == '-' && std::isdigit(input[currentPos + 1]))
    {
        Token negativeNumber = readNumber(true);
        return negativeNumber;
    }

    if (currentChar == '=')

    {
        currentPos++;
        return {TokenType::EQUALS, "=", currentPos};
    }

    if (currentChar == ';')
    {
        currentPos++;
        return {TokenType::SEMICOLON, ";", currentPos};
    }

    if (currentChar == ':')
    {
        currentPos++;
        returnTypeNext = true;

        return {TokenType::COLON, ":", currentPos};
    }

    if (currentChar == '(')
    {
        currentPos++;
        return {TokenType::LPAREN, "(", currentPos};
    }

    if (currentChar == ')')
    {
        currentPos++;
        return {TokenType::RPAREN, ")", currentPos};
    }

    if (currentChar == ',')
    {
        currentPos++;
        return {TokenType::COMMA, ",", currentPos};
    }

    if (currentChar == '{')
    {
        currentPos++;
        return {TokenType::LBRACE, "{", currentPos};
    }

    if (currentChar == '}')
    {
        currentPos++;
        return {TokenType::RBRACE, "}", currentPos};
    }

    return {TokenType::UNKNOWN, std::string(1, currentChar), currentPos};
}

bool Lexer::HasNextToken()
{
    return currentPos < input.size();
}

Token Lexer::readIdentifier()
{
    int startPos = currentPos;
    std::string result;

    while (currentPos < input.size() && (std::isalnum(input[currentPos]) || input[currentPos] == '_'))
    {
        result += input[currentPos++];
    }

    if (result == "let")
        return {TokenType::KEYWORD, result, startPos};
    if (result == "fun")
        return {TokenType::KEYWORD, result, startPos};
    if (result == "extern")
        return {TokenType::KEYWORD, result, startPos};

    return {TokenType::IDENTIFIER, result, startPos};
}

Token Lexer::readNumber(bool isNegative)
{
    int startPos = currentPos;
    std::string result;

    if (isNegative)
    {
        result += '-';
        currentPos++;
    }

    while (currentPos < input.size() && std::isdigit(input[currentPos]))
    {
        result += input[currentPos++];
    }

    if (currentPos < input.size() && input[currentPos] == '.')
    {
        result += '.';
        currentPos++;

        while (currentPos < input.size() && std::isdigit(input[currentPos]))
        {
            result += input[currentPos++];
        }
    }

    return {TokenType::NUMBER, result, startPos};
}

Token Lexer::readString()
{
    int startPos = currentPos;
    currentPos++;

    std::string result;

    while (currentPos < input.size())
    {
        char currentChar = input[currentPos];

        if (currentChar == '"')
        {
            currentPos++;
            return {TokenType::STRING, result, startPos};
        }

        if (currentChar == '\\' && currentPos + 1 < input.size())
        {
            char nextChar = input[currentPos + 1];
            switch (nextChar)
            {
            case 'n':
                result += '\n';
                break;
            case 't':
                result += '\t';
                break;
            case 'r':
                result += '\r';
                break;
            case '\\':
                result += '\\';
                break;
            case '"':
                result += '"';
                break;
            default:
                result += nextChar;
                break;
            }
            currentPos += 2;
            continue;
        }

        result += currentChar;
        currentPos++;
    }

    printf("Syntax Error: Unterminate string literal at positon %i\n", startPos);
    exit(1);
}

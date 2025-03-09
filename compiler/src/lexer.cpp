#include "lexer.h"

#include <cctype>
#include <cstdio>

Token Lexer::NextToken()
{
    while (m_CurrentPos < m_Input.size() && std::isspace(m_Input[m_CurrentPos]))
    {
        m_CurrentPos++;
    }

    if (m_CurrentPos == m_Input.size())
    {
        return {TokenType::TK_EOF, "EOF", m_CurrentPos};
    }

    // Skip singleline comments
    if (m_CurrentPos + 1 < m_Input.size() && m_Input[m_CurrentPos] == '/' && m_Input[m_CurrentPos + 1] == '/')
    {
        // Skip until newline terminator
        while (m_CurrentPos < m_Input.size() && m_Input[m_CurrentPos] != '\n')
        {
            m_CurrentPos++;
        }
        return NextToken();
    }

    // Skip multiline comments
    if (m_CurrentPos + 1 < m_Input.size() && m_Input[m_CurrentPos] == '/' && m_Input[m_CurrentPos + 1] == '*')
    {
        // Skip over the opening "/*"
        m_CurrentPos += 2;

        // Skip until "*/" is found
        while (m_CurrentPos + 1 < m_Input.size() && !(m_Input[m_CurrentPos] == '*' && m_Input[m_CurrentPos + 1] == '/'))
        {
            m_CurrentPos++;
        }

        // If "*/" is found, move past it
        if (m_CurrentPos + 1 < m_Input.size())
        {
            m_CurrentPos += 2;
        }

        return NextToken();
    }

    if (m_ReturnTypeNext)
    {
        Token type = readIdentifier();

        m_ReturnTypeNext = false;
        return {TokenType::TYPE, type.value, m_CurrentPos};
    }

    char currentChar = m_Input[m_CurrentPos];

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

    if (currentChar == '-' && std::isdigit(m_Input[m_CurrentPos + 1]))
    {
        Token negativeNumber = readNumber(true);
        return negativeNumber;
    }

    if (currentChar == '=')
    {
        m_CurrentPos++;
        return {TokenType::EQUALS, "=", m_CurrentPos};
    }

    if (currentChar == ';')
    {
        m_CurrentPos++;
        return {TokenType::SEMICOLON, ";", m_CurrentPos};
    }

    if (currentChar == ':')
    {
        m_CurrentPos++;
        m_ReturnTypeNext = true;

        return {TokenType::COLON, ":", m_CurrentPos};
    }

    if (currentChar == '(')
    {
        m_CurrentPos++;
        return {TokenType::LPAREN, "(", m_CurrentPos};
    }

    if (currentChar == ')')
    {
        m_CurrentPos++;
        return {TokenType::RPAREN, ")", m_CurrentPos};
    }

    if (currentChar == ',')
    {
        m_CurrentPos++;
        return {TokenType::COMMA, ",", m_CurrentPos};
    }

    if (currentChar == '{')
    {
        m_CurrentPos++;
        return {TokenType::LBRACE, "{", m_CurrentPos};
    }

    if (currentChar == '}')
    {
        m_CurrentPos++;
        return {TokenType::RBRACE, "}", m_CurrentPos};
    }

    return {TokenType::UNKNOWN, std::string(1, currentChar), m_CurrentPos};
}

bool Lexer::HasNextToken()
{
    return m_CurrentPos < m_Input.size();
}

Token Lexer::readIdentifier()
{
    int startPos = m_CurrentPos;
    std::string result;

    while (m_CurrentPos < m_Input.size() && (std::isalnum(m_Input[m_CurrentPos]) || m_Input[m_CurrentPos] == '_'))
    {
        result += m_Input[m_CurrentPos++];
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
    int startPos = m_CurrentPos;
    std::string result;

    if (isNegative)
    {
        result += '-';
        m_CurrentPos++;
    }

    while (m_CurrentPos < m_Input.size() && std::isdigit(m_Input[m_CurrentPos]))
    {
        result += m_Input[m_CurrentPos++];
    }

    if (m_CurrentPos < m_Input.size() && m_Input[m_CurrentPos] == '.')
    {
        result += '.';
        m_CurrentPos++;

        while (m_CurrentPos < m_Input.size() && std::isdigit(m_Input[m_CurrentPos]))
        {
            result += m_Input[m_CurrentPos++];
        }
    }

    return {TokenType::NUMBER, result, startPos};
}

Token Lexer::readString()
{
    int startPos = m_CurrentPos;
    m_CurrentPos++;

    std::string result;

    while (m_CurrentPos < m_Input.size())
    {
        char currentChar = m_Input[m_CurrentPos];

        if (currentChar == '"')
        {
            m_CurrentPos++;
            return {TokenType::STRING, result, startPos};
        }

        if (currentChar == '\\' && m_CurrentPos + 1 < m_Input.size())
        {
            char nextChar = m_Input[m_CurrentPos + 1];
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
            m_CurrentPos += 2;
            continue;
        }

        result += currentChar;
        m_CurrentPos++;
    }

    printf("Syntax Error: Unterminate string literal at positon %i\n", startPos);
    exit(1);
}

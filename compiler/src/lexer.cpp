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
        Token number = readNumber();
        return number;
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

    // printf("Identifier result: %s\n", result.c_str());

    if (result == "let")
        return {TokenType::KEYWORD, result, startPos};
    if (result == "fun")
        return {TokenType::KEYWORD, result, startPos};
    return {TokenType::IDENTIFIER, result, startPos};
}

Token Lexer::readNumber()
{
    int startPos = m_CurrentPos;
    std::string result;

    while (m_CurrentPos < m_Input.size() && std::isdigit(m_Input[m_CurrentPos]))
    {
        result += m_Input[m_CurrentPos++];
    }

    return {TokenType::NUMBER, result, startPos};
}

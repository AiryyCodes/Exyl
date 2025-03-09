#include "parser.h"
#include "ast.h"
#include "token.h"
#include "type.h"

#include <cstdio>
#include <memory>
#include <vector>

std::vector<std::shared_ptr<ASTNode>> Parser::Parse()
{
    std::vector<std::shared_ptr<ASTNode>> nodes;

    while (currentToken().type != TokenType::TK_EOF)
    {
        if (currentToken().type == TokenType::KEYWORD)
        {
            if (currentToken().value == "let")
            {
                nodes.push_back(parseVariableDecl());
            }
            else if (currentToken().value == "fun")
            {
                nodes.push_back(parseFunctionDecl());
            }
            else if (currentToken().value == "extern")
            {
                nodes.push_back(parseExternStatement());
            }
            else
            {
                printf("Syntax Error: Unexpected keyword %s\n", currentToken().value.c_str());
                exit(1);
            }
        }
        else
        {
            auto expr = parseExpression();
            if (expr)
            {
                nodes.push_back(expr);
            }
            else
            {
                printf("Syntax Error: Unexpected token %s\n", currentToken().value.c_str());
                exit(1);
            }
        }

        if (peekToken().type == TokenType::UNKNOWN)
        {
            break;
        }
    }

    return nodes;
}

std::shared_ptr<VariableDeclaration> Parser::parseVariableDecl()
{
    expect(TokenType::KEYWORD, "let");

    Token identifierToken = nextToken();
    expect(TokenType::IDENTIFIER);
    std::string varName = identifierToken.value;

    nextToken();

    std::string type;
    if (currentToken().type == TokenType::COLON)
    {
        nextToken();
        expect(TokenType::TYPE);
        type = currentToken().value;
        nextToken();
    }

    std::string value;
    if (currentToken().type == TokenType::EQUALS)
    {
        nextToken();
        if (currentToken().type == TokenType::STRING)
            expect(TokenType::STRING);
        else if (currentToken().type == TokenType::NUMBER)
            expect(TokenType::NUMBER);
        value = currentToken().value;
        nextToken();
    }

    if (value.empty() && type.empty())
    {
        printf("Syntax Error: Variable '%s' must have either a type or a value.\n", varName.c_str());
        exit(1);
    }

    if (type.empty())
    {
        // Attempt to infer the type based on the value
        try
        {
            if (value.find('.') != std::string::npos)
            {
                try
                {
                    std::stof(value); // Try parsing as float
                    type = "float";
                }
                catch (const std::exception &)
                {
                    std::stod(value); // If float fails, try double
                    type = "double";
                }
            }
            else
            {
                std::stoi(value); // Try parsing as int (INT32)
                type = "int32";
            }
        }
        catch (const std::exception &)
        {
            type = "string"; // Default to string if no valid value
        }
    }

    // Create the appropriate Value based on the inferred or provided type
    Value varValue;
    if (!value.empty())
    {
        if (type == "int8")
        {
            varValue = getValueFromString<int8_t>(value, Type::INT8);
        }
        else if (type == "int16")
        {
            varValue = getValueFromString<int16_t>(value, Type::INT16);
        }
        else if (type == "int32" || type == "int")
        {
            varValue = getValueFromString<int32_t>(value, Type::INT32);
        }
        else if (type == "int64")
        {
            varValue = getValueFromString<int64_t>(value, Type::INT64);
        }
        else if (type == "string" || type.empty())
        {
            varValue = Value(value); // Treat as a string if type is empty or string
        }
        else if (type == "float")
        {
            varValue = getValueFromString<float>(value, Type::FLOAT);
        }
        else
        {
            printf("Error: Unsupported type '%s' for value '%s'.\n", type.c_str(), value.c_str());
            exit(1);
        }
    }

    expect(TokenType::SEMICOLON);

    nextToken();

    return std::make_shared<VariableDeclaration>(varName, type, varValue);
}

std::shared_ptr<FunctionDeclaration> Parser::parseFunctionDecl()
{
    expect(TokenType::KEYWORD, "fun");

    Token identifierToken = nextToken();
    expect(TokenType::IDENTIFIER);
    std::string name = identifierToken.value;

    // Consume function identifier
    nextToken();

    expect(TokenType::LPAREN);

    std::vector<std::unique_ptr<Parameter>> parameters;

    if (peekToken().type != TokenType::RPAREN)
    {
        do
        {
            Token paramToken = nextToken();
            expect(TokenType::IDENTIFIER);
            std::string paramName = paramToken.value;

            nextToken();
            expect(TokenType::COLON);

            Token typeToken = nextToken();
            expect(TokenType::TYPE);
            std::string typeName = typeToken.value;

            parameters.push_back(std::make_unique<Parameter>(paramName, typeName));

            if (peekToken().type == TokenType::COMMA)
            {
                nextToken();
            }

        } while (peekToken().type != TokenType::RPAREN);
    }

    nextToken();

    expect(TokenType::RPAREN);

    std::string type;
    if (peekToken().type == TokenType::COLON)
    {
        nextToken();

        Token typeToken = nextToken();
        expect(TokenType::TYPE);
        type = typeToken.value;
    }

    if (type.empty())
    {
        type = "void";
    }

    // Create the appropriate Value based on the inferred or provided type
    if (type == "void")
    {
    }
    else if (type == "int8")
    {
    }
    else if (type == "int16")
    {
    }
    else if (type == "int32" || type == "int")
    {
    }
    else if (type == "int64")
    {
    }
    else if (type == "string" || type.empty())
    {
    }
    else if (type == "float")
    {
    }
    else
    {
        printf("Error: Unsupported type '%s' for function '%s'.\n", type.c_str(), name.c_str());
        exit(1);
    }

    nextToken(); // Consume type and it is now LBRACE

    std::unique_ptr<FunctionBody> body = parseFunctionBody();

    return std::make_shared<FunctionDeclaration>(name, type, std::move(parameters), std::move(body));
}

std::unique_ptr<FunctionBody> Parser::parseFunctionBody()
{
    // Ensure that we expect the opening '{'
    if (currentToken().type != TokenType::LBRACE)
    {
        printf("Syntax Error: Expected '{' to begin function body.\n");
        exit(1);
    }

    nextToken(); // Consume '{'

    std::vector<std::shared_ptr<ASTNode>> statements;

    // Consume statements until we reach the closing '}'
    while (peekToken().type != TokenType::RBRACE)
    {
        auto statement = parseStatement();

        if (statement == nullptr) // If we reach the end of the block
        {
            break;
        }

        statements.push_back(std::move(statement));
    }

    if (currentToken().type != TokenType::RBRACE)
    {
        printf("Syntax Error: Function body not properly closed with '}'.\n");
        exit(1); // Or handle the error as needed
    }

    nextToken(); // Consume '}'

    return std::make_unique<FunctionBody>(statements);
}

std::shared_ptr<ASTNode> Parser::parseStatement()
{
    Token token = currentToken();
    // printf("Current token in Parser::parseStatement: %s (%s)\n", token.value.c_str(), tokenTypeToString(token.type).c_str());

    if (token.type == TokenType::KEYWORD && token.value == "let")
    {
        return parseVariableDecl();
    }

    if (token.type == TokenType::IDENTIFIER)
    {
        return parseExpression();
    }

    if (token.type == TokenType::RBRACE || token.type == TokenType::TK_EOF)
    {
        return nullptr; // End of block
    }

    printf("Error: Unexpected token in statement: %s\n", token.value.c_str());
    exit(1); // Unexpected error
}

std::shared_ptr<ExternalStatement> Parser::parseExternStatement()
{
    expect(TokenType::KEYWORD, "extern");

    Token tokenName = nextToken();

    expect(TokenType::IDENTIFIER);

    // Should be ':'
    nextToken();

    // Should be the type
    Token tokenType = nextToken();

    expect(TokenType::TYPE);

    nextToken();

    expect(TokenType::SEMICOLON);

    nextToken();

    return std::make_shared<ExternalStatement>(tokenName.value, tokenType.value);
}

std::shared_ptr<ASTNode> Parser::parseExpression()
{
    if (currentToken().type == TokenType::IDENTIFIER)
    {
        Token tokenName = currentToken();
        printf("Identifier Name: %s\n", tokenName.value.c_str());
        nextToken();

        if (currentToken().type == TokenType::LPAREN)
        {
            return parseFuncCall(tokenName.value);
        }

        return std::make_shared<VariableExpression>(tokenName.value);
    }

    if (currentToken().type == TokenType::STRING)
    {
        std::string valueStr = currentToken().value;
        nextToken();
        printf("Current token: %s\n", valueStr.c_str());
        Value value(valueStr);
        return std::make_shared<Literal>(valueStr, Type::STRING, value);
    }

    printf("Unexpected token in expression: %s\n", currentToken().value.c_str());
    exit(1);
}

std::shared_ptr<FunctionCall> Parser::parseFuncCall(const std::string &callee)
{
    expect(TokenType::LPAREN, "(");

    // Should be '('
    nextToken();

    std::vector<std::shared_ptr<ASTNode>> args;
    if (currentToken().type != TokenType::RPAREN)
    {
        while (true)
        {
            args.push_back(parseExpression());

            if (currentToken().type == TokenType::RPAREN)
                break;

            expect(TokenType::COMMA, ",");
            nextToken();
        }
    }

    nextToken();
    nextToken();

    return std::make_shared<FunctionCall>(callee, args);
}

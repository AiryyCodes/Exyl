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

    while (CurrentToken().type != TokenType::TK_EOF)
    {
        if (CurrentToken().type == TokenType::KEYWORD)
        {
            if (CurrentToken().value == "let")
            {
                nodes.push_back(parseVariableDecl());
            }
            else if (CurrentToken().value == "fun")
            {
                nodes.push_back(parseFunctionDecl());
            }
            else if (CurrentToken().value == "extern")
            {
                nodes.push_back(parseExternStatement());
            }
            else
            {
                printf("Syntax Error: Unexpected keyword %s\n", CurrentToken().value.c_str());
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
                printf("Syntax Error: Unexpected token %s\n", CurrentToken().value.c_str());
                exit(1);
            }
        }

        if (PeekToken().type == TokenType::UNKNOWN)
        {
            break;
        }
    }

    return nodes;
}

std::shared_ptr<VariableDeclaration> Parser::parseVariableDecl()
{
    Expect(TokenType::KEYWORD, "let");

    Token identifierToken = NextToken();
    Expect(TokenType::IDENTIFIER);
    std::string varName = identifierToken.value;

    NextToken();

    std::string type;
    if (CurrentToken().type == TokenType::COLON)
    {
        NextToken();
        Expect(TokenType::TYPE);
        type = CurrentToken().value;
        NextToken();
    }

    std::string value;
    if (CurrentToken().type == TokenType::EQUALS)
    {
        NextToken();
        if (CurrentToken().type == TokenType::STRING)
            Expect(TokenType::STRING);
        else if (CurrentToken().type == TokenType::NUMBER)
            Expect(TokenType::NUMBER);
        value = CurrentToken().value;
        NextToken();
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

    Expect(TokenType::SEMICOLON);

    NextToken();

    return std::make_shared<VariableDeclaration>(varName, type, varValue);
}

std::shared_ptr<FunctionDeclaration> Parser::parseFunctionDecl()
{
    Expect(TokenType::KEYWORD, "fun");

    Token identifierToken = NextToken();
    Expect(TokenType::IDENTIFIER);
    std::string name = identifierToken.value;

    // Consume function identifier
    NextToken();

    Expect(TokenType::LPAREN);

    std::vector<std::unique_ptr<Parameter>> parameters;

    if (PeekToken().type != TokenType::RPAREN)
    {
        do
        {
            Token paramToken = NextToken();
            Expect(TokenType::IDENTIFIER);
            std::string paramName = paramToken.value;

            NextToken();
            Expect(TokenType::COLON);

            Token typeToken = NextToken();
            Expect(TokenType::TYPE);
            std::string typeName = typeToken.value;

            parameters.push_back(std::make_unique<Parameter>(paramName, typeName));

            if (PeekToken().type == TokenType::COMMA)
            {
                NextToken();
            }

        } while (PeekToken().type != TokenType::RPAREN);
    }

    NextToken();

    Expect(TokenType::RPAREN);

    std::string type;
    if (PeekToken().type == TokenType::COLON)
    {
        NextToken();

        Token typeToken = NextToken();
        Expect(TokenType::TYPE);
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

    NextToken(); // Consume type and it is now LBRACE

    std::unique_ptr<FunctionBody> body = parseFunctionBody();

    return std::make_shared<FunctionDeclaration>(name, type, std::move(parameters), std::move(body));
}

std::unique_ptr<FunctionBody> Parser::parseFunctionBody()
{
    // Ensure that we expect the opening '{'
    if (CurrentToken().type != TokenType::LBRACE)
    {
        printf("Syntax Error: Expected '{' to begin function body.\n");
        exit(1);
    }

    NextToken(); // Consume '{'

    std::vector<std::shared_ptr<ASTNode>> statements;

    // Consume statements until we reach the closing '}'
    while (PeekToken().type != TokenType::RBRACE)
    {
        auto statement = parseStatement();

        if (statement == nullptr) // If we reach the end of the block
        {
            break;
        }

        statements.push_back(std::move(statement));
    }

    if (CurrentToken().type != TokenType::RBRACE)
    {
        printf("Syntax Error: Function body not properly closed with '}'.\n");
        exit(1); // Or handle the error as needed
    }

    NextToken(); // Consume '}'

    return std::make_unique<FunctionBody>(statements);
}

std::shared_ptr<ASTNode> Parser::parseStatement()
{
    Token token = CurrentToken();

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

std::shared_ptr<ExternStatement> Parser::parseExternStatement()
{
    Expect(TokenType::KEYWORD, "extern");

    Token tokenName = NextToken();

    Expect(TokenType::IDENTIFIER);

    std::vector<std::string> parameters;
    if (PeekToken().type == TokenType::LPAREN)
    {
        NextToken();

        if (PeekToken().type != TokenType::RPAREN)
        {
            do
            {
                Token paramToken = NextToken();
                Expect(TokenType::IDENTIFIER);
                std::string paramName = paramToken.value;

                if (PeekToken().type == TokenType::COMMA)
                {
                    NextToken();
                }

                parameters.push_back(paramName);
            } while (PeekToken().type != TokenType::RPAREN);

            NextToken();
        }
    }

    // Should be ':'
    NextToken();

    // Should be the type
    Token tokenType = NextToken();

    Expect(TokenType::TYPE);

    NextToken();

    Expect(TokenType::SEMICOLON);

    NextToken();

    return std::make_shared<ExternStatement>(tokenName.value, tokenType.value);
}

std::shared_ptr<ASTNode> Parser::parseExpression()
{
    if (CurrentToken().type == TokenType::IDENTIFIER)
    {
        Token tokenName = CurrentToken();
        printf("Identifier Name: %s\n", tokenName.value.c_str());
        NextToken();

        if (CurrentToken().type == TokenType::LPAREN)
        {
            return parseFuncCall(tokenName.value);
        }

        return std::make_shared<VariableExpression>(tokenName.value);
    }

    if (CurrentToken().type == TokenType::STRING)
    {
        std::string valueStr = CurrentToken().value;
        NextToken();
        printf("Current token: %s\n", valueStr.c_str());
        Value value(valueStr);
        return std::make_shared<Literal>(valueStr, Type::STRING, value);
    }

    printf("Unexpected token in expression: %s\n", CurrentToken().value.c_str());
    exit(1);
}

std::shared_ptr<FunctionCall> Parser::parseFuncCall(const std::string &callee)
{
    Expect(TokenType::LPAREN, "(");

    // Should be '('
    NextToken();

    std::vector<std::shared_ptr<ASTNode>> args;
    if (CurrentToken().type != TokenType::RPAREN)
    {
        while (true)
        {
            args.push_back(parseExpression());

            if (CurrentToken().type == TokenType::RPAREN)
                break;

            Expect(TokenType::COMMA, ",");
            NextToken();
        }
    }

    NextToken();
    NextToken();

    return std::make_shared<FunctionCall>(callee, args);
}

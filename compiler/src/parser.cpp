#include "parser.h"
#include "ast.h"
#include "token.h"

#include <memory>
#include <vector>

std::vector<std::unique_ptr<ASTNode>> Parser::Parse()
{
    std::vector<std::unique_ptr<ASTNode>> nodes;

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
            else
            {
                printf("Syntax Error: Unexpected keyword %s\n", currentToken().value.c_str());
                if (nextToken().type == TokenType::UNKNOWN)
                {
                    break;
                }
            }
        }
        else
        {
            printf("Syntax Error: Unexpected token %s\n", currentToken().value.c_str());
            if (nextToken().type == TokenType::UNKNOWN)
            {
                break;
            }
        }
    }

    return nodes;
}

std::unique_ptr<VariableDeclaration> Parser::parseVariableDecl()
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
        expect(TokenType::NUMBER);
        value = currentToken().value;
        nextToken();
    }

    if (value.empty() && type.empty())
    {
        printf("Syntax Error: Variable '%s' must have either a type or a value.\n", varName.c_str());
        exit(1);
    }

    expect(TokenType::SEMICOLON);

    // If i dont have this here the token in the while loop will never reach TK_EOF
    nextToken();

    // TODO: If value is empty the value type is gonna be string
    return std::make_unique<VariableDeclaration>(varName, type, value);
}

std::unique_ptr<FunctionDeclaration> Parser::parseFunctionDecl()
{
    expect(TokenType::KEYWORD, "fun");

    Token identifierToken = nextToken();
    expect(TokenType::IDENTIFIER);
    std::string name = identifierToken.value;

    // Consume function identifier
    nextToken();

    expect(TokenType::LPAREN);

    // nextToken();

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

    std::string typeName;
    if (peekToken().type == TokenType::COLON)
    {
        nextToken();

        Token typeToken = nextToken();
        expect(TokenType::TYPE);
        typeName = typeToken.value;
    }

    nextToken();

    return std::make_unique<FunctionDeclaration>(name, typeName, std::move(parameters));
}

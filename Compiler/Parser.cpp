#include "Parser.h"
#include "Tokenizer.h"
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <print>
#include <stdexcept>
#include <string>

Token &Parser::current()
{
    if (Tokenization.Tokens.empty())
        throw std::runtime_error("Parser has no tokens!");

    if (Index >= Tokenization.Tokens.size())
        return Tokenization.Tokens.back();

    return Tokenization.Tokens[Index];
}

Token Parser::advance()
{
    if (Index + 1 < Tokenization.Tokens.size())
        Index++;

    return current();
}

Token Parser::expect(TokenId id)
{
    Token token = current();
    if (token.Id != id)
    {
        printf("Expected '%s', got '%s'\n",
               get_token_id_name(id).c_str(),
               get_token_id_name(token.Id).c_str());
        return token;
    }

    advance();
    return token;
}

bool Parser::is_at_end()
{
    return current().Id == TokenId::EndOfFile;
}

std::unique_ptr<ASTNode> Parser::parse_program()
{
    auto program = std::make_unique<ASTNode>();
    program->Type = NodeType::Program;

    while (!is_at_end())
    {
        std::unique_ptr<ASTNode> node;

        switch (current().Id)
        {
        case TokenId::Fun:
            node = parse_func_decl();
            break;

        default:
            printf("Unexpected token '%s'\n",
                   get_token_id_name(current().Id).c_str());
            advance();
            continue;
        }

        if (node)
            program->Children.push_back(std::move(node));
    }

    return program;
}

std::unique_ptr<ASTNode> Parser::parse_func_decl()
{
    expect(TokenId::Fun);

    Token symbol = expect(TokenId::Symbol);

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::FuncDecl;

    // TODO: Parse function params
    expect(TokenId::LParen);
    expect(TokenId::RParen);

    TypeRef typeRef;
    if (current().Id == TokenId::Colon)
    {
        expect(TokenId::Colon);

        Token returnType = expect(TokenId::Symbol);
        typeRef.Name = returnType.Name;
    }

    node->Data = FuncDeclNode{
        .Name = symbol.Name,
        .ReturnTypeRef = typeRef,
    };

    node->Children = parse_block();

    return node;
}

std::vector<std::unique_ptr<ASTNode>> Parser::parse_block()
{
    expect(TokenId::LBrace);

    std::vector<std::unique_ptr<ASTNode>> children;

    while (current().Id != TokenId::RBrace)
    {
        switch (current().Id)
        {
        case TokenId::Let:
            children.push_back(parse_let_decl());
            break;

        default:
            printf("Unexpected token '%s'\n",
                   get_token_id_name(current().Id).c_str());
            advance();
            continue;
        }
    }

    expect(TokenId::RBrace);

    return children;
}

std::unique_ptr<ASTNode> Parser::parse_let_decl()
{
    expect(TokenId::Let);

    Token symbol = expect(TokenId::Symbol);

    // Consume '='
    advance();

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::VarDecl;
    node->Data = VarDeclNode{
        .Name = symbol.Name,
        .Initializer = parse_literal(),
    };

    expect(TokenId::Semicolon);

    return node;
}

Literal Parser::parse_literal()
{
    Token token = current();

    switch (token.Id)
    {
    case TokenId::String:
        advance();
        return Literal{
            .Type = LiteralType::String,
            .Value = token.Name,
            .RawValue = token.Name,
        };
    case TokenId::Int:
        advance();
        return Literal{
            .Type = LiteralType::Int,
            .Value = std::stoll(token.Name),
            .RawValue = token.Name,
        };
    case TokenId::Float:
        advance();
        return Literal{
            .Type = LiteralType::Float,
            .Value = std::stod(token.Name),
            .RawValue = token.Name,
        };
    case TokenId::True:
        advance();
        return Literal{
            .Type = LiteralType::Bool,
            .Value = true,
            .RawValue = token.Name,
        };
    case TokenId::False:
        advance();
        return Literal{
            .Type = LiteralType::Bool,
            .Value = false,
            .RawValue = token.Name,
        };

    default:
        throw std::runtime_error("Expected literal");
    }
}

std::unique_ptr<ASTNode> parse(Tokenization &tokenization)
{
    Parser parser = {
        .Tokenization = tokenization,
    };

    // TODO: Return error/invalid node
    return parser.parse_program();
}

void ASTNode::print()
{
    for (auto &child : Children)
    {
        switch (child->Type)
        {
        case NodeType::Program:
            printf("Program\n");
            break;

        case NodeType::FuncDecl:
        {
            FuncDeclNode func = std::get<FuncDeclNode>(child->Data);
            printf("Function: %s | Type: %s\n", func.Name.c_str(), func.ReturnType->get_name().c_str());
            child->print();
            break;
        }

        case NodeType::VarDecl:
        {
            VarDeclNode var = std::get<VarDeclNode>(child->Data);
            assert(var.VarType && "VarType not set");

            printf("Variable: %s | Value: %s | Type: %s\n",
                   var.Name.c_str(),
                   var.Initializer.RawValue.c_str(),
                   var.VarType->get_name().c_str());
            break;
        }
        }
    }
}

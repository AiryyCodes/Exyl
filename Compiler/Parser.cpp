#include "Parser.h"
#include "Tokenizer.h"

#include <cassert>
#include <cstdio>
#include <memory>
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

    std::vector<FuncParam> params;

    if (current().Id != TokenId::RParen)
    {
        while (true)
        {
            Token paramName = expect(TokenId::Symbol);
            expect(TokenId::Colon);
            Token paramType = expect(TokenId::Symbol);

            params.push_back(FuncParam{
                .Name = paramName.Name,
                .Type = TypeRef{.Name = paramType.Name},
            });

            if (current().Id == TokenId::Comma)
            {
                advance(); // consume comma
                continue;
            }

            break;
        }
    }

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
        .Params = std::move(params),
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
        case TokenId::Return:
            children.push_back(parse_return_stmt());
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

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::VarDecl;

    Token symbol = expect(TokenId::Symbol);

    VarDeclNode varDecl;
    varDecl.Name = symbol.Name;

    if (current().Id == TokenId::Colon)
    {
        expect(TokenId::Colon);

        Token type = expect(TokenId::Symbol);
        varDecl.VarTypeRef = TypeRef{.Name = type.Name};
    }

    // Consume '='
    advance();

    varDecl.Initializer = parse_literal();

    node->Data = varDecl;

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

std::unique_ptr<ASTNode> Parser::parse_expr()
{
    return parse_binary_expr(0);
}

std::unique_ptr<ASTNode> Parser::parse_primary()
{
    Token tok = current();

    if (tok.Id == TokenId::Int)
    {
        advance();
        auto node = std::make_unique<ASTNode>();
        node->Type = NodeType::LiteralExpr;
        node->Data = LiteralExprNode{
            .LitType = LiteralExprNode::TypeKind::Int,
            .Value = tok.Name,
        };
        return node;
    }
    else if (tok.Id == TokenId::String)
    {
        advance();
        auto node = std::make_unique<ASTNode>();
        node->Type = NodeType::LiteralExpr;
        node->Data = LiteralExprNode{
            .LitType = LiteralExprNode::TypeKind::String,
            .Value = tok.Name,
        };
        return node;
    }
    else if (tok.Id == TokenId::Symbol)
    {
        advance();
        auto node = std::make_unique<ASTNode>();
        node->Type = NodeType::IdentifierExpr;
        node->Data = IdentifierExprNode{.Name = tok.Name};
        return node;
    }
    else if (tok.Id == TokenId::LParen)
    {
        // consume '('
        advance();
        auto expr = parse_expr();
        expect(TokenId::RParen);
        return expr;
    }

    throw std::runtime_error(
        "Unexpected token in primary expression: " +
        get_token_id_name(tok.Id));
}

std::unique_ptr<ASTNode> Parser::parse_binary_expr(int minPrec)
{
    auto lhs = parse_primary();

    while (true)
    {
        Token op = current();
        int prec = get_precedence(op.Id);

        if (prec < minPrec)
            break;

        advance(); // consume operator

        auto rhs = parse_binary_expr(prec + 1);

        auto node = std::make_unique<ASTNode>();
        node->Type = NodeType::BinaryExpr;

        node->Data = BinaryExprNode{
            .Op = op.Id,
            .LHS = std::move(lhs),
            .RHS = std::move(rhs),
        };

        lhs = std::move(node);
    }

    return lhs;
}

std::unique_ptr<ASTNode> Parser::parse_return_stmt()
{
    expect(TokenId::Return);

    std::unique_ptr<ASTNode> expr = nullptr;
    if (current().Id != TokenId::Semicolon)
    {
        expr = parse_expr();
    }

    expect(TokenId::Semicolon);

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::ReturnStmt;
    node->Data = ReturnStmtNode{
        .Expr = std::move(expr),
    };

    return node;
}

int Parser::get_precedence(TokenId op)
{
    switch (op)
    {
    case TokenId::Star:
    case TokenId::Slash:
        return 2;
    case TokenId::Plus:
    case TokenId::Minus:
        return 1;
    default:
        return -1;
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

void ASTNode::print(int indent)
{
    auto print_indent = [indent]()
    {
        for (int i = 0; i < indent; i++)
            printf("  ");
    };

    // Print this node itself
    switch (Type)
    {
    case NodeType::Program:
        print_indent();
        printf("Program\n");
        break;

    case NodeType::FuncDecl:
    {
        FuncDeclNode func = std::get<FuncDeclNode>(Data);
        print_indent();
        printf("Function: %s | Return Type: %s\n",
               func.Name.c_str(),
               func.ReturnType->get_name().c_str());
        break;
    }

    case NodeType::VarDecl:
    {
        VarDeclNode var = std::get<VarDeclNode>(Data);
        print_indent();
        printf("Variable: %s | Value: %s | Type: %s\n",
               var.Name.c_str(),
               var.Initializer.RawValue.c_str(),
               var.VarType->get_name().c_str());
        break;
    }

    case NodeType::ReturnStmt:
    {
        print_indent();
        printf("Return\n");

        auto &ret = std::get<ReturnStmtNode>(Data);
        if (ret.Expr)
        {
            print_indent();
            printf("  Expr:\n");
            ret.Expr->print(indent + 2);
        }
        else
        {
            print_indent();
            printf("  <no expression>\n");
        }
        break;
    }

    case NodeType::LiteralExpr:
    {
        LiteralExprNode expr = std::get<LiteralExprNode>(Data);
        print_indent();
        printf("LiteralExpr: Type: %s | Value: %s\n",
               expr.get_type_name().c_str(),
               expr.Value.c_str());
        break;
    }

    case NodeType::IdentifierExpr:
    {
        IdentifierExprNode expr = std::get<IdentifierExprNode>(Data);
        print_indent();
        printf("IdentifierExpr: Name: %s\n", expr.Name.c_str());
        break;
    }

    case NodeType::BinaryExpr:
    {
        const BinaryExprNode &expr = std::get<BinaryExprNode>(Data);
        print_indent();
        printf("BinaryExpr: Op: %s\n", get_token_id_name(expr.Op).c_str());

        // Recursively print LHS and RHS
        if (expr.LHS)
        {
            print_indent();
            printf("  LHS:\n");
            expr.LHS->print(indent + 2);
        }
        if (expr.RHS)
        {
            print_indent();
            printf("  RHS:\n");
            expr.RHS->print(indent + 2);
        }
        break;
    }
    }

    for (const auto &child : Children)
    {
        child->print(indent + 1);
    }
}

std::string LiteralExprNode::get_type_name()
{
    switch (LitType)
    {
    case TypeKind::Int:
        return "Int";
    case TypeKind::Float:
        return "Float";
    case TypeKind::String:
        return "String";
    }

    return "";
}

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

Token Parser::peek()
{
    int offset = 1;
    if (Index + offset >= Tokenization.Tokens.size())
        return Tokenization.Tokens.back();

    return Tokenization.Tokens[Index + offset];
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

        case TokenId::Extern:
            if (peek().Id == TokenId::Fun)
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
    bool isExtern = false;
    if (current().Id == TokenId::Extern)
    {
        expect(TokenId::Extern);
        isExtern = true;
    }

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
    if (current().Id == TokenId::Colon || isExtern)
    {
        expect(TokenId::Colon);

        Token returnType = expect(TokenId::Symbol);
        typeRef.Name = returnType.Name;
    }

    node->Data = FuncDeclNode{
        .Name = symbol.Name,
        .Params = std::move(params),
        .IsExtern = isExtern,
        .ReturnTypeRef = typeRef,
    };

    if (!isExtern)
        node->Children = parse_block();
    else
        expect(TokenId::Semicolon);

    return node;
}

std::unique_ptr<ASTNode> Parser::parse_func_call(std::unique_ptr<ASTNode> callee)
{
    // Should be '('
    Token token = current();
    expect(TokenId::LParen);

    std::vector<std::unique_ptr<ASTNode>> args;

    if (current().Id != TokenId::RParen)
    {
        while (true)
        {
            auto argNode = parse_expr();
            args.push_back(std::move(argNode));

            if (current().Id == TokenId::Comma)
            {
                advance(); // consume comma
                continue;
            }

            break;
        }
    }

    expect(TokenId::RParen);

    expect(TokenId::Semicolon);

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::FuncCall;
    node->Data = FuncCallNode{
        .Name = std::get<IdentifierExprNode>(callee->Data).Name,
        .Args = std::move(args),
    };

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
        {
            children.push_back(parse_expr());
            expect(TokenId::Semicolon);
            break;
        }
        break;
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
    expect(TokenId::Equals);

    varDecl.Initializer = parse_expr();

    node->Data = std::move(varDecl);

    expect(TokenId::Semicolon);

    return node;
}

std::unique_ptr<ASTNode> Parser::parse_literal(LiteralExprNode::TypeKind kind)
{
    Token token = current();
    advance();

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::LiteralExpr;
    node->Data = LiteralExprNode{
        .LitType = kind,
        .Value = token.Name,
    };
    return node;
}

std::unique_ptr<ASTNode> Parser::parse_identifier()
{
    Token token = current();
    advance();

    auto node = std::make_unique<ASTNode>();
    node->Type = NodeType::IdentifierExpr;
    node->Data = IdentifierExprNode{.Name = token.Name};
    return node;
}

std::unique_ptr<ASTNode> Parser::parse_grouped_expr()
{
    expect(TokenId::LParen);
    auto expr = parse_expr();
    expect(TokenId::RParen);
    return expr;
}

std::unique_ptr<ASTNode> Parser::parse_expr()
{
    return parse_binary_expr(0);
}

std::unique_ptr<ASTNode> Parser::parse_primary()
{
    Token token = current();

    switch (token.Id)
    {
    case TokenId::Int:
        return parse_literal(LiteralExprNode::TypeKind::Int);
    case TokenId::Float:
        return parse_literal(LiteralExprNode::TypeKind::Float);

    case TokenId::String:
        return parse_literal(LiteralExprNode::TypeKind::String);

    case TokenId::True:
        return parse_literal(LiteralExprNode::TypeKind::Bool);
    case TokenId::False:
        return parse_literal(LiteralExprNode::TypeKind::Bool);

    case TokenId::Symbol:
    {
        auto ident = parse_identifier();

        // function call
        if (current().Id == TokenId::LParen)
        {
            advance(); // consume '('
            FuncCallNode call;
            call.Name = std::get<IdentifierExprNode>(ident->Data).Name;

            if (current().Id != TokenId::RParen) // not empty
            {
                while (true)
                {
                    call.Args.push_back(parse_expr());

                    if (current().Id == TokenId::RParen)
                        break;

                    expect(TokenId::Comma);
                }
            }

            expect(TokenId::RParen);

            auto node = std::make_unique<ASTNode>();
            node->Type = NodeType::FuncCall;
            node->Data = std::move(call);
            return node;
        }

        return ident;
    }

    case TokenId::LParen:
        return parse_grouped_expr();

    default:
        throw std::runtime_error(
            "Unexpected token in primary expression: " +
            get_token_id_name(token.Id));
    }
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
        printf("Program:\n");
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
    case NodeType::FuncCall:
    {
        FuncCallNode &call = std::get<FuncCallNode>(Data);
        print_indent();
        printf("FuncCall: %s\n",
               call.Name.c_str());

        for (const auto &arg : call.Args)
        {
            print_indent();
            printf("  Arg:\n");
            arg->print(indent + 2);
        }

        break;
    }

    case NodeType::VarDecl:
    {
        VarDeclNode &var = std::get<VarDeclNode>(Data);
        print_indent();
        printf("Variable: %s | Type: %s\n",
               var.Name.c_str(),
               var.VarType->get_name().c_str());

        if (var.Initializer)
        {
            print_indent();
            printf("  Expr:\n");
            var.Initializer->print(indent + 2);
        }

        break;
    }

    case NodeType::ReturnStmt:
    {
        print_indent();
        printf("Return:\n");

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
               expr.ExprType->get_name().c_str(),
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

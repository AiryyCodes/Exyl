#pragma once

#include "Tokenizer.h"
#include "Type.h"

#include <memory>
#include <string>
#include <variant>
#include <vector>

struct ASTNode;

enum class NodeType
{
    Program,
    FuncDecl,
    FuncCall,
    ReturnStmt,
    VarDecl,
    LiteralExpr,
    IdentifierExpr,
    BinaryExpr,
};

struct TypeRef
{
    // The parsed type name (ex. void, i32, string, etc...)
    std::string Name;
};

struct LiteralExprNode
{
    enum class TypeKind
    {
        Int,
        Float,
        String
    } LitType;
    std::string Value;

    Type *ExprType = nullptr;

    std::string get_type_name();
};

struct IdentifierExprNode
{
    std::string Name;

    Type *ExprType = nullptr;
};

struct BinaryExprNode
{
    TokenId Op; // Plus, Minus, etc.

    std::unique_ptr<ASTNode> LHS;
    std::unique_ptr<ASTNode> RHS;

    Type *ExprType = nullptr;
};

struct FuncParam
{
    std::string Name;
    TypeRef Type;
};

struct FuncDeclNode
{
    std::string Name;
    std::vector<FuncParam> Params;

    TypeRef ReturnTypeRef;
    Type *ReturnType = &Types::Error;
};

struct FuncCallParam
{
    std::string Name;
};

struct FuncCallNode
{
    std::string Name;

    std::vector<std::unique_ptr<ASTNode>> Args;
};

struct ReturnStmtNode
{
    std::unique_ptr<ASTNode> Expr;
};

enum class LiteralType
{
    Int,
    Float,
    Bool,
    String,
};

using LiteralValue = std::variant<
    int64_t,
    double,
    bool,
    std::string,
    std::monostate // for null
    >;

struct Literal
{
    LiteralType Type;
    LiteralValue Value;
    std::string RawValue;
};

struct VarDeclNode
{
    std::string Name;
    Literal Initializer;

    TypeRef VarTypeRef;
    Type *VarType = &Types::Error;
};

using ASTData = std::variant<
    std::monostate,
    FuncDeclNode,
    FuncCallNode,
    ReturnStmtNode,
    VarDeclNode,
    LiteralExprNode,
    IdentifierExprNode,
    BinaryExprNode>;

struct ASTNode
{
    NodeType Type;

    // Children of nodes that have a body (ex Program, FuncDecl, etc...)
    std::vector<std::unique_ptr<ASTNode>> Children;

    // Data for the node
    ASTData Data;

    void print(int indent = 0);
};

struct Parser
{
    struct Tokenization Tokenization;

    int Index = 0;

    Token &current();
    Token advance();
    Token peek();
    Token expect(TokenId id);

    bool is_at_end();

    std::unique_ptr<ASTNode> parse_program();
    std::unique_ptr<ASTNode> parse_func_decl();
    std::unique_ptr<ASTNode> parse_func_call();
    std::vector<std::unique_ptr<ASTNode>> parse_block();

    std::unique_ptr<ASTNode> parse_let_decl();
    Literal parse_literal();

    std::unique_ptr<ASTNode> parse_expr();
    std::unique_ptr<ASTNode> parse_primary();
    std::unique_ptr<ASTNode> parse_binary_expr(int minPrec);
    std::unique_ptr<ASTNode> parse_return_stmt();

    int get_precedence(TokenId op);
};

std::unique_ptr<ASTNode> parse(Tokenization &tokenization);

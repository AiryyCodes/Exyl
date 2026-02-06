#pragma once

#include "Tokenizer.h"
#include "Type.h"
#include <memory>
#include <string>
#include <variant>
#include <vector>

enum class NodeType
{
    Program,
    FuncDecl,
    VarDecl,
};

struct TypeRef
{
    // The parsed type name (ex. void, i32, string, etc...)
    std::string Name;
};

struct FuncDeclNode
{
    std::string Name;

    TypeRef ReturnTypeRef;
    Type *ReturnType;
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

    struct Type *ExprType = nullptr;
};

struct VarDeclNode
{
    std::string Name;
    Literal Initializer;

    Type *VarType = nullptr;
};

using ASTData = std::variant<std::monostate, FuncDeclNode, VarDeclNode>;

struct ASTNode
{
    NodeType Type;

    // Children of nodes that have a body (ex Program, FuncDecl, etc...)
    std::vector<std::unique_ptr<ASTNode>> Children;

    // Data for the node
    ASTData Data;

    void print();
};

struct Parser
{
    struct Tokenization Tokenization;

    int Index = 0;

    Token &current();
    Token advance();
    Token expect(TokenId id);

    bool is_at_end();

    std::unique_ptr<ASTNode> parse_program();
    std::unique_ptr<ASTNode> parse_func_decl();
    std::vector<std::unique_ptr<ASTNode>> parse_block();

    std::unique_ptr<ASTNode> parse_let_decl();
    Literal parse_literal();
};

std::unique_ptr<ASTNode> parse(Tokenization &tokenization);

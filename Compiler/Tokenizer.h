#pragma once

#include <string>
#include <vector>

enum class TokenId
{
    Symbol,

    String,

    Let,
    Fun,

    Equals,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Semicolon,

    Invalid,
    EndOfFile,
};

struct Token
{
    TokenId Id;
    std::string Name;
};

struct Tokenization
{
    std::vector<Token> Tokens;
};

struct Tokenizer
{
    Token CurrentToken{};

    std::string Code;
    int Cursor;
    int Line;

    std::vector<Token> Tokens;
};

void tokenize(const std::string &code, Tokenization &tokenization);

std::string get_token_id_name(TokenId id);

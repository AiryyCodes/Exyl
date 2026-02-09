#include "Tokenizer.h"
#include <cassert>
#include <cctype>
#include <cstdio>
#include <string>
#include <unordered_map>

static const std::unordered_map<std::string, TokenId> KEYWORDS = {
    {"let", TokenId::Let},
    {"fun", TokenId::Fun},
    {"return", TokenId::Return},
    {"extern", TokenId::Extern},
    {"true", TokenId::True},
    {"false", TokenId::False},
};

char advance(Tokenizer &tokenizer)
{
    if (tokenizer.Cursor >= tokenizer.Code.size())
        return '\0';

    return tokenizer.Code.at(tokenizer.Cursor++);
}

char peek(Tokenizer &tokenizer, int offset = 1)
{
    int index = tokenizer.Cursor + offset;
    if (index >= tokenizer.Code.size())
        return '\0';

    return tokenizer.Code[index];
}

char current(Tokenizer &tokenizer)
{
    if (tokenizer.Cursor >= tokenizer.Code.size())
        return '\0';

    return tokenizer.Code[tokenizer.Cursor];
}

void skip_whitespace(Tokenizer &tokenizer)
{
    while (true)
    {
        char c = current(tokenizer);
        if (c == ' ' || c == '\t' || c == '\r')
        {
            advance(tokenizer);
        }
        else if (c == '\n')
        {
            advance(tokenizer);
            tokenizer.Line++;
        }
        else if (c == '/' && peek(tokenizer) == '/')
        {
            while (current(tokenizer) != '\n' && tokenizer.Cursor < tokenizer.Code.size())
                advance(tokenizer);
        }
        else
        {
            break;
        }
    }
}

Token lex_symbol(Tokenizer &tokenizer, int start)
{
    while (std::isalnum(current(tokenizer)) || current(tokenizer) == '_')
        advance(tokenizer);

    auto text = tokenizer.Code.substr(start, tokenizer.Cursor - start);

    auto it = KEYWORDS.find(text);
    if (it != KEYWORDS.end())
        return {.Id = it->second, .Name = text};

    return {
        .Id = TokenId::Symbol,
        .Name = text,
    };
}

Token lex_string(Tokenizer &tokenizer, int start)
{
    std::string value;

    while (tokenizer.Cursor < tokenizer.Code.size())
    {
        char c = advance(tokenizer);

        // End of string
        if (c == '"')
        {
            return {
                .Id = TokenId::String,
                .Name = value,
            };
        }

        // Escape sequence
        if (c == '\\')
        {
            if (tokenizer.Cursor >= tokenizer.Code.size())
                break;

            char esc = advance(tokenizer);
            switch (esc)
            {
            case '"':
                value.push_back('"');
                break;
            case '\\':
                value.push_back('\\');
                break;
            case 'n':
                value.push_back('\n');
                break;
            case 't':
                value.push_back('\t');
                break;
            case 'r':
                value.push_back('\r');
                break;

            default:
                return {
                    .Id = TokenId::Invalid,
                    .Name = "Invalid escape sequence",
                };
            }
        }
        else
        {
            value.push_back(c);
        }
    }

    return {
        .Id = TokenId::Invalid,
        .Name = "Unterminated string",
    };
}

Token lex_number(Tokenizer &tokenizer, int start)
{
    while (isdigit(current(tokenizer)))
    {
        advance(tokenizer);
    }

    bool isFloat = false;

    if (current(tokenizer) == '.' && isdigit(peek(tokenizer)))
    {
        isFloat = true;

        // Consume '.'
        advance(tokenizer);

        while (isdigit(current(tokenizer)))
        {
            advance(tokenizer);
        }
    }

    return {
        .Id = isFloat ? TokenId::Float : TokenId::Int,
        .Name = tokenizer.Code.substr(start, tokenizer.Cursor - start),
    };
}

void make_token(Tokenizer &tokenizer, TokenId id, int start)
{
    tokenizer.Tokens.push_back({
        .Id = id,
        .Name = tokenizer.Code.substr(start, tokenizer.Cursor - start),
    });
}

void add_token(Tokenizer &tokenizer, const Token &token)
{
    tokenizer.Tokens.push_back(token);
}

void add_eof(Tokenizer &t)
{
    t.Tokens.push_back({TokenId::EndOfFile, "EOF"});
}

void tokenize(const std::string &code, Tokenization &tokenization)
{
    Tokenizer tokenizer{
        .Code = code,
        .Cursor = 0,
        .Line = 1,
    };

    Token token;

    while (true)
    {
        skip_whitespace(tokenizer);

        if (tokenizer.Cursor >= tokenizer.Code.size())
            break;

        int start = tokenizer.Cursor;
        char c = advance(tokenizer);

        switch (c)
        {
        case '=':
            make_token(tokenizer, TokenId::Equals, start);
            continue;
        case '!':
            if (peek(tokenizer) == '=')
            {
                // Consume '='
                advance(tokenizer);
                make_token(tokenizer, TokenId::NotEquals, start);
            }

        case '*':
            make_token(tokenizer, TokenId::Star, start);
            continue;
        case '/':
            make_token(tokenizer, TokenId::Slash, start);
            continue;
        case '+':
            make_token(tokenizer, TokenId::Plus, start);
            continue;
        case '-':
            make_token(tokenizer, TokenId::Minus, start);
            continue;

        case '(':
            make_token(tokenizer, TokenId::LParen, start);
            continue;
        case ')':
            make_token(tokenizer, TokenId::RParen, start);
            continue;
        case '{':
            make_token(tokenizer, TokenId::LBrace, start);
            continue;
        case '}':
            make_token(tokenizer, TokenId::RBrace, start);
            continue;

        case ',':
            make_token(tokenizer, TokenId::Comma, start);
            continue;
        case ':':
            make_token(tokenizer, TokenId::Colon, start);
            continue;
        case ';':
            make_token(tokenizer, TokenId::Semicolon, start);
            continue;

        case '"':
            add_token(tokenizer, lex_string(tokenizer, start));
            continue;
        }

        if (isdigit(c))
        {
            add_token(tokenizer, lex_number(tokenizer, start));
        }
        else if (isalpha(c))
        {
            add_token(tokenizer, lex_symbol(tokenizer, start));
        }
        else
        {
            add_token(tokenizer, {.Id = TokenId::Invalid, .Name = std::string(1, c)});
        }
    }

    add_eof(tokenizer);

    for (const auto &token : tokenizer.Tokens)
    {
        printf("%s: %s\n", get_token_id_name(token.Id).c_str(), token.Name.c_str());
    }

    tokenization.Tokens = tokenizer.Tokens;

    printf("\n");
}

std::string get_token_id_name(TokenId id)
{
    switch (id)
    {
    case TokenId::Symbol:
        return "Symbol";

    case TokenId::String:
        return "String";
    case TokenId::Int:
        return "Int";
    case TokenId::Float:
        return "Float";
    case TokenId::True:
        return "True";
    case TokenId::False:
        return "False";

    case TokenId::Let:
        return "Let";
    case TokenId::Fun:
        return "Fun";
    case TokenId::Return:
        return "Return";
    case TokenId::Extern:
        return "Extern";

    case TokenId::Equals:
        return "Equals";
    case TokenId::NotEquals:
        return "NotEquals";

    case TokenId::Star:
        return "Star";
    case TokenId::Slash:
        return "Slash";
    case TokenId::Plus:
        return "Plus";
    case TokenId::Minus:
        return "Minus";

    case TokenId::LParen:
        return "LParen";
    case TokenId::RParen:
        return "RParen";
    case TokenId::LBrace:
        return "LBrace";
    case TokenId::RBrace:
        return "RBrace";

    case TokenId::Comma:
        return "Comma";
    case TokenId::Colon:
        return "Colon";
    case TokenId::Semicolon:
        return "Semicolon";

    case TokenId::Invalid:
        return "Invalid";
    case TokenId::EndOfFile:
        return "EndOfFile";
    }

    return "";
}

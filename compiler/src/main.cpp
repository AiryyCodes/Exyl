#include "lexer.h"
#include "parser.h"
#include "token.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

const std::string TEST_SOURCE = R"(
let myVar: float = 69.69;

fun add(a: int, b: int): int
{
}

// TODO: Add checking if symbol already exists
let myVar: int = 69;
)";

Lexer lexer(TEST_SOURCE);
Parser parser(lexer);

void printTokens(const std::vector<Token> tokens)
{
    for (const auto &token : tokens)
    {
        printf("Token: Type: %s Value: %s Position: %i\n", tokenTypeToString(token.type).c_str(), token.value.c_str(), token.position);
    }
}

int main(int argc, char *argv[])
{
    auto ast = parser.Parse();
    for (const auto &node : ast)
    {
        node->Print();
    }

    std::vector<Token> tokens;
    Lexer newLexer(TEST_SOURCE);
    while (newLexer.HasNextToken())
    {
        tokens.push_back(newLexer.NextToken());
    }

    printTokens(tokens);

    return EXIT_SUCCESS;
}

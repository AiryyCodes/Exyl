#include "analyzer.h"
#include "lexer.h"
#include "parser.h"
#include "token.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <utility>
#include <vector>

const std::string TEST_SOURCE = R"(
let myVar: string = "Hello, World!";
let test_var: int = 69;

fun add(a: int, b: int): int
{
    let test_var1 = "This is a test var 1 in add scope";
    //let test_var1 = 69;
}

/*
fun add()
{

}
*/
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

    Analyzer analyzer(std::move(ast));
    analyzer.Analyze();

    /*
    std::vector<Token> tokens;
    Lexer newLexer(TEST_SOURCE);
    while (newLexer.HasNextToken())
    {
        tokens.push_back(newLexer.NextToken());
    }

    printTokens(tokens);
    */

    return EXIT_SUCCESS;
}

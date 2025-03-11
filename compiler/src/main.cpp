#include "analyzer.h"
#include "lexer.h"
#include "parser.h"
#include "codegen/codegen.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

const std::string TEST_SOURCE = R"(
let test_var0: double = 69;
let test_var1 = "Hello, Exyl!\n";
let test_var2 = "Hello, Main!\n";

extern printf(string): void;

fun main(): void
{
    printf("This is a string literal\n");
}
)";

Lexer lexer(TEST_SOURCE);
Parser parser(lexer);

using namespace llvm;

int main(int argc, char *argv[])
{
    auto ast = parser.Parse();
    for (const auto &node : ast)
    {
        node->Print();
    }

    Analyzer analyzer(ast);
    analyzer.Analyze();

    CodeGen codegen("exyl");
    codegen.Generate(ast);
    codegen.Compile("output.o");

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

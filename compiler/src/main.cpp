#include "analyzer.h"
#include "lexer.h"
#include "parser.h"
#include "codegen.h"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/IR/Value.h>

const std::string TEST_SOURCE = R"(
let test_var1 = "Hello, Exyl!\n";
//let test_var: int = 69;
let test_var2 = "Hello, Main!";

extern printf: void;

fun main()
{
    printf(test_var2);
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
    codegen.generate(ast);
    codegen.compile("output.o");

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

#include "CodeGen.h"
#include "Finish.h"
#include "Parser.h"
#include "Semantics.h"
#include "Tokenizer.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        printf("Usage: %s <input>.exl\n", argv[0]);
        return EXIT_FAILURE;
    }

    std::string inputFile;
    std::string outputFile = "a.out";
    bool shouldRun = false;
    bool shouldPrintAst = false;
    bool shouldPrintIr = false;
    bool isVerbose = false;

    for (int i = 1; i < argc; i++)
    {
        std::string arg = argv[i];

        if (arg == "-o" && i + 1 < argc)
        {
            outputFile = argv[i + 1];
            i++;
        }
        else if (arg == "--run")
        {
            shouldRun = true;
        }
        else if (arg == "--ast")
        {
            shouldPrintAst = true;
        }
        else if (arg == "--ir")
        {
            shouldPrintIr = true;
        }
        else if (arg == "--verbose")
        {
            isVerbose = true;
        }
        else if (arg[0] != '-')
        {
            inputFile = arg;
        }
        else
        {
            std::cerr << "Unknown option: " << arg << "\n";
        }
    }

    if (inputFile.empty())
    {
        std::cerr << "Usage: exylc <source> [-o output]\n";
        return EXIT_FAILURE;
    }

    std::ifstream stream(argv[1], std::ios_base::in);

    std::stringstream buffer;
    buffer << stream.rdbuf();

    Tokenization tokenization{};

    tokenize(buffer.str(), tokenization);

    auto root = parse(tokenization);
    if (!root)
    {
        printf("Failed to parse tokens\n");
        return EXIT_FAILURE;
    }

    SemanticAnalyzer semanticAnalyzer;
    semanticAnalyzer.analyze(root.get());

    if (semanticAnalyzer.has_errors())
    {
        fprintf(stderr, "Compilation failed: semantic errors detected\n");
        return EXIT_FAILURE;
    }

    if (shouldPrintAst)
    {
        root->print();
    }

    CodeGen codegen;
    codegen.emit_program(root.get());

    if (shouldPrintIr)
    {
        codegen.print();
    }

    if (shouldRun)
    {
        build_and_run(codegen.get_module(), outputFile, isVerbose);
    }
    else
    {
        std::string irFile = outputFile + ".ll";
        std::string exeFile = outputFile;

        if (!write_ir_to_file(codegen.get_module(), irFile))
        {
            llvm::errs() << "Failed to write IR file\n";
            return EXIT_FAILURE;
        }

        if (!compile_with_clang(irFile, exeFile, "Library", isVerbose))
        {
            llvm::errs() << "Clang compilation failed\n";
            return EXIT_FAILURE;
        }
    }

    return EXIT_SUCCESS;
}

#include "Parser.h"
#include "Tokenizer.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <print>
#include <sstream>
#include <string>

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <input>.exl\n", argv[0]);
        return EXIT_FAILURE;
    }

    std::string fileName = std::string(argv[1]);
    if (!fileName.ends_with(".exl"))
    {
        printf("Source file must end with \".exl\"\n");
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

    root->print();

    return EXIT_SUCCESS;
}

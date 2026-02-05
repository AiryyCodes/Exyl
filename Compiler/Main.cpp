#include "Tokenizer.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>

int main(int argc, char *argv[])
{
    Tokenization tokenization;

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

    tokenize(buffer.str(), tokenization);

    return EXIT_SUCCESS;
}

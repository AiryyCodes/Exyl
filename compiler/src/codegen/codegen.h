#pragma once

#include "ast.h"
#include "codegen/codegen_visitor.h"

#include <string>
#include <vector>
#include <cstdio>

class CodeGen
{
public:
    CodeGen(std::string moduleName)
        : moduleName(moduleName), visitor(moduleName) {}

    void Generate(std::vector<std::shared_ptr<ASTNode>> &nodes)
    {
        for (const auto &node : nodes)
        {
            node->Accept(visitor);
        }
    }

    void Compile(const std::string &output);

private:
    std::string moduleName;

    LLVMCodeGenVisitor visitor;
};

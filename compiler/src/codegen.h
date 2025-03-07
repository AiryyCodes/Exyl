#pragma once

#include "ast.h"

#include <cstdio>
#include <ostream>

class AsmGenerator
{
public:
    void generate(ASTNode *node, std::ostream &output)
    {
        if (auto varDecl = dynamic_cast<VariableDeclaration *>(node))
        {
            if (varDecl->m_Type == "int")
            {
                output << "section .data" << std::endl;
                output << "    " << varDecl->m_Name << " dd " << varDecl->m_Value.GetValueString() << std::endl;
            }
        }
        else if (auto funcDecl = dynamic_cast<FunctionDeclaration *>(node))
        {
            if (funcDecl->m_Name == "main")
            {
                // Define start function
                output << "section .text" << std::endl;
                output << "    global _start" << std::endl;
                output << "_start:" << std::endl;
                output << "    call main" << std::endl;
                output << "    mov rdi, rsi" << std::endl;
                output << "    mov rax, 60" << std::endl;
                output << "    syscall" << std::endl;

                // Define main function
                output << "main:" << std::endl;

                if (funcDecl->m_Type == "int")
                {
                    output << "    mov rax, 1" << std::endl;
                }
                output << "    ret";
            }
        }
    }
};

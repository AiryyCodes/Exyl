#include "Semantics.h"
#include "Parser.h"
#include "Type.h"

#include <cstdio>

void SemanticAnalyzer::analyze(ASTNode *root)
{
    m_Symbols.push_scope();

    visit(root);

    m_Symbols.pop_scope();
}

void SemanticAnalyzer::visit(ASTNode *node)
{
    switch (node->Type)
    {
    case NodeType::Program:
        visit_program(node);
        break;

    case NodeType::FuncDecl:
        visit_func_decl(node, std::get<FuncDeclNode>(node->Data));
        break;

    case NodeType::VarDecl:
        visit_var_decl(node, std::get<VarDeclNode>(node->Data));
        break;
    }
}

void SemanticAnalyzer::visit_program(ASTNode *node)
{
    for (const auto &child : node->Children)
    {
        visit(child.get());
    }
}

void SemanticAnalyzer::visit_func_decl(ASTNode *node, FuncDeclNode &func)
{
    func.ReturnType = func.ReturnTypeRef.Name.empty() ? &Types::Void : resolve_type(func.ReturnTypeRef.Name);

    auto *sym = new Symbol{
        func.Name,
        SymbolKind::Function,
        func.ReturnType};

    if (!m_Symbols.declare(sym))
    {
        printf("Semantic error: redeclared function '%s'\n", func.Name.c_str());
    }

    m_Symbols.push_scope();

    for (auto &child : node->Children)
        visit(child.get());

    m_Symbols.pop_scope();
}

void SemanticAnalyzer::visit_var_decl(ASTNode *, VarDeclNode &var)
{
    Type *initType = analyze_literal(var.Initializer);
    var.VarType = initType;

    auto *sym = new Symbol{
        .Name = var.Name,
        .Kind = SymbolKind::Variable,
        .TypeInfo = var.VarType,
    };

    if (!m_Symbols.declare(sym))
    {
        printf("Semantic error: redeclared variable '%s'\n", var.Name.c_str());
    }
}

Type *SemanticAnalyzer::analyze_literal(Literal &literal)
{
    switch (literal.Type)
    {
    case LiteralType::Int:
        return &Types::I64;
    case LiteralType::Float:
        return &Types::F32;
    case LiteralType::Bool:
        return &Types::Bool;
    case LiteralType::String:
        return &Types::String;
    }
    return &Types::Error;
}

Type *SemanticAnalyzer::resolve_type(const std::string &name)
{
    auto it = BuiltinTypeMap.find(name);
    if (it != BuiltinTypeMap.end())
        return it->second;

    // For now: unknown type = Error
    printf("Unknown type: %s\n", name.c_str());
    return &Types::Error;
}

#include "Semantics.h"
#include "Parser.h"
#include "Type.h"

#include <string>

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

    auto sym = std::make_unique<Symbol>(Symbol{
        func.Name,
        SymbolKind::Function,
        func.ReturnType});

    if (!m_Symbols.declare(std::move(sym)))
    {
        error("redeclared function '{}'", func.Name.c_str());
    }

    m_Symbols.push_scope();

    for (auto &child : node->Children)
        visit(child.get());

    m_Symbols.pop_scope();
}

void SemanticAnalyzer::visit_var_decl(ASTNode *, VarDeclNode &var)
{
    Type *initType = analyze_literal(var.Initializer);
    Type *declaredType = nullptr;
    if (!var.VarTypeRef.Name.empty())
    {
        declaredType = resolve_type(var.VarTypeRef.Name);
    }

    if (initType == &Types::Error || declaredType == &Types::Error)
    {
        var.VarType = &Types::Error;
        return;
    }

    Type *resultType = nullptr;

    if (declaredType)
    {
        if (!is_assignable(declaredType, initType))
        {
            error("Cannot assign {} to variable of type {}",
                  initType->get_name(),
                  declaredType->get_name());
            resultType = &Types::Error;
        }
        else
        {
            resultType = declaredType;
        }
    }
    else
    {
        resultType = initType;
    }

    var.VarType = resultType;

    auto sym = std::make_unique<Symbol>(Symbol{
        .Name = var.Name,
        .Kind = SymbolKind::Variable,
        .TypeInfo = var.VarType,
    });

    if (!m_Symbols.declare(std::move(sym)))
    {
        error("redeclared variable '{}'", var.Name);
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
    error("Unknown type: {}", name);
    return &Types::Error;
}

bool SemanticAnalyzer::is_assignable(Type *target, Type *value)
{
    if (target == value)
        return true;

    // TODO: Add implicit conversion (maybe)

    return false;
}

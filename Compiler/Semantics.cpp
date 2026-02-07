#include "Semantics.h"
#include "Parser.h"
#include "Tokenizer.h"
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

    case NodeType::ReturnStmt:
        visit_return_stmt(node);
        break;

    default:
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
        .Name = func.Name,
        .Kind = SymbolKind::Function,
        .TypeInfo = func.ReturnType,
    });

    if (!m_Symbols.declare(std::move(sym)))
    {
        error("redeclared function '{}'", func.Name.c_str());
    }

    m_Symbols.push_scope();

    Type *prevReturn = m_CurrentFunctionReturn;
    m_CurrentFunctionReturn = func.ReturnType;

    for (auto &param : func.Params)
    {
        Type *paramType = resolve_type(param.Type.Name);

        auto paramSym = std::make_unique<Symbol>(Symbol{
            .Name = param.Name,
            .Kind = SymbolKind::Variable,
            .TypeInfo = paramType,
        });

        if (!m_Symbols.declare(std::move(paramSym)))
        {
            error("redeclared parameter '{}'", param.Name);
        }
    }

    for (auto &child : node->Children)
        visit(child.get());

    m_CurrentFunctionReturn = prevReturn;

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

Type *SemanticAnalyzer::visit_expr(ASTNode *node)
{
    switch (node->Type)
    {
    case NodeType::LiteralExpr:
        return visit_literal_expr(std::get<LiteralExprNode>(node->Data));
    case NodeType::IdentifierExpr:
        return visit_identifier_expr(std::get<IdentifierExprNode>(node->Data));
    case NodeType::BinaryExpr:
        return visit_binary_expr(std::get<BinaryExprNode>(node->Data));
    default:
        error("Unexpected node type in expression");
        return &Types::Error;
    }
}
Type *SemanticAnalyzer::visit_literal_expr(LiteralExprNode &node)
{
    switch (node.LitType)
    {
    case LiteralExprNode::TypeKind::Int:
        node.ExprType = &Types::I64;
        break;
    case LiteralExprNode::TypeKind::Float:
        node.ExprType = &Types::F32;
        break;
    case LiteralExprNode::TypeKind::String:
        node.ExprType = &Types::String;
        break;
    default:
        node.ExprType = &Types::Error;
        break;
    }
    return node.ExprType;
}

Type *SemanticAnalyzer::visit_identifier_expr(IdentifierExprNode &node)
{
    Symbol *sym = m_Symbols.lookup(node.Name);
    if (!sym)
    {
        error("Use of undeclared identifier '{}'", node.Name);
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    node.ExprType = sym->TypeInfo;
    return node.ExprType;
}

Type *SemanticAnalyzer::visit_binary_expr(BinaryExprNode &node)
{
    Type *lhs = visit_expr(node.LHS.get());
    Type *rhs = visit_expr(node.RHS.get());

    if (lhs == &Types::Error || rhs == &Types::Error)
    {
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    if (lhs != rhs)
    {
        error("Operator '{}' requires matching operand types, got '{}' and '{}'",
              get_token_id_name(node.Op),
              lhs->get_name(),
              rhs->get_name());
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    node.ExprType = lhs;
    return lhs;
}

Type *SemanticAnalyzer::visit_return_stmt(ASTNode *node)
{
    auto &ret = std::get<ReturnStmtNode>(node->Data);
    Type *exprType = nullptr;
    if (ret.Expr)
        exprType = visit_expr(ret.Expr.get());

    if (exprType == &Types::Error)
        return exprType;

    if (!is_assignable(m_CurrentFunctionReturn, exprType))
    {
        error("cannot return '{}' from a function returning '{}'",
              exprType->get_name(),
              m_CurrentFunctionReturn->get_name());
    }

    return exprType;
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

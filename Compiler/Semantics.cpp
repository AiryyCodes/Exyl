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
    case NodeType::FuncCall: // <-- add this
        visit_func_call_stmt(node, std::get<FuncCallNode>(node->Data));
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
    func.ReturnType = func.ReturnTypeRef.Name.empty() ? &Types::Unknown : resolve_type(func.ReturnTypeRef.Name);

    auto funcType = std::make_unique<FunctionType>();
    funcType->ReturnType = func.ReturnType;

    for (auto &param : func.Params)
    {
        auto paramType = resolve_type(param.Type.Name);
        param.ResolvedType = paramType;
        funcType->ParamTypes.push_back(paramType);
    }

    auto sym = std::make_unique<Symbol>(Symbol{
        .Name = func.Name,
        .Kind = SymbolKind::Function,
        .TypeInfo = nullptr,
        .FunctionInfo = std::move(funcType),
    });

    if (!m_Symbols.declare(std::move(sym)))
    {
        error("redeclared function '{}'", func.Name.c_str());
    }

    if (func.IsExtern)
    {
        return;
    }

    m_Symbols.push_scope();

    Type *prevReturn = m_CurrentFunctionReturn;
    Type *prevInferred = m_InferredReturnType;

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

    if (func.ReturnType == &Types::Unknown)
    {
        if (!m_InferredReturnType)
        {
            func.ReturnType = &Types::Void;
        }
        else
        {
            func.ReturnType = m_InferredReturnType;
        }
    }

    m_CurrentFunctionReturn = prevReturn;
    m_InferredReturnType = nullptr;

    m_Symbols.pop_scope();
}

void SemanticAnalyzer::visit_func_call_stmt(ASTNode *node, FuncCallNode &call)
{
    Type *retType = visit_func_call_expr(node, call);

    // warn if the function has a non-void return type
    if (retType != &Types::Void && retType != &Types::Error)
    {
        printf("Warning: Ignoring return value of function '%s'\n", call.Name.c_str());
    }
}

Type *SemanticAnalyzer::visit_func_call_expr(ASTNode *node, FuncCallNode &call)
{
    Symbol *sym = m_Symbols.lookup(call.Name);
    if (!sym || sym->Kind != SymbolKind::Function)
    {
        error("call to undeclared function '{}'", call.Name);
        return &Types::Error;
    }

    auto &func = sym->FunctionInfo;

    if (call.Args.size() != func->ParamTypes.size())
    {
        error("function '{}' expects {} arguments, got {}",
              call.Name,
              func->ParamTypes.size(),
              call.Args.size());
        return &Types::Error;
    }

    for (size_t i = 0; i < call.Args.size(); ++i)
    {
        Type *argType = visit_expr(call.Args[i].get());
        Type *paramType = func->ParamTypes[i];

        if (!is_assignable(paramType, argType))
        {
            error("cannot pass '{}' as argument {} to parameter of type '{}'",
                  argType->get_name(),
                  i + 1,
                  paramType->get_name());
        }
    }

    return func->ReturnType;
}

void SemanticAnalyzer::visit_var_decl(ASTNode *, VarDeclNode &var)
{
    Type *initType = visit_expr(var.Initializer.get());
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
    case NodeType::FuncCall:
        return visit_func_call_expr(node, std::get<FuncCallNode>(node->Data));
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
    case LiteralExprNode::TypeKind::Bool:
        node.ExprType = &Types::Bool;
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

    auto *lhsBT = dynamic_cast<BuiltinType *>(lhs);
    auto *rhsBT = dynamic_cast<BuiltinType *>(rhs);

    if (!lhsBT || !rhsBT)
    {
        error("Operator '{}' not supported for this types",
              get_token_id_name(node.Op));
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    if (!lhs->is_numeric())
    {
        error("Operator '{}' not supported for type '{}'",
              get_token_id_name(node.Op),
              lhs->get_name());
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    if (lhsBT->kind != rhsBT->kind)
    {
        error("Operator '{}' requires matching operand types, got '{}' and '{}'",
              get_token_id_name(node.Op),
              lhs->get_name(),
              rhs->get_name());
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    using K = BuiltinType::Kind;

    switch (lhsBT->kind)
    {
    case K::I32:
    case K::I64:
        switch (node.Op)
        {
        case TokenId::Plus:
            node.ResolvedOp = BinaryExprNode::OpKind::IntAdd;
            break;
        case TokenId::Minus:
            node.ResolvedOp = BinaryExprNode::OpKind::IntSub;
            break;
        case TokenId::Star:
            node.ResolvedOp = BinaryExprNode::OpKind::IntMul;
            break;
        case TokenId::Slash:
            node.ResolvedOp = BinaryExprNode::OpKind::IntDiv;
            break;
        default:
            goto invalid;
        }
        break;

    case K::F32:
    case K::F64:
        switch (node.Op)
        {
        case TokenId::Plus:
            node.ResolvedOp = BinaryExprNode::OpKind::FloatAdd;
            break;
        case TokenId::Minus:
            node.ResolvedOp = BinaryExprNode::OpKind::FloatSub;
            break;
        case TokenId::Star:
            node.ResolvedOp = BinaryExprNode::OpKind::FloatMul;
            break;
        case TokenId::Slash:
            node.ResolvedOp = BinaryExprNode::OpKind::FloatDiv;
            break;
        default:
            goto invalid;
        }
        break;

    default:
    invalid:
        error("Operator '{}' not supported for type '{}'",
              get_token_id_name(node.Op),
              lhs->get_name());
        node.ExprType = &Types::Error;
        return node.ExprType;
    }

    node.ExprType = lhs;
    return lhs;
}

Type *SemanticAnalyzer::visit_return_stmt(ASTNode *node)
{
    auto &ret = std::get<ReturnStmtNode>(node->Data);
    Type *exprType = ret.Expr ? visit_expr(ret.Expr.get()) : &Types::Void;

    if (exprType == &Types::Error)
        return exprType;

    if (m_CurrentFunctionReturn != &Types::Unknown)
    {
        if (!is_assignable(m_CurrentFunctionReturn, exprType))
        {
            error("cannot return '{}' from a function returning '{}'",
                  exprType->get_name(),
                  m_CurrentFunctionReturn->get_name());
        }
        return exprType;
    }

    if (!m_InferredReturnType)
    {
        m_InferredReturnType = exprType;
    }
    else if (!is_assignable(m_InferredReturnType, exprType))
    {
        error("inconsistent return types: '{}' vs '{}'",
              m_InferredReturnType->get_name(),
              exprType->get_name());
        m_InferredReturnType = &Types::Error;
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

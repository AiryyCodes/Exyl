#pragma once

#include "codegen/codegen_visitor.h"
#include "type.h"
#include <cstdio>
#include <memory>
#include <string>
#include <utility>
#include <vector>

class ASTNode
{
public:
    virtual ~ASTNode() = default;

    virtual void Accept(CodeGenVisitor &visitor) = 0;
    virtual void Print() = 0;
};

class Literal : public ASTNode
{
public:
    Literal(const std::string &name, const Type &type, Value &value)
        : name(name), type(type), value(value) {}

    virtual void Print() override
    {
        printf("Literal: Name: %s Type: %s Value: %s\n", name.c_str(), TypeToString(type).c_str(), value.GetValueString().c_str());
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        // visitor.Visit(*this);
    }

    std::string name;
    Type type;
    Value value;
};

class VariableDeclaration : public ASTNode
{
public:
    VariableDeclaration(const std::string &name, const std::string &type, Value value)
        : name(name), type(type), value(value) {}

    virtual void Print() override
    {
        printf("Variable Declaration: Name: %s Type: %s Value: %s Value Type: %s\n", name.c_str(), type.c_str(), value.GetValueString().c_str(), TypeToString(value.type).c_str());
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        visitor.Visit(*this);
    }

    std::string name;
    std::string type;
    Value value;
};

class Parameter : public ASTNode
{
public:
    Parameter(const std::string &name, const std::string &type)
        : name(name), type(type) {}

    virtual void Print() override
    {
        printf("Parameter: Name: %s Type: %s\n", name.c_str(), type.c_str());
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        // visitor.Visit(*this);
    }

    std::string name;
    std::string type;
};

class FunctionBody : public ASTNode
{
public:
    FunctionBody(const std::vector<std::shared_ptr<ASTNode>> &body)
        : body(body) {}

    virtual void Print() override
    {
        for (const auto &child : body)
        {
            child->Print();
        }
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        // visitor.Visit(*this);
    }

    std::vector<std::shared_ptr<ASTNode>> body;
};

class FunctionDeclaration : public ASTNode
{
public:
    FunctionDeclaration(const std::string &name, const std::string &type, std::vector<std::unique_ptr<Parameter>> parameters)
        : name(name), type(type), parameters(std::move(parameters)), body(nullptr) {}
    FunctionDeclaration(const std::string &name, const std::string &type, std::vector<std::unique_ptr<Parameter>> parameters, std::unique_ptr<FunctionBody> body)
        : name(name), type(type), parameters(std::move(parameters)), body(std::move(body)) {}

    virtual void Print() override
    {
        printf("Function Declaration: Name: %s Type: %s\n", name.c_str(), type.c_str());
        for (const auto &param : parameters)
        {
            param->Print();
        }

        if (body)
        {
            body->Print();
        }
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        visitor.Visit(*this);
    }

    std::string name;
    std::string type;
    std::vector<std::unique_ptr<Parameter>> parameters;
    std::unique_ptr<FunctionBody> body;
};

class FunctionCall : public ASTNode
{
public:
    FunctionCall(const std::string &callee, std::vector<std::shared_ptr<ASTNode>> args)
        : callee(callee), args(std::move(args)) {}

    virtual void Print() override
    {
        printf("Function Call: Callee: %s\n", callee.c_str());

        for (const auto &arg : args)
        {
            arg->Print();
        }
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        // visitor.Visit(*this);
    }

    std::string callee;
    std::vector<std::shared_ptr<ASTNode>> args;
};

class ExternStatement : public ASTNode
{
public:
    ExternStatement(const std::string &name, const std::string &type, const std::vector<std::string> &parameters = {})
        : name(name), type(type), parameters(parameters) {}

    virtual void Print() override
    {
        printf("Extern Statement: Name: %s Type: %s\n", name.c_str(), type.c_str());
        for (const auto &param : parameters)
        {
            printf("Extern Param: Type: %s\n", param.c_str());
        }
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        visitor.Visit(*this);
    }

    std::string name;
    std::string type;
    std::vector<std::string> parameters;
};

class VariableExpression : public ASTNode
{
public:
    VariableExpression(const std::string &name)
        : name(name) {}

    virtual void Print() override
    {
        printf("Variable Expression: Name: %s\n", name.c_str());
    }

    void Accept(CodeGenVisitor &visitor) override
    {
        // visitor.Visit(*this);
    }

    std::string name;
};

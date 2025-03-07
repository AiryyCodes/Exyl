#pragma once

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

    virtual void Print() = 0;
};

class Literal : public ASTNode
{
public:
    Literal(const std::string &name, const Type &type, Value &value)
        : m_Name(name), m_Type(type), m_Value(value) {}

    virtual void Print() override
    {
        printf("Literal: Name: %s Type: %s Value: %s\n", m_Name.c_str(), typeToString(m_Type).c_str(), m_Value.GetValueString().c_str());
    }

    std::string m_Name;
    Type m_Type;
    Value m_Value;
};

class VariableDeclaration : public ASTNode
{
public:
    VariableDeclaration(const std::string &name, const std::string &type, Value value)
        : m_Name(name), m_Type(type), m_Value(value) {}

    virtual void Print() override
    {
        printf("Variable Declaration: Name: %s Type: %s Value: %s Value Type: %s\n", m_Name.c_str(), m_Type.c_str(), m_Value.GetValueString().c_str(), typeToString(m_Value.m_Type).c_str());
    }

    std::string m_Name;
    std::string m_Type;
    Value m_Value;
};

class Parameter : public ASTNode
{
public:
    Parameter(const std::string &name, const std::string &type)
        : m_Name(name), m_Type(type) {}

    virtual void Print() override
    {
        printf("Parameter: Name: %s Type: %s\n", m_Name.c_str(), m_Type.c_str());
    }

    std::string m_Name;
    std::string m_Type;
};

class FunctionBody : public ASTNode
{
public:
    FunctionBody(const std::vector<std::shared_ptr<ASTNode>> &body)
        : m_Body(body) {}

    virtual void Print() override
    {
        for (const auto &child : m_Body)
        {
            child->Print();
        }
    }

    std::vector<std::shared_ptr<ASTNode>> m_Body;
};

class FunctionDeclaration : public ASTNode
{
public:
    FunctionDeclaration(const std::string &name, const std::string &type, std::vector<std::unique_ptr<Parameter>> parameters)
        : m_Name(name), m_Type(type), m_Parameters(std::move(parameters)), m_Body(nullptr) {}
    FunctionDeclaration(const std::string &name, const std::string &type, std::vector<std::unique_ptr<Parameter>> parameters, std::unique_ptr<FunctionBody> body)
        : m_Name(name), m_Type(type), m_Parameters(std::move(parameters)), m_Body(std::move(body)) {}

    virtual void Print() override
    {
        printf("Function Declaration: Name: %s Type: %s\n", m_Name.c_str(), m_Type.c_str());
        for (const auto &param : m_Parameters)
        {
            param->Print();
        }

        if (m_Body)
        {
            m_Body->Print();
        }
    }

    std::string m_Name;
    std::string m_Type;
    std::vector<std::unique_ptr<Parameter>> m_Parameters;
    std::unique_ptr<FunctionBody> m_Body;
};

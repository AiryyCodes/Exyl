#pragma once

#include "type.h"
#include <cstdio>
#include <memory>
#include <string>
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
        printf("Literal: Name: %s Type: %s Value: %s\n", m_Name.c_str(), typeToString(m_Type).c_str(), m_Value.GetTypeString().c_str());
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
        printf("Variable Declaration: Name: %s Type: %s Value: %s Value Type: %s\n", m_Name.c_str(), m_Type.c_str(), m_Value.GetTypeString().c_str(), typeToString(m_Value.m_Type).c_str());
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

class FunctionDeclaration : public ASTNode
{
public:
    FunctionDeclaration(const std::string &name, const std::string &type, std::vector<std::unique_ptr<Parameter>> parameters)
        : m_Name(name), m_Type(type), m_Parameters(std::move(parameters)) {}

    virtual void Print() override
    {
        printf("Function Declaration: Name: %s Type: %s\n", m_Name.c_str(), m_Type.c_str());
        for (const auto &param : m_Parameters)
        {
            param->Print();
        }
    }

    std::string m_Name;
    std::string m_Type;
    std::vector<std::unique_ptr<Parameter>> m_Parameters;
};

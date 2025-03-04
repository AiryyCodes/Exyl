#pragma once

#include <cstdint>
#include <iostream>
#include <string>
#include <variant>

enum class Type
{
    UNKNOWN,
    INT8,
    INT16,
    INT32,
    INT64,
    STRING,
};

class Value
{
public:
    Value(int8_t int8) : m_Value(int8), m_Type(Type::INT8) {}
    Value(int16_t int16) : m_Value(int16), m_Type(Type::INT16) {}
    Value(int32_t int32) : m_Value(int32), m_Type(Type::INT32) {}
    Value(int64_t int64) : m_Value(int64), m_Type(Type::INT64) {}
    Value(std::string string) : m_Value(string), m_Type(string.empty() ? Type::UNKNOWN : Type::STRING) {}

    std::string GetTypeString()
    {
        return std::visit([](auto &&val)
                          {
                using T = std::decay_t<decltype(val)>;
                if constexpr (std::is_same_v<T, std::string>)
                    return val;
                else if constexpr (std::is_same_v<T, bool>)
                    return val ? "true" : "false";
                else
                    return std::to_string(val); }, m_Value);
    }

    void Print()
    {
        std::visit([](auto &&val)
                   { std::cout << val << std::endl; }, m_Value);
    };

    Type m_Type;

    std::variant<int8_t, int16_t, int32_t, int64_t, std::string> m_Value;
};

static std::string typeToString(Type type)
{
    switch (type)
    {
    case Type::INT8:
        return "INT8";
    case Type::INT16:
        return "INT16";
    case Type::INT32:
        return "INT32";
    case Type::INT64:
        return "INT64";
    case Type::STRING:
        return "STRING";
    default:
        return "UNKNOWN";
    };
}

#pragma once

#include <cstdint>
#include <cstdio>
#include <exception>
#include <limits>
#include <string>
#include <variant>

enum class Type
{
    UNKNOWN,
    VOID,
    INT8,
    INT16,
    INT32,
    INT64,
    STRING,
    FLOAT
};

class Value
{
public:
    Value() : value(std::monostate{}), type(Type::UNKNOWN) {}
    Value(int8_t int8) : value(int8), type(Type::INT8) {}
    Value(int16_t int16) : value(int16), type(Type::INT16) {}
    Value(int32_t int32) : value(int32), type(Type::INT32) {}
    Value(int64_t int64) : value(int64), type(Type::INT64) {}
    Value(std::string string) : value(string), type(string.empty() ? Type::UNKNOWN : Type::STRING) {}
    Value(float value) : value(value), type(Type::FLOAT) {}

    bool IsEmpty() const
    {
        return std::holds_alternative<std::monostate>(value);
    }

    std::string GetValueString() const
    {
        if (IsEmpty())
            return "<empty>";

        return std::visit([](const auto &val) -> std::string
                          {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, std::string>)
                return val;
            else if constexpr (std::is_same_v<T, bool>)
                return val ? "true" : "false";
            else if constexpr (std::is_same_v<T, int8_t> || std::is_same_v<T, int16_t> || std::is_same_v<T, int32_t> || std::is_same_v<T, int64_t>)
                return std::to_string(static_cast<int64_t>(val));
            else if constexpr (std::is_same_v<T, float>)
                return std::to_string(static_cast<float>(val));
            else
                return "<unsupported type>"; }, value);
    }

    Type type;

    std::variant<std::monostate, int8_t, int16_t, int32_t, int64_t, std::string, float> value;
};

static std::string TypeToString(Type type);

template <typename T>
static Value getValueFromString(const std::string &string, Type type)
{
    try
    {
        if (type == Type::INT8 || type == Type::INT16 || type == Type::INT32 || type == Type::INT64)
        {

            int64_t parsedValue = std::stoll(string);

            // Ensure parsedValue is within range
            auto limitMin = static_cast<int64_t>(std::numeric_limits<T>::min());
            auto limitMax = static_cast<int64_t>(std::numeric_limits<T>::max());

            if (parsedValue < limitMin || parsedValue > limitMax)
            {
                printf("Error: Value '%s' exceeds the allowed range for type '%s'. Value must be between %lld and %lld.\n",
                       string.c_str(), TypeToString(type).c_str(), static_cast<long long int>(limitMin), static_cast<long long int>(limitMax));
                exit(1);
            }

            return Value(static_cast<T>(parsedValue));
        }
        else if (type == Type::FLOAT)
        {
            float parsedValue = std::stof(string);

            // Ensure parsedValue is within range
            auto limitMin = static_cast<float>(std::numeric_limits<T>::min());
            auto limitMax = static_cast<float>(std::numeric_limits<T>::max());

            if (parsedValue < limitMin || parsedValue > limitMax)
            {
                printf("Error: Value '%s' exceeds the allowed range for type '%s'. Value must be between %lld and %lld.\n",
                       string.c_str(), TypeToString(type).c_str(), static_cast<long long int>(limitMin), static_cast<long long int>(limitMax));
                exit(1);
            }

            return Value(static_cast<T>(parsedValue));
        }
        else
        {
            printf("Error: Unknown type '%s' for value '%s'.\n", TypeToString(type).c_str(), string.c_str());
            exit(1);
        }
    }
    catch (const std::exception &)
    {
        printf("Error: Invalid value: '%s' for type '%s'\n", string.c_str(), TypeToString(type).c_str());
        exit(1);
    }
}

static Type stringToType(const std::string &type)
{
    if (type == "void")
    {
        return Type::VOID;
    }
    else if (type == "int8")
    {
        return Type::INT8;
    }
    else if (type == "int16")
    {
        return Type::INT16;
    }
    else if (type == "int32" || type == "int")
    {
        return Type::INT32;
    }
    else if (type == "int64")
    {
        return Type::INT64;
    }
    else if (type == "string")
    {
        return Type::STRING;
    }
    else if (type == "float")
    {
        return Type::FLOAT;
    }
    else
    {
        return Type::UNKNOWN;
    }
}

static std::string TypeToString(Type type)
{
    switch (type)
    {
    case Type::VOID:
        return "VOID";
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
    case Type::FLOAT:
        return "FLOAT";
    default:
        return "UNKNOWN";
    };
}

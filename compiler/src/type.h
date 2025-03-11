#pragma once

#include <cstdint>
#include <cstdio>
#include <functional>
#include <limits>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>

class Type
{
public:
    enum class Kind
    {
        Unknown,
        Void,
        Int8,
        Int16,
        Int32,
        Int64,
        String,
        Float
    };

    Type() : kind(Kind::Unknown) {}
    explicit Type(Kind kind) : kind(kind) {}

    Kind GetKind() const { return kind; }
    std::string ToString() const
    {
        switch (kind)
        {
        case Kind::Unknown:
            return "unknown";
        case Kind::Void:
            return "void";
        case Kind::Int8:
            return "int8";
        case Kind::Int16:
            return "int16";
        case Kind::Int32:
            return "int32";
        case Kind::Int64:
            return "int64";
        case Kind::String:
            return "string";
        case Kind::Float:
            return "float";
        }
        return "unknown";
    }

    static std::string ToString(Kind kind)
    {
        Type type(kind);
        return type.ToString();
    }

    static Kind FromString(const std ::string &string)
    {
        if (string == "void")
        {
            return Kind::Void;
        }
        else if (string == "int8")
        {
            return Kind::Int8;
        }
        else if (string == "int16")
        {
            return Kind::Int16;
        }
        else if (string == "int32" || string == "int")
        {
            return Kind::Int32;
        }
        else if (string == "int64")
        {
            return Kind::Int64;
        }
        else if (string == "string")
        {
            return Kind::String;
        }
        else if (string == "float")
        {
            return Kind::Float;
        }

        // TODO: Return custom type
        return Kind::Unknown;
    }

    bool operator==(const Type &other) { return kind == other.kind; }
    bool operator!=(const Type &other) { return kind != other.kind; }

private:
    Kind kind;
};

class Value
{
public:
    Value(int8_t int8) : value(int8), type(Type::Kind::Int8) {}
    Value(int16_t int16) : value(int16), type(Type::Kind::Int16) {}
    Value(int32_t int32) : value(int32), type(Type::Kind::Int32) {}
    Value(int64_t int64) : value(int64), type(Type::Kind::Int64) {}
    Value(std::string string) : value(string), type(Type::Kind::String) {}
    Value(float value) : value(value), type(Type::Kind::Float) {}

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

    Type GetType() const { return type; }

    auto GetValue() { return value; }

private:
    Type type;
    std::variant<std::monostate, int8_t, int16_t, int32_t, int64_t, std::string, float> value;
};

template <typename T>
std::optional<Value> getValueFromString(const std::string &str, Type::Kind kind)
{
    std::istringstream iss(str);
    T convertedValue;
    if (!(iss >> convertedValue))
    {
        printf("Error: Failed to convert '%s' to type '%s'.\n", str.c_str(), Type(kind).ToString().c_str());
        return std::nullopt;
    }

    // Range check (optional)
    if constexpr (std::is_integral_v<T>)
    {
        auto minValue = std::numeric_limits<T>::min();
        auto maxValue = std::numeric_limits<T>::max();
        if (convertedValue < minValue || convertedValue > maxValue)
        {
            printf("Error: Value '%s' is out of range for type '%s'.\n", str.c_str(), Type(kind).ToString().c_str());
            return std::nullopt;
        }
    }

    return Value(convertedValue);
}

using TypeConverter = std::function<std::optional<Value>(const std::string &)>;

const std::unordered_map<std::string, TypeConverter> typeHandlers = {
    {"int8", [](const std::string &val)
     { return getValueFromString<int8_t>(val, Type::Kind::Int8); }},
    {"int16", [](const std::string &val)
     { return getValueFromString<int16_t>(val, Type::Kind::Int16); }},
    {"int32", [](const std::string &val)
     { return getValueFromString<int32_t>(val, Type::Kind::Int32); }},
    {"int", [](const std::string &val)
     { return getValueFromString<int32_t>(val, Type::Kind::Int32); }},
    {"int64", [](const std::string &val)
     { return getValueFromString<int64_t>(val, Type::Kind::Int64); }},
    {"float", [](const std::string &val)
     { return getValueFromString<float>(val, Type::Kind::Float); }},
    {"string", [](const std::string &val)
     { return std::optional<Value>(Value(val)); }},
};

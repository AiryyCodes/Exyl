#pragma once

#include <string>
#include <unordered_map>

struct Type
{
    virtual ~Type() = default;

    virtual std::string get_name() const = 0;
};

struct BuiltinType : Type
{
    enum class Kind
    {
        I32,
        I64,

        F32,
        F64,

        Bool,
        String,
        Void,

        Unknown,
        Error,
    };
    Kind kind;

    explicit BuiltinType(Kind k) : kind(k) {}

    std::string get_name() const override
    {
        switch (kind)
        {
        case Kind::I32:
            return "i32";
        case Kind::I64:
            return "i64";

        case Kind::F32:
            return "f32";
        case Kind::F64:
            return "f64";

        case Kind::Bool:
            return "bool";
        case Kind::String:
            return "string";
        case Kind::Void:
            return "void";

        case Kind::Unknown:
            return "<unknown>";
        case Kind::Error:
            return "<error>";
        }
        return "<unknown>";
    }
};

namespace Types
{
inline BuiltinType I32{BuiltinType::Kind::I32};
inline BuiltinType I64{BuiltinType::Kind::I64};
inline BuiltinType F64{BuiltinType::Kind::F64};
inline BuiltinType F32{BuiltinType::Kind::F32};
inline BuiltinType Bool{BuiltinType::Kind::Bool};
inline BuiltinType String{BuiltinType::Kind::String};
inline BuiltinType Void{BuiltinType::Kind::Void};
inline BuiltinType Unknown{BuiltinType::Kind::Unknown};
inline BuiltinType Error{BuiltinType::Kind::Error};
} // namespace Types

inline std::unordered_map<std::string, Type *> BuiltinTypeMap = {
    {"void", &Types::Void},
    {"i32", &Types::I32},
    {"i64", &Types::I64},
    {"f32", &Types::F32},
    {"f64", &Types::F64},
    {"bool", &Types::Bool},
    {"string", &Types::String},
};

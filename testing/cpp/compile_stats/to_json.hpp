#include <rapidjson/document.h>
#include <type_traits>

#include <cppdecl/declarations/to_string.h>

namespace cppdecl {
using JsonValue = rapidjson::Value;
using JsonAlloc = rapidjson::Document::AllocatorType;

template <typename E>
[[nodiscard]] inline auto ToJsonEnum(E value) {
    static_assert(std::is_enum_v<E>);
    return static_cast<std::underlying_type_t<E>>(value);
}

[[nodiscard]] inline JsonValue ToJsonString(
    std::string_view s,
    JsonAlloc&       alloc) {
    JsonValue v;
    v.SetString(
        s.data(), static_cast<rapidjson::SizeType>(s.size()), alloc);
    return v;
}

[[nodiscard]] inline JsonValue ToJson(
    const std::string& target,
    JsonAlloc&         alloc) {
    return ToJsonString(target, alloc);
}

[[nodiscard]] inline JsonValue ToJson(
    const CvQualifiers& target,
    JsonAlloc&) {
    return JsonValue(ToJsonEnum(target));
}

[[nodiscard]] inline JsonValue ToJson(
    const RefQualifier& target,
    JsonAlloc&) {
    return JsonValue(ToJsonEnum(target));
}

[[nodiscard]] inline JsonValue ToJson(
    const SimpleTypePrefix& target,
    JsonAlloc&) {
    return JsonValue(ToJsonEnum(target));
}

[[nodiscard]] inline JsonValue ToJson(
    const PunctuationToken& target,
    JsonAlloc&              alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("value", ToJsonString(target.value, alloc), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const NumericLiteral& target,
    JsonAlloc&            alloc) {
    JsonValue obj(rapidjson::kObjectType);

    std::visit(
        Overload{
            [&](const NumericLiteral::Integer& i) {
                obj.AddMember("kind", "integer", alloc);
                obj.AddMember("base", ToJsonEnum(i.base), alloc);
                obj.AddMember(
                    "value", ToJsonString(i.value, alloc), alloc);

                JsonValue suffix(rapidjson::kObjectType);
                std::visit(
                    Overload{
                        [&](std::string_view str) {
                            suffix.AddMember("kind", "udl", alloc);
                            suffix.AddMember(
                                "value", ToJsonString(str, alloc), alloc);
                        },
                        [&](const NumericLiteral::Integer::Suffix& s) {
                            suffix.AddMember("kind", "builtin", alloc);
                            suffix.AddMember(
                                "is_unsigned", s.is_unsigned, alloc);
                            suffix.AddMember(
                                "signed_part",
                                ToJsonEnum(s.signed_part),
                                alloc);
                        },
                    },
                    i.suffix);

                obj.AddMember("suffix", std::move(suffix), alloc);
            },
            [&](const NumericLiteral::FloatingPoint& f) {
                obj.AddMember("kind", "floating", alloc);
                obj.AddMember("base", ToJsonEnum(f.base), alloc);
                obj.AddMember(
                    "value_int", ToJsonString(f.value_int, alloc), alloc);

                if (f.value_frac) {
                    obj.AddMember(
                        "value_frac",
                        ToJsonString(*f.value_frac, alloc),
                        alloc);
                } else {
                    obj.AddMember(
                        "value_frac",
                        JsonValue(rapidjson::kNullType),
                        alloc);
                }

                obj.AddMember(
                    "value_exp", ToJsonString(f.value_exp, alloc), alloc);

                JsonValue suffix(rapidjson::kObjectType);
                std::visit(
                    Overload{
                        [&](std::string_view str) {
                            suffix.AddMember("kind", "udl", alloc);
                            suffix.AddMember(
                                "value", ToJsonString(str, alloc), alloc);
                        },
                        [&](NumericLiteral::FloatingPoint::Suffix s) {
                            suffix.AddMember("kind", "builtin", alloc);
                            suffix.AddMember(
                                "value", ToJsonEnum(s), alloc);
                        },
                    },
                    f.suffix);

                obj.AddMember("suffix", std::move(suffix), alloc);
            },
        },
        target.var);

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const StringOrCharLiteral& target,
    JsonAlloc&                 alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("type", ToJsonEnum(target.type), alloc);
    obj.AddMember("kind", ToJsonEnum(target.kind), alloc);
    obj.AddMember("value", ToJsonString(target.value, alloc), alloc);
    obj.AddMember(
        "literal_suffix",
        ToJsonString(target.literal_suffix, alloc),
        alloc);
    obj.AddMember(
        "raw_string_delim",
        ToJsonString(target.raw_string_delim, alloc),
        alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const TemplateArgumentList& target,
    JsonAlloc&                  alloc);
[[nodiscard]] inline JsonValue ToJson(
    const TemplateArgument& target,
    JsonAlloc&              alloc);
[[nodiscard]] inline JsonValue ToJson(
    const Type& target,
    JsonAlloc&  alloc);
[[nodiscard]] inline JsonValue ToJson(
    const TypeModifier& target,
    JsonAlloc&          alloc);
[[nodiscard]] inline JsonValue ToJson(
    const PseudoExpr& target,
    JsonAlloc&        alloc);
[[nodiscard]] inline JsonValue ToJson(
    const PseudoExprList& target,
    JsonAlloc&            alloc);
[[nodiscard]] inline JsonValue ToJson(
    const Attribute& target,
    JsonAlloc&       alloc);
[[nodiscard]] inline JsonValue ToJson(
    const AttributeList& target,
    JsonAlloc&           alloc);
[[nodiscard]] inline JsonValue ToJson(
    const SimpleType& target,
    JsonAlloc&        alloc);
[[nodiscard]] inline JsonValue ToJson(
    const QualifiedName& target,
    JsonAlloc&           alloc);
[[nodiscard]] inline JsonValue ToJson(
    const UnqualifiedName& target,
    JsonAlloc&             alloc);
[[nodiscard]] inline JsonValue ToJson(
    const Decl& target,
    JsonAlloc&  alloc);

[[nodiscard]] inline JsonValue ToJson(
    const UnqualifiedName::Variant& target,
    JsonAlloc&                      alloc) {
    JsonValue obj(rapidjson::kObjectType);

    std::visit(
        Overload{
            [&](const std::string& name) {
                obj.AddMember("kind", "name", alloc);
                obj.AddMember("value", ToJsonString(name, alloc), alloc);
            },
            [&](const OverloadedOperator& op) {
                obj.AddMember("kind", "overloaded_operator", alloc);
                obj.AddMember(
                    "token", ToJsonString(op.token, alloc), alloc);
            },
            [&](const ConversionOperator& conv) {
                obj.AddMember("kind", "conversion_operator", alloc);
                obj.AddMember(
                    "target_type", ToJson(conv.target_type, alloc), alloc);
            },
            [&](const UserDefinedLiteral& udl) {
                obj.AddMember("kind", "user_defined_literal", alloc);
                obj.AddMember(
                    "suffix", ToJsonString(udl.suffix, alloc), alloc);
                obj.AddMember(
                    "space_before_suffix", udl.space_before_suffix, alloc);
            },
            [&](const DestructorName& dtor) {
                obj.AddMember("kind", "destructor", alloc);
                obj.AddMember(
                    "simple_type", ToJson(dtor.simple_type, alloc), alloc);
            },
            [&](const NewDeleteOperator& op) {
                obj.AddMember("kind", "new_delete_operator", alloc);
                obj.AddMember("op_kind", ToJsonEnum(op.kind), alloc);
            },
            [&](const UnspellableName& unsp) {
                obj.AddMember("kind", "unspellable_name", alloc);
                obj.AddMember(
                    "name", ToJsonString(unsp.name, alloc), alloc);
            },
        },
        target);

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const UnqualifiedName& target,
    JsonAlloc&             alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("var", ToJson(target.var, alloc), alloc);

    if (target.template_args) {
        obj.AddMember(
            "template_args", ToJson(*target.template_args, alloc), alloc);
    } else {
        obj.AddMember(
            "template_args", JsonValue(rapidjson::kNullType), alloc);
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const QualifiedName& target,
    JsonAlloc&           alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("force_global_scope", target.force_global_scope, alloc);
    obj.AddMember("flags", ToJsonEnum(target.flags), alloc);

    JsonValue parts(rapidjson::kArrayType);
    for (const auto& part : target.parts) {
        parts.PushBack(ToJson(part, alloc), alloc);
    }
    obj.AddMember("parts", std::move(parts), alloc);

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Attribute& target,
    JsonAlloc&       alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("style", ToJsonEnum(target.style), alloc);
    obj.AddMember("expr", ToJson(target.expr, alloc), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const AttributeList& target,
    JsonAlloc&           alloc) {
    JsonValue obj(rapidjson::kObjectType);
    JsonValue attrs(rapidjson::kArrayType);
    for (const auto& a : target.attrs) {
        attrs.PushBack(ToJson(a, alloc), alloc);
    }
    obj.AddMember("attrs", std::move(attrs), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const SimpleType& target,
    JsonAlloc&        alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("attrs", ToJson(target.attrs, alloc), alloc);
    obj.AddMember("prefix", ToJsonEnum(target.prefix), alloc);
    obj.AddMember("flags", ToJsonEnum(target.flags), alloc);
    obj.AddMember("quals", ToJsonEnum(target.quals), alloc);
    obj.AddMember("name", ToJson(target.name, alloc), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Pointer& target,
    JsonAlloc&     alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", "pointer", alloc);
    obj.AddMember("quals", ToJsonEnum(target.quals), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Reference& target,
    JsonAlloc&       alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", "reference", alloc);
    obj.AddMember("ref_kind", ToJsonEnum(target.kind), alloc);
    obj.AddMember("quals", ToJsonEnum(target.quals), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const MemberPointer& target,
    JsonAlloc&           alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", "member_pointer", alloc);
    obj.AddMember("base", ToJson(target.base, alloc), alloc);
    obj.AddMember("quals", ToJsonEnum(target.quals), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Array& target,
    JsonAlloc&   alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", "array", alloc);
    obj.AddMember("size", ToJson(target.size, alloc), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Function& target,
    JsonAlloc&      alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", "function", alloc);

    JsonValue params(rapidjson::kArrayType);
    for (const auto& p : target.params) {
        params.PushBack(ToJson(p, alloc), alloc);
    }
    obj.AddMember("params", std::move(params), alloc);

    obj.AddMember("c_style_variadic", target.c_style_variadic, alloc);
    obj.AddMember(
        "c_style_variadic_without_comma",
        target.c_style_variadic_without_comma,
        alloc);
    obj.AddMember(
        "c_style_void_params", target.c_style_void_params, alloc);
    obj.AddMember("cv_quals", ToJsonEnum(target.cv_quals), alloc);
    obj.AddMember("ref_qual", ToJsonEnum(target.ref_qual), alloc);
    obj.AddMember("noexcept", target.noexcept_, alloc);
    obj.AddMember(
        "uses_trailing_return_type",
        target.uses_trailing_return_type,
        alloc);

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const TypeModifier& target,
    JsonAlloc&          alloc) {
    JsonValue obj(rapidjson::kObjectType);
    std::visit(
        Overload{
            [&](const Pointer& p) { obj = ToJson(p, alloc); },
            [&](const Reference& r) { obj = ToJson(r, alloc); },
            [&](const MemberPointer& m) { obj = ToJson(m, alloc); },
            [&](const Array& a) { obj = ToJson(a, alloc); },
            [&](const Function& f) { obj = ToJson(f, alloc); },
        },
        target.var);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Type& target,
    JsonAlloc&  alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("simple_type", ToJson(target.simple_type, alloc), alloc);

    JsonValue modifiers(rapidjson::kArrayType);
    for (const auto& m : target.modifiers) {
        modifiers.PushBack(ToJson(m, alloc), alloc);
    }
    obj.AddMember("modifiers", std::move(modifiers), alloc);

    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const PseudoExprList& target,
    JsonAlloc&            alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("kind", ToJsonEnum(target.kind), alloc);

    JsonValue elems(rapidjson::kArrayType);
    for (const auto& e : target.elems) {
        elems.PushBack(ToJson(e, alloc), alloc);
    }
    obj.AddMember("elems", std::move(elems), alloc);

    obj.AddMember("has_trailing_comma", target.has_trailing_comma, alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const PseudoExpr& target,
    JsonAlloc&        alloc) {
    JsonValue obj(rapidjson::kObjectType);
    JsonValue tokens(rapidjson::kArrayType);

    for (const auto& token : target.tokens) {
        JsonValue token_obj(rapidjson::kObjectType);
        std::visit(
            [&](const auto& elem) {
                token_obj.AddMember("value", ToJson(elem, alloc), alloc);
            },
            token);
        tokens.PushBack(std::move(token_obj), alloc);
    }

    obj.AddMember("tokens", std::move(tokens), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const TemplateArgument& target,
    JsonAlloc&              alloc) {
    JsonValue obj(rapidjson::kObjectType);
    std::visit(
        Overload{
            [&](const Type& type) {
                obj.AddMember("kind", "type", alloc);
                obj.AddMember("value", ToJson(type, alloc), alloc);
            },
            [&](const PseudoExpr& expr) {
                obj.AddMember("kind", "expr", alloc);
                obj.AddMember("value", ToJson(expr, alloc), alloc);
            },
        },
        target.var);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const TemplateArgumentList& target,
    JsonAlloc&                  alloc) {
    JsonValue obj(rapidjson::kObjectType);
    JsonValue args(rapidjson::kArrayType);
    for (const auto& arg : target.args) {
        args.PushBack(ToJson(arg, alloc), alloc);
    }
    obj.AddMember("args", std::move(args), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToJson(
    const Decl& target,
    JsonAlloc&  alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("type", ToJson(target.type, alloc), alloc);
    obj.AddMember("name", ToJson(target.name, alloc), alloc);
    return obj;
}

template <typename T>
[[nodiscard]] inline JsonValue ToJson(
    const MaybeAmbiguous<T>& target,
    JsonAlloc&               alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember(
        "value", ToJson(static_cast<const T&>(target), alloc), alloc);

    JsonValue                alternatives(rapidjson::kArrayType);
    const MaybeAmbiguous<T>* cur = target.ambiguous_alternative.get();
    while (cur) {
        alternatives.PushBack(
            ToJson(static_cast<const T&>(*cur), alloc), alloc);
        cur = cur->ambiguous_alternative.get();
    }

    obj.AddMember(
        "ambiguous_alternatives", std::move(alternatives), alloc);
    return obj;
}
} // namespace cppdecl

#pragma once

#include <rapidjson/document.h>
#include <rapidjson/rapidjson.h>
#include <type_traits>
#include <algorithm>
#include <cctype>
#include <string>
#include <string_view>

#include <cppdecl/declarations/to_string.h>

#include "to_json.hpp"

namespace cppdecl {

namespace normalized_detail {

[[nodiscard]] inline JsonValue MakeIdentifier(
    std::string_view name,
    JsonAlloc&       alloc) {
    JsonValue identifier(rapidjson::kObjectType);
    identifier.AddMember("name", ToJsonString(name, alloc), alloc);
    identifier.AddMember("cvr", JsonValue(rapidjson::kArrayType), alloc);
    return identifier;
}

[[nodiscard]] inline JsonValue MakeName(
    std::string_view name,
    JsonAlloc&       alloc) {
    JsonValue obj(rapidjson::kObjectType);
    obj.AddMember("name", MakeIdentifier(name, alloc), alloc);
    obj.AddMember("params", JsonValue(rapidjson::kArrayType), alloc);
    return obj;
}

[[nodiscard]] inline JsonValue& NameIdentifier(JsonValue& name_obj) {
    return name_obj["name"];
}

[[nodiscard]] inline JsonValue& NameCvr(JsonValue& name_obj) {
    return NameIdentifier(name_obj)["cvr"];
}

[[nodiscard]] inline JsonValue& NameParams(JsonValue& name_obj) {
    return name_obj["params"];
}

inline void AddQualifier(
    JsonValue&       name_obj,
    std::string_view tag,
    JsonAlloc&       alloc) {
    JsonValue q(rapidjson::kObjectType);
    q.AddMember("t", ToJsonString(tag, alloc), alloc);
    NameCvr(name_obj).PushBack(std::move(q), alloc);
}

inline void AddCvQualifiers(
    JsonValue&   name_obj,
    CvQualifiers quals,
    JsonAlloc&   alloc) {
    if (bool(quals & CvQualifiers::const_)) {
        AddQualifier(name_obj, "const", alloc);
    }
    if (bool(quals & CvQualifiers::volatile_)) {
        AddQualifier(name_obj, "volatile", alloc);
    }
    if (bool(quals & CvQualifiers::restrict_)) {
        AddQualifier(name_obj, "restrict", alloc);
    }
    if (bool(quals & CvQualifiers::msvc_ptr32)) {
        AddQualifier(name_obj, "ptr32", alloc);
    }
    if (bool(quals & CvQualifiers::msvc_ptr64)) {
        AddQualifier(name_obj, "ptr64", alloc);
    }
    if (bool(quals & CvQualifiers::msvc_unaligned)) {
        AddQualifier(name_obj, "unaligned", alloc);
    }
}

inline void AddRefQualifier(
    JsonValue&   name_obj,
    RefQualifier ref,
    JsonAlloc&   alloc) {
    switch (ref) {
        case RefQualifier::none: break;
        case RefQualifier::lvalue:
            AddQualifier(name_obj, "lv", alloc);
            break;
        case RefQualifier::rvalue:
            AddQualifier(name_obj, "rv", alloc);
            break;
    }
}

inline void AddSimpleTypePrefix(
    JsonValue&       name_obj,
    SimpleTypePrefix prefix,
    JsonAlloc&       alloc) {
    switch (prefix) {
        case SimpleTypePrefix::none: break;
        case SimpleTypePrefix::struct_:
            AddQualifier(name_obj, "struct", alloc);
            break;
        case SimpleTypePrefix::class_:
            AddQualifier(name_obj, "class", alloc);
            break;
        case SimpleTypePrefix::union_:
            AddQualifier(name_obj, "union", alloc);
            break;
        case SimpleTypePrefix::enum_:
            AddQualifier(name_obj, "enum", alloc);
            break;
        case SimpleTypePrefix::typename_:
            AddQualifier(name_obj, "typename", alloc);
            break;
    }
}

inline void AddSimpleTypeFlags(
    JsonValue&      name_obj,
    SimpleTypeFlags flags,
    JsonAlloc&      alloc) {
    if (bool(flags & SimpleTypeFlags::unsigned_)) {
        AddQualifier(name_obj, "unsigned", alloc);
    }
    if (bool(flags & SimpleTypeFlags::explicitly_signed)) {
        AddQualifier(name_obj, "signed", alloc);
    }
    if (bool(flags & SimpleTypeFlags::implied_int)) {
        AddQualifier(name_obj, "implied_int", alloc);
    }
    if (bool(flags & SimpleTypeFlags::c_complex)) {
        AddQualifier(name_obj, "c_complex", alloc);
    }
    if (bool(flags & SimpleTypeFlags::c_imaginary)) {
        AddQualifier(name_obj, "c_imaginary", alloc);
    }
    if (bool(flags & SimpleTypeFlags::c_implied_double)) {
        AddQualifier(name_obj, "c_implied_double", alloc);
    }
}

inline void AddQualifiedNameFlags(
    JsonValue&         name_obj,
    QualifiedNameFlags flags,
    JsonAlloc&         alloc) {
    if (bool(flags & QualifiedNameFlags::redundant_int)) {
        AddQualifier(name_obj, "redundant_int", alloc);
    }
}

[[nodiscard]] inline JsonValue ToName(
    const Type& target,
    JsonAlloc&  alloc);
[[nodiscard]] inline JsonValue ToName(
    const Decl& target,
    JsonAlloc&  alloc);
[[nodiscard]] inline JsonValue ToName(
    const TemplateArgument& target,
    JsonAlloc&              alloc);
[[nodiscard]] inline JsonValue ToName(
    const PseudoExpr& target,
    JsonAlloc&        alloc);
[[nodiscard]] inline JsonValue ToName(
    const PseudoExprList& target,
    JsonAlloc&            alloc);

[[nodiscard]] inline JsonValue ToName(
    const NumericLiteral& target,
    JsonAlloc&            alloc) {
    JsonValue obj = MakeName("<number>", alloc);
    AddQualifier(obj, "number", alloc);

    std::visit(
        Overload{
            [&](const NumericLiteral::Integer& i) {
                NameIdentifier(obj)["name"] = ToJsonString(i.value, alloc);
                AddQualifier(obj, "integer", alloc);
                switch (i.base) {
                    case NumericLiteral::Integer::Base::decimal:
                        AddQualifier(obj, "base10", alloc);
                        break;
                    case NumericLiteral::Integer::Base::binary:
                        AddQualifier(obj, "base2", alloc);
                        break;
                    case NumericLiteral::Integer::Base::octal:
                        AddQualifier(obj, "base8", alloc);
                        break;
                    case NumericLiteral::Integer::Base::hex:
                        AddQualifier(obj, "base16", alloc);
                        break;
                }
            },
            [&](const NumericLiteral::FloatingPoint& f) {
                std::string text = f.value_int;
                if (f.value_frac) {
                    text += ".";
                    text += *f.value_frac;
                }
                if (!f.value_exp.empty()) {
                    text += "e";
                    text += f.value_exp;
                }
                NameIdentifier(obj)["name"] = ToJsonString(text, alloc);
                AddQualifier(obj, "floating", alloc);
                switch (f.base) {
                    case NumericLiteral::FloatingPoint::Base::decimal:
                        AddQualifier(obj, "base10", alloc);
                        break;
                    case NumericLiteral::FloatingPoint::Base::hex:
                        AddQualifier(obj, "base16", alloc);
                        break;
                }
            },
        },
        target.var);

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const StringOrCharLiteral& target,
    JsonAlloc&                 alloc) {
    JsonValue obj = MakeName(target.value, alloc);
    AddQualifier(obj, "literal", alloc);

    switch (target.kind) {
        case StringOrCharLiteral::Kind::character:
            AddQualifier(obj, "char", alloc);
            break;
        case StringOrCharLiteral::Kind::string:
            AddQualifier(obj, "string", alloc);
            break;
        case StringOrCharLiteral::Kind::raw_string:
            AddQualifier(obj, "raw_string", alloc);
            break;
    }

    switch (target.type) {
        case StringOrCharLiteral::Type::normal:
            AddQualifier(obj, "normal", alloc);
            break;
        case StringOrCharLiteral::Type::wide:
            AddQualifier(obj, "wide", alloc);
            break;
        case StringOrCharLiteral::Type::u8:
            AddQualifier(obj, "u8", alloc);
            break;
        case StringOrCharLiteral::Type::u16:
            AddQualifier(obj, "u16", alloc);
            break;
        case StringOrCharLiteral::Type::u32:
            AddQualifier(obj, "u32", alloc);
            break;
    }

    if (!target.literal_suffix.empty()) {
        JsonValue suffix = MakeName(target.literal_suffix, alloc);
        AddQualifier(suffix, "literal_suffix", alloc);
        NameParams(obj).PushBack(std::move(suffix), alloc);
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const PunctuationToken& target,
    JsonAlloc&              alloc) {
    JsonValue obj = MakeName(target.value, alloc);
    AddQualifier(obj, "punct", alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const TemplateArgument& target,
    JsonAlloc&              alloc) {
    JsonValue obj = std::visit(
        Overload{
            [&](const Type& type) { return ToName(type, alloc); },
            [&](const PseudoExpr& expr) { return ToName(expr, alloc); },
        },
        target.var);

    AddQualifier(obj, "template_arg", alloc);
    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const TemplateArgumentList& target,
    JsonAlloc&                  alloc) {
    JsonValue obj = MakeName("<template_args>", alloc);
    AddQualifier(obj, "template_args", alloc);

    for (const auto& arg : target.args) {
        NameParams(obj).PushBack(ToName(arg, alloc), alloc);
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const PseudoExprList& target,
    JsonAlloc&            alloc) {
    JsonValue obj = MakeName("<list>", alloc);
    AddQualifier(obj, "list", alloc);

    switch (target.kind) {
        case PseudoExprList::Kind::parentheses:
            AddQualifier(obj, "paren", alloc);
            break;
        case PseudoExprList::Kind::curly:
            AddQualifier(obj, "curly", alloc);
            break;
        case PseudoExprList::Kind::square:
            AddQualifier(obj, "square", alloc);
            break;
    }

    if (target.has_trailing_comma) {
        AddQualifier(obj, "trailing_comma", alloc);
    }

    for (const auto& elem : target.elems) {
        NameParams(obj).PushBack(ToName(elem, alloc), alloc);
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const QualifiedName& target,
    JsonAlloc&           alloc);

[[nodiscard]] inline JsonValue ToName(
    const PseudoExpr& target,
    JsonAlloc&        alloc) {
    JsonValue obj = MakeName("<expr>", alloc);
    AddQualifier(obj, "expr", alloc);

    for (const auto& token : target.tokens) {
        JsonValue elem = std::visit(
            Overload{
                [&](const SimpleType& t) {
                    JsonValue n = MakeName("<simple_type>", alloc);
                    AddQualifier(n, "simple_type", alloc);
                    if (!t.name.parts.empty()) {
                        n = ToName(t.name, alloc);
                        AddCvQualifiers(n, t.quals, alloc);
                        AddSimpleTypeFlags(n, t.flags, alloc);
                        AddSimpleTypePrefix(n, t.prefix, alloc);
                    }
                    return n;
                },
                [&](const PunctuationToken& p) {
                    return ToName(p, alloc);
                },
                [&](const NumericLiteral& n) { return ToName(n, alloc); },
                [&](const StringOrCharLiteral& s) {
                    return ToName(s, alloc);
                },
                [&](const PseudoExprList& l) { return ToName(l, alloc); },
                [&](const TemplateArgumentList& a) {
                    return ToName(a, alloc);
                },
            },
            token);

        NameParams(obj).PushBack(std::move(elem), alloc);
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const UnqualifiedName::Variant& target,
    JsonAlloc&                      alloc) {
    return std::visit(
        Overload{
            [&](const std::string& name) { return MakeName(name, alloc); },
            [&](const OverloadedOperator& op) {
                JsonValue obj = MakeName("operator", alloc);
                AddQualifier(obj, "overloaded_operator", alloc);

                JsonValue tok = MakeName(op.token, alloc);
                AddQualifier(tok, "operator_token", alloc);
                NameParams(obj).PushBack(std::move(tok), alloc);

                return obj;
            },
            [&](const ConversionOperator& conv) {
                JsonValue obj = MakeName("operator", alloc);
                AddQualifier(obj, "conversion_operator", alloc);
                NameParams(obj).PushBack(
                    ToName(conv.target_type, alloc), alloc);
                return obj;
            },
            [&](const UserDefinedLiteral& udl) {
                JsonValue obj = MakeName("operator\"\"", alloc);
                AddQualifier(obj, "udl", alloc);

                JsonValue suffix = MakeName(udl.suffix, alloc);
                AddQualifier(suffix, "udl_suffix", alloc);
                NameParams(obj).PushBack(std::move(suffix), alloc);

                return obj;
            },
            [&](const DestructorName& dtor) {
                JsonValue obj = MakeName("~", alloc);
                AddQualifier(obj, "destructor", alloc);

                JsonValue t = MakeName("<destructor_target>", alloc);
                if (!dtor.simple_type.name.parts.empty()) {
                    t = ToName(dtor.simple_type.name, alloc);
                    AddCvQualifiers(t, dtor.simple_type.quals, alloc);
                    AddSimpleTypeFlags(t, dtor.simple_type.flags, alloc);
                    AddSimpleTypePrefix(t, dtor.simple_type.prefix, alloc);
                }
                NameParams(obj).PushBack(std::move(t), alloc);

                return obj;
            },
            [&](const NewDeleteOperator& op) {
                JsonValue obj = MakeName("operator", alloc);
                AddQualifier(obj, "new_delete_operator", alloc);
                switch (op.kind) {
                    case NewDeleteOperator::Kind::new_:
                        AddQualifier(obj, "new", alloc);
                        break;
                    case NewDeleteOperator::Kind::new_array:
                        AddQualifier(obj, "new_array", alloc);
                        break;
                    case NewDeleteOperator::Kind::delete_:
                        AddQualifier(obj, "delete", alloc);
                        break;
                    case NewDeleteOperator::Kind::delete_array:
                        AddQualifier(obj, "delete_array", alloc);
                        break;
                }
                return obj;
            },
            [&](const UnspellableName& unsp) {
                JsonValue obj = MakeName(unsp.name, alloc);
                AddQualifier(obj, "unspellable", alloc);
                return obj;
            },
        },
        target);
}

[[nodiscard]] inline JsonValue ToName(
    const UnqualifiedName& target,
    JsonAlloc&             alloc) {
    JsonValue obj = ToName(target.var, alloc);

    if (target.template_args) {
        for (const auto& arg : target.template_args->args) {
            NameParams(obj).PushBack(ToName(arg, alloc), alloc);
        }
    }

    return obj;
}

[[nodiscard]] inline JsonValue ToName(
    const QualifiedName& target,
    JsonAlloc&           alloc) {
    if (target.parts.empty()) {
        JsonValue empty = MakeName("<empty>", alloc);
        if (target.force_global_scope) {
            AddQualifier(empty, "global", alloc);
        }
        AddQualifiedNameFlags(empty, target.flags, alloc);
        return empty;
    }

    // Keep unqualified names unchanged.
    if (target.parts.size() == 1 && !target.force_global_scope) {
        JsonValue single = ToName(target.parts.front(), alloc);
        AddQualifiedNameFlags(single, target.flags, alloc);
        return single;
    }

    // Flatten qualified names to match `parts`-like structure.
    JsonValue obj = MakeName("<qualified>", alloc);
    if (target.force_global_scope) { AddQualifier(obj, "global", alloc); }
    AddQualifiedNameFlags(obj, target.flags, alloc);

    for (const auto& part : target.parts) {
        NameParams(obj).PushBack(ToName(part, alloc), alloc);
    }

    return obj;
}

inline void ApplyTypeToName(
    JsonValue&  name_obj,
    const Type& type,
    JsonAlloc&  alloc) {
    AddCvQualifiers(name_obj, type.simple_type.quals, alloc);
    AddSimpleTypeFlags(name_obj, type.simple_type.flags, alloc);
    AddSimpleTypePrefix(name_obj, type.simple_type.prefix, alloc);

    for (const auto& mod : type.modifiers) {
        std::visit(
            Overload{
                [&](const Pointer& p) {
                    AddQualifier(name_obj, "ptr", alloc);
                    AddCvQualifiers(name_obj, p.quals, alloc);
                },
                [&](const Reference& r) {
                    AddRefQualifier(name_obj, r.kind, alloc);
                    AddCvQualifiers(name_obj, r.quals, alloc);
                },
                [&](const MemberPointer& m) {
                    AddQualifier(name_obj, "member_ptr", alloc);
                    AddCvQualifiers(name_obj, m.quals, alloc);
                    NameParams(name_obj).PushBack(
                        ToName(m.base, alloc), alloc);
                },
                [&](const Array& a) {
                    AddQualifier(name_obj, "array", alloc);
                    if (!a.size.IsEmpty()) {
                        JsonValue size = ToName(a.size, alloc);
                        AddQualifier(size, "array_size", alloc);
                        NameParams(name_obj).PushBack(
                            std::move(size), alloc);
                    }
                },
                [&](const Function& f) {
                    AddQualifier(name_obj, "fn", alloc);
                    AddCvQualifiers(name_obj, f.cv_quals, alloc);
                    AddRefQualifier(name_obj, f.ref_qual, alloc);

                    if (f.noexcept_) {
                        AddQualifier(name_obj, "noexcept", alloc);
                    }
                    if (f.uses_trailing_return_type) {
                        AddQualifier(
                            name_obj, "trailing_return_type", alloc);
                    }
                    if (f.c_style_void_params) {
                        AddQualifier(name_obj, "c_void_params", alloc);
                    }
                    if (f.c_style_variadic) {
                        AddQualifier(name_obj, "c_variadic", alloc);
                    }
                    if (f.c_style_variadic_without_comma) {
                        AddQualifier(
                            name_obj, "c_variadic_no_comma", alloc);
                    }

                    for (const auto& p : f.params) {
                        NameParams(name_obj).PushBack(
                            ToName(static_cast<const Decl&>(p), alloc),
                            alloc);
                    }
                },
            },
            mod.var);
    }
}

[[nodiscard]] inline JsonValue ToName(
    const Type& target,
    JsonAlloc&  alloc) {
    JsonValue base = target.simple_type.name.parts.empty()
                       ? MakeName("<type>", alloc)
                       : ToName(target.simple_type.name, alloc);

    ApplyTypeToName(base, target, alloc);
    return base;
}

[[nodiscard]] inline JsonValue ToName(
    const Decl& target,
    JsonAlloc&  alloc) {
    JsonValue name = target.name.parts.empty()
                       ? (target.type.simple_type.name.parts.empty()
                              ? MakeName("<decl>", alloc)
                              : ToName(
                                    target.type.simple_type.name, alloc))
                       : ToName(target.name, alloc);

    ApplyTypeToName(name, target.type, alloc);
    return name;
}

template <typename T>
[[nodiscard]] inline JsonValue ToRoot(
    const MaybeAmbiguous<T>& target,
    JsonAlloc&               alloc) {
    JsonValue root(rapidjson::kObjectType);
    root.AddMember(
        "names", ToName(static_cast<const T&>(target), alloc), alloc);

    JsonValue                alts(rapidjson::kArrayType);
    const MaybeAmbiguous<T>* cur = target.ambiguous_alternative.get();
    while (cur) {
        alts.PushBack(ToName(static_cast<const T&>(*cur), alloc), alloc);
        cur = cur->ambiguous_alternative.get();
    }

    root.AddMember("ambiguous_alternatives", std::move(alts), alloc);
    return root;
}

} // namespace normalized_detail

[[nodiscard]] inline JsonValue ToJsonNormalized(
    const QualifiedName& target,
    JsonAlloc&           alloc) {
    JsonValue root(rapidjson::kObjectType);
    root.AddMember(
        "names", normalized_detail::ToName(target, alloc), alloc);
    return root;
}

[[nodiscard]] inline JsonValue ToJsonNormalized(
    const Type& target,
    JsonAlloc&  alloc) {
    JsonValue root(rapidjson::kObjectType);
    root.AddMember(
        "names", normalized_detail::ToName(target, alloc), alloc);
    return root;
}

[[nodiscard]] inline JsonValue ToJsonNormalized(
    const Decl& target,
    JsonAlloc&  alloc) {
    JsonValue root(rapidjson::kObjectType);
    root.AddMember(
        "names", normalized_detail::ToName(target, alloc), alloc);
    return root;
}

template <typename T>
[[nodiscard]] inline JsonValue ToJsonNormalized(
    const MaybeAmbiguous<T>& target,
    JsonAlloc&               alloc) {
    return normalized_detail::ToRoot(target, alloc);
}

} // namespace cppdecl

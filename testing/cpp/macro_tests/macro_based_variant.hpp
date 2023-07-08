#define PARENS ()

#define EXPAND(...) EXPAND4(EXPAND4(EXPAND4(EXPAND4(__VA_ARGS__))))
#define EXPAND4(...) EXPAND3(EXPAND3(EXPAND3(EXPAND3(__VA_ARGS__))))
#define EXPAND3(...) EXPAND2(EXPAND2(EXPAND2(EXPAND2(__VA_ARGS__))))
#define EXPAND2(...) EXPAND1(EXPAND1(EXPAND1(EXPAND1(__VA_ARGS__))))
#define EXPAND1(...) __VA_ARGS__

#define __unpack_pars(...) __VA_ARGS__
#define __macro_aux(macro, pass, ...)                                     \
    macro(__unpack_pars pass, __VA_ARGS__)

#define __FOR_EACH_HELPER(macro, pass, a1, ...)                           \
    __macro_aux(macro, pass, a1)                                          \
        __VA_OPT__(__FOR_EACH_AGAIN PARENS(macro, pass, __VA_ARGS__))

#define __FOR_EACH_AGAIN() __FOR_EACH_HELPER

#define FOR_EACH_CALL_WITH_PASS(macro, pass, ...)                         \
    __VA_OPT__(EXPAND(__FOR_EACH_HELPER(macro, pass, __VA_ARGS__)))

#define __it(...) print(__VA_ARGS__);

#define DECL_DESCRIBED_ENUM(Name, ...)                                    \
    enum class Name                                                       \
    {                                                                     \
        __VA_ARGS__                                                       \
    };                                                                    \
    BOOST_DESCRIBE_NESTED_ENUM(Name, __VA_ARGS__);

#define __SUB_VARIANT_UNION_GETTER(fieldName, Type)                       \
    Type&       get##Type() { std::get<Type>(fieldName); }                \
    Type const& get##Type() const { std::get<Type>(fieldName); }

#define __SUB_VARIANT_UNION_KIND_LAMBDA(EnumName, Type)                   \
    [](Type const&) { return EnumName::Type; },

#define __SUB_VARIANT_UNION_DEFINE_UNION(_, Type) Type Type##_field;

#define __PACK_IDX0(it1, ...) it1
#define __PACK_IDX1(it1, it2, ...) it2
#define __PACK_IDX2(it1, it2, it3, ...) it3

#define __SUB_VARIANT_UNION_DEFINE_METHODS(Pass, Type)                    \
    __PACK_IDX1(Pass)                                                     \
    (Type const& arg)                                                     \
        : kindValue(__PACK_IDX0(Pass)::Type), Type##_field(arg) {}        \
                                                                          \
    Type& get##Type() {                                                   \
        Q_ASSERT(kindValue == __PACK_IDX0(Pass)::Type);                   \
        return Type##_field;                                              \
    }

#define __SUB_VARIANT_UNION_DEFINE_VISIT(EnumName, Type)                  \
    case __PACK_IDX0(EnumName)::Type:                                     \
        return cb(                                                        \
            Type##_field, std::forward<Args>(__PACK_IDX1(EnumName))...);

#define SUB_VARIANTS_UNION(                                               \
    EnumName, VariantName, fieldName, kindGetterName, ...)                \
    DECL_DESCRIBED_ENUM(EnumName, __VA_ARGS__)                            \
    struct VariantName {                                                  \
        EnumName kindValue;                                               \
        union {                                                           \
            FOR_EACH_CALL_WITH_PASS(                                      \
                __SUB_VARIANT_UNION_DEFINE_UNION,                         \
                (fieldName),                                              \
                __VA_ARGS__)                                              \
        };                                                                \
                                                                          \
        FOR_EACH_CALL_WITH_PASS(                                          \
            __SUB_VARIANT_UNION_DEFINE_METHODS,                           \
            (EnumName, VariantName),                                      \
            __VA_ARGS__)                                                  \
                                                                          \
        template <typename Func, typename... Args>                        \
        auto visit(Func const& cb, Args&&... args) {                      \
            switch (kindValue) {                                          \
                FOR_EACH_CALL_WITH_PASS(                                  \
                    __SUB_VARIANT_UNION_DEFINE_VISIT,                     \
                    (EnumName, args),                                     \
                    __VA_ARGS__)                                          \
            }                                                             \
        }                                                                 \
    }

int main() {
    SUB_VARIANTS_UNION(Base, Data, field, getKind, Weekday, Range);
}

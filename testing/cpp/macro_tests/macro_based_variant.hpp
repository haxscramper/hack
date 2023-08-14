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
    };

#define Q_ASSERT(X)
#define CONCAT(a, b) CONCAT_INNER(a, b)
#define CONCAT_INNER(a, b) a##b


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
    Type& get##Type() { return Type##_field; }

#define __SUB_VARIANT_UNION_DEFINE_VISIT(EnumName, Type)                  \
    case __PACK_IDX0(EnumName)::Type:                                     \
        return cb(Type##_field);


#define __SUB_VARIANT_UNION_DEFINE_FIELD_COPY(EnumName, Type)             \
    case __PACK_IDX0(EnumName)::Type:                                     \
        this->Type##_field = other.Type##_field;                          \
        break;


#define __SUB_VARIANT_UNION_DEFINE_FIELD_DESTROY(EnumName, Type)          \
    case __PACK_IDX0(EnumName)::Type:                                     \
        Type##_field.~Type();                                             \
        break;

#define __TAIL(Head, ...) __VA_ARGS__
#define __HEAD(Head, ...) Head

#define SUB_VARIANTS_UNION(                                               \
    EnumName, VariantName, fieldName, kindGetterName, ...)                \
    DECL_DESCRIBED_ENUM(EnumName, __VA_ARGS__)                            \
    struct VariantName {                                                  \
        EnumName kindValue;                                               \
        union {                                                           \
            __HEAD(__VA_ARGS__)                                           \
            CONCAT(__HEAD(__VA_ARGS__), _field) = __HEAD(__VA_ARGS__)();  \
            FOR_EACH_CALL_WITH_PASS(                                      \
                __SUB_VARIANT_UNION_DEFINE_UNION,                         \
                (fieldName),                                              \
                __TAIL(__VA_ARGS__))                                      \
        };                                                                \
                                                                          \
        FOR_EACH_CALL_WITH_PASS(                                          \
            __SUB_VARIANT_UNION_DEFINE_METHODS,                           \
            (EnumName, VariantName),                                      \
            __VA_ARGS__)                                                  \
                                                                          \
        VariantName()                                                     \
            : kindValue(EnumName::__PACK_IDX0(__VA_ARGS__))               \
            , CONCAT(__PACK_IDX0(__VA_ARGS__), _field)(                   \
                  __PACK_IDX0(__VA_ARGS__)()) {}                          \
                                                                          \
        template <typename Func>                                          \
        auto visit(Func const& cb) {                                      \
            switch (kindValue) {                                          \
                FOR_EACH_CALL_WITH_PASS(                                  \
                    __SUB_VARIANT_UNION_DEFINE_VISIT,                     \
                    (EnumName),                                           \
                    __VA_ARGS__)                                          \
            }                                                             \
        }                                                                 \
        ~VariantName() {                                                  \
            switch (kindValue) {                                          \
                FOR_EACH_CALL_WITH_PASS(                                  \
                    __SUB_VARIANT_UNION_DEFINE_FIELD_DESTROY,             \
                    (EnumName),                                           \
                    __VA_ARGS__)                                          \
            }                                                             \
        }                                                                 \
    }

struct Weekday {};
struct Range {};

SUB_VARIANTS_UNION(Base, Data, field, getKind, Weekday, Range);


int main() { Data data; }

template <typename... Args>
void print(Args... args) {}


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


#define __SUB_VARIANT_GETTER(fieldName, Type)                             \
    Type&       get##Type() { std::get<Type>(fieldName); }                \
    Type const& get##Type() const { std::get<Type>(fieldName); }

#define __SUB_VARIANT_KIND_LAMBDA(EnumName, Type)                         \
    [](Type const&) { return EnumName::Type; },

#define SUB_VARIANTS(EnumName, VariantName, fieldName, ...)               \
    DECL_DESCRIBED_ENUM(EnumName, __VA_ARGS__)                            \
    FOR_EACH_CALL_WITH_PASS(                                              \
        __SUB_VARIANT_GETTER, (fieldName), __VA_ARGS__)                   \
    using VariantName = std::variant<__VA_ARGS__>;                        \
    VariantName fieldName;                                                \
                                                                          \
    EnumName getKind() const {                                            \
        std::visit(                                                       \
            overloaded{FOR_EACH_CALL_WITH_PASS(                           \
                __SUB_VARIANT_KIND_LAMBDA, (EnumName), __VA_ARGS__)},     \
            fieldName);                                                   \
    }

int main() {
    FOR_EACH_CALL_WITH_PASS(__it, (res, Static, name), 1, 2, 3, 3, 4, 5);
    SUB_VARIANTS(Base, Data, field, Weekday, Range);
}

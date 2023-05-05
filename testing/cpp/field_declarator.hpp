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

#define BOOST_DESCRIBE_CLASS(a, b, c, d, e) (a, b, c, d, e);


#define __define_field_aux(first, second, uppercase, third)               \
    __unpack_pars first        second = third;                            \
    __unpack_pars first const& get##uppercase() const { return second; }  \
    __unpack_pars first&       get##uppercase() { return second; }        \
    void set##uppercase(__unpack_pars first const& value) {               \
        second = value;                                                   \
    };                                                                    \
    ;

#define __define_field(arg) __define_field_aux arg
#define __per_field(class_bases_bases, field) __define_field(field)

#define __per_field1_aux(a, fieldName, c, d) fieldName
#define __per_field1(_, arg) , __per_field1_aux arg
#define __drop_leading_comma(first, ...) __VA_ARGS__

#define EMPTY()

#define DECL_FIELDS(classname, bases, ...)                                \
    FOR_EACH_CALL_WITH_PASS(__per_field, (classname, bases), __VA_ARGS__) \
    BOOST_DESCRIBE_CLASS(                                                 \
        classname,                                                        \
        bases,                                                            \
        (__drop_leading_comma EMPTY()(EXPAND(                             \
            FOR_EACH_CALL_WITH_PASS(__per_field1, (), __VA_ARGS__)))),    \
        (),                                                               \
        ());


int main() {
    DECL_FIELDS(
        Class,
        (Base1, Base2, Base3),
        ((Map<int, float>), mapings, Mappings, {}), //
        ((Map<char, string>), otherf, Otherf, {}));
}

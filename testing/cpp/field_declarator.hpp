template <typename... Args>
struct Map {};
struct string {};
template <typename... Args>
struct Vec {};

#define PARENS ()

#define EXPAND(...) EXPAND4(EXPAND4(EXPAND4(EXPAND4(__VA_ARGS__))))
#define EXPAND4(...) EXPAND3(EXPAND3(EXPAND3(EXPAND3(__VA_ARGS__))))
#define EXPAND3(...) EXPAND2(EXPAND2(EXPAND2(EXPAND2(__VA_ARGS__))))
#define EXPAND2(...) EXPAND1(EXPAND1(EXPAND1(EXPAND1(__VA_ARGS__))))
#define EXPAND1(...) __VA_ARGS__

#define __unpack_pars(...) __VA_ARGS__
#define __macro_aux(macro, pass, ...)                                     \
    macro(__unpack_pars pass, __VA_ARGS__)

template <typename... Args>
struct Map {};
struct string {};
template <typename... Args>
struct Vec {};

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

#define BOOST_DESCRIBE_CLASS(a, b, c, d, e) /* (a, b, c, d, e); */

#define __define_getters_aux(first, second, uppercase, third)             \
    __unpack_pars first const& get##uppercase() const { return second; }  \
    __unpack_pars first&       get##uppercase() { return second; }        \
    void set##uppercase(__unpack_pars first const& value) {               \
        second = value;                                                   \
    };                                                                    \
    ;

#define __define_field_aux(first, second, uppercase, third)               \
    __unpack_pars first second = third;

#define __per_field(class_bases_bases, field) __define_field(field)

#define __get_field_name_aux(a, fieldName, c, d) fieldName
#define __get_field_name(_, arg) , __get_field_name_aux arg
#define __drop_leading_comma(first, ...) __VA_ARGS__

#define __define_field(arg)                                               \
    __define_field_aux arg // __define_getters_aux arg
#define __define_field_only(_, arg) __define_field_aux arg

#define __pass_args_field_aux(_1, fieldname, _2, _3)                      \
    fieldname(args.fieldname),
#define __pass_args_field(_, arg) __pass_args_field_aux arg

#define EMPTY()

#define __extra_args_fields Vec<int> subnodes = {};
#define __extra_args_pass Org(args.subnodes)

#define DECL_FIELDS(classname, bases, ...)                                \
    FOR_EACH_CALL_WITH_PASS(__per_field, (classname, bases), __VA_ARGS__) \
    struct Args {                                                         \
        FOR_EACH_CALL_WITH_PASS(                                          \
            __define_field_only,                                          \
            (classname, bases),                                           \
            __VA_ARGS__)                                                  \
        __extra_args_fields                                               \
    };                                                                    \
                                                                          \
    inline classname(Args const& args)                                    \
        : FOR_EACH_CALL_WITH_PASS(__pass_args_field, (), __VA_ARGS__)     \
            __extra_args_pass /**/ {};                                    \
                                                                          \
    BOOST_DESCRIBE_CLASS(                                                 \
        classname,                                                        \
        bases,                                                            \
        (__drop_leading_comma EMPTY()(EXPAND(FOR_EACH_CALL_WITH_PASS(     \
            __get_field_name, (), __VA_ARGS__)))),                        \
        (),                                                               \
        ());

struct Org {
    Vec<int> subnodes;
    Org(Vec<int> const&) {}
};

struct Subtree : public Org {
    DECL_FIELDS(
        Subtree,
        (Org),                                      //
        ((Map<int, float>), mapings, Mappings, {}), //
        ((Map<char, string>), otherf, Otherf, {}));
};


#define __extra_args_pass Org()
#define __extra_args_fields

struct Leaf : public Org {
    DECL_FIELDS(
        Subtree,
        (Org), //
        ((string), name, Name, {}));
};

#define __extra_args_fields string subnodes = {};
#define __extra_args_pass Org()


struct Word : public Leaf {
    DECL_FIELDS(Word, (Leaf));
};

int main() {}

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


#define __FOR_EACH_HELPER(macro, infix, pass, a1, ...)                    \
    __macro_aux(macro, pass, a1) __VA_OPT__(infix) __VA_OPT__(            \
        __FOR_EACH_AGAIN PARENS(macro, infix, pass, __VA_ARGS__))

#define __FOR_EACH_AGAIN() __FOR_EACH_HELPER

#define FOR_EACH_CALL_WITH_PASS(macro, infix, pass, ...)                  \
    __VA_OPT__(EXPAND(__FOR_EACH_HELPER(macro, infix, pass, __VA_ARGS__)))

#define FOR_EACH_CALL_WITH_WRAP(macro, pass, names)                       \
    FOR_EACH_CALL_WITH_PASS(macro, , pass, __unpack_pars names)

#define double_nested(macro, pass, names)                                 \
    FOR_EACH_CALL_WITH_PASS(macro, , pass, __unpack_pars names)

int main() {
#define __it(...) print(__VA_ARGS__);
    FOR_EACH_CALL_WITH_PASS(
        __it, , (res, Static, name), 1, 2, 4, 4, 4, 4, 4);
    FOR_EACH_CALL_WITH_WRAP(__it, (res, Static, name), (1, 2, 3, 3, 4, 5));

#define quux(res, type, fields)                                           \
    FOR_EACH_CALL_WITH_PASS(__field, (res, type), __unpack_pars fields)

#define quas(res, type) XXXX(res, type)

#define __thing(res, args) quas(res, __unpack_pars args);

    double_nested(
        __thing,
        (res),
        ((sem::Link::Id, (text, asd)),
         (sem::Link::Footnote, (target, sdd)),
         (sem::Link::Raw, (text, eee))));
}

#include <optional>

#define if_let_copy(__val, __opt)                                         \
    {                                                                     \
        auto __val     = typename std::decay_t<decltype(                  \
            __opt)>::value_type{};                                    \
        auto __opt_res = __opt;                                           \
        if (__opt_res.has_value()) {                                      \
            __val = __opt_res.value();                                    \
        }                                                                 \
                                                                          \
        if (__opt_res.has_value())

#define if_let(__val, __opt)                                              \
    {                                                                     \
        auto __val = typename std::decay_t<decltype(                      \
            __opt)>::value_type{};                                        \
        if (__opt.has_value()) {                                          \
            __val = __opt.value();                                        \
        }                                                                 \
                                                                          \
        if (__opt.has_value())

#define ifl_end }

std::optional<char> get_option() {
    return std::optional<char>('3');
}

int main() {
    std::optional<int> opt;

    if_let_copy(tmp, opt){ifl_end}

    if_let(tmp, get_option()) {
        ifl_end
    }
}
#include <optional>

/// Create the copy of optional value and get it's value
#define if_let_copy(__val, __opt)                                         \
    {                                                                     \
        auto __val     = typename std::decay_t<decltype(                  \
            __opt)>::value_type{};                                    \
        auto __opt_res = __opt;                                           \
        if (__opt_res.has_value()) {                                      \
            __val = __opt_res.value();                                    \
        }                                                                 \
                                                                          \
        if (__opt_res.has_value())

/// Take the reference to the optional value
#define if_let(__val, __opt)                                              \
    {                                                                     \
        auto __val = typename std::decay_t<decltype(                      \
            __opt)>::value_type{};                                        \
        if (__opt.has_value()) {                                          \
            __val = __opt.value();                                        \
        }                                                                 \
                                                                          \
        if (__opt.has_value())

#define ifl_end }

std::optional<char> get_option() {
    return std::optional<char>('3');
}

int main() {
    std::optional<int> opt;

    if_let_copy(tmp, opt){ifl_end}

    if_let(tmp, get_option()) {
        ifl_end
    }
}

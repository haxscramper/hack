
/*!
 * Prequsites: templates, variadic macro, std::optional (std::vector
 * of course). C macro, initializer lists
 */
#include <iostream>
#include <iterator>
#include <optional>
#include <vector>

template <class T>
std::optional<T> get_but_last(std::vector<T>& vec) {
    if (vec.size() < 2) {
        return std::optional<T>();
    } else {
        return (*std::next(vec.rbegin(), 1));
    }
}

#define TEST(vectorname, type, ...)                                       \
    std::vector<type>   vectorname       = __VA_ARGS__;                   \
    std::optional<type> vectorname##_res = get_but_last(vectorname);      \
    if ((vectorname##_res).has_value()) {                                 \
        std::cout << (vectorname##_res).value() << "\n";                  \
    } else {                                                              \
        std::cout << "No pre-last element\n";                             \
    }


int main() {
    TEST(string_vec, std::string, {"hello", "random", "string"})
    TEST(string_vec_empty, std::string, {})
    TEST(string_vec_one, std::string, {"hello"})
    TEST(int_vec, int, {1})
}

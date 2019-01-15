/*!
 * Prequsites: templates, variadic macro, std::optional (std::vector
 * of course). C macro, initializer lists
 */
#include <iostream>
#include <iterator>
#include <optional>
#include <vector>

template <class T>
std::optional<T> get_k_th(std::vector<T>& vec, int k) {
    if (vec.size() < k) {
        return std::optional<T>();
    } else {
        return *std::next(vec.begin(), k - 1);
    }
}

#define TEST(vectorname, type, k, ...)                                    \
    std::vector<type>   vectorname       = __VA_ARGS__;                   \
    std::optional<type> vectorname##_res = get_k_th(vectorname, k);       \
    if ((vectorname##_res).has_value()) {                                 \
        std::cout << (vectorname##_res).value() << "\n";                  \
    } else {                                                              \
        std::cout << "No K'th element\n";                                 \
    }


int main() {
    TEST(string_vec, std::string, 1, {"hello", "random", "string"})
    TEST(string_vec_empty, std::string, 3, {})
    TEST(string_vec_one, std::string, 4, {"hello"})
    TEST(int_vec, int, -1, {1})
    TEST(string_vec_1, std::string, 3, {"we", "wer", "w23er"})
}

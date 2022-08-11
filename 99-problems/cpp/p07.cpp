#include "common.hpp"

template <typename T, typename Out>
void insertFlattened(
    const T&                                    value,
    std::back_insert_iterator<std::vector<Out>> out) {
    out = value;
    ++out;
}

template <typename T, typename Out>
void insertFlattened(
    const std::vector<T>&                       vec,
    std::back_insert_iterator<std::vector<Out>> out) {
    for (const auto& it : vec) {
        insertFlattened(it, out);
    }
}

template <typename T>
using V = std::vector<T>;

int main() {
    V<int> out;
    insertFlattened(
        V<V<V<int>>>{
            V<V<int>>{V<int>{2, 3, 4}, V<int>{0, 3, 4}},
            V<V<int>>{V<int>{4, 5, 6}, V<int>{10, 20}}},
        std::back_inserter(out));

    std::cout << out << "\n";
}

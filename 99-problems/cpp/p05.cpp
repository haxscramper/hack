#include "common.hpp"
#include <algorithm>

int main() {
    std::vector<int> vec{1, 2, 3, 4, 4};
    std::reverse(vec.begin(), vec.end());
    std::cout << vec << "\n";
    for (int i = 0; i < vec.size() / 2; ++i) {
        std::swap(vec.at(i), vec.at(vec.size() - i - 1));
    }
    std::cout << vec << "\n";
}

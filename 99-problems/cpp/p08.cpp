#include "common.hpp"

template <typename IterIn, typename IterOut>
void deduplicate(IterIn begin, IterIn end, IterOut out) {
    auto prev = end;
    while (begin != end) {
        if (prev == end || *begin != *prev) {
            out  = *begin;
            prev = begin;
        }
        ++begin;
    }
}

int main() {
    std::vector<int> in{
        1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 3, 2, 324, 23, 423, 423};
    std::vector<int> out;
    deduplicate(in.begin(), in.end(), std::back_inserter(out));
    std::cout << out << "\n";
}

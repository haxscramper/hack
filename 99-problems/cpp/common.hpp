#include <iostream>
#include <vector>

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& vec) {
    out << "[";
    for (int i = 0; i < vec.size(); ++i) {
        if (0 < i) { out << ", "; }
        out << vec[i];
    }

    out << "]";

    return out;
}

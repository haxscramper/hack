#include <functional>
#include <iostream>
#include <optional>
#include <set>
#include <vector>

template <typename T>
using Opt = std::optional<T>;

template <typename C>
using Tok = std::function<bool(Opt<C>)>;

int main() {
}

#pragma once

#include <iostream>
#include <vector>
#include <optional>
#include <functional>
#include <variant>
#include <memory>
#include <string>
#include <cstdint>

using Str = std::string;

using i8  = std::int8_t;
using u8  = std::uint8_t;
using i16 = std::int16_t;
using u16 = std::uint16_t;
using i32 = std::int32_t;
using u32 = std::uint32_t;
using i64 = std::int64_t;
using u64 = std::uint64_t;
// clang-format off

template <typename ...Args> using Var = std::vector<Args...>;

template <typename T> using Vec  = std::vector<T>;
template <typename T> using CR   = const T&;
template <typename T> using CP   = const T*;
template <typename T> using C    = const T;
template <typename T> using Opt  = std::optional<T>;
template <typename T> using UPtr = std::unique_ptr<T>;
template <typename T> using SPtr = std::shared_ptr<T>;
template <typename T> using Func = std::function<T>;

// clang-format on

struct finally {
    Func<void(void)> action;
    explicit finally(Func<void(void)> _action) : action(_action) {}
    ~finally() { action(); }
};


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

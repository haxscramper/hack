// testing/cpp/compile_stats/src/strong_id.hpp
#pragma once

#include <cstdint>
#include <type_traits>

template <class Tag>
class StrongId {
  public:
    using underlying_type = std::uint32_t;

    constexpr StrongId() = default;
    explicit constexpr StrongId(underlying_type v) : value_(v) {}

    // copy
    constexpr StrongId(const StrongId& other)            = default;
    constexpr StrongId& operator=(const StrongId& other) = default;

    // move
    constexpr StrongId(StrongId&& other) noexcept            = default;
    constexpr StrongId& operator=(StrongId&& other) noexcept = default;

    ~StrongId() = default;

    constexpr underlying_type raw() const noexcept { return value_; }

    friend constexpr bool operator==(StrongId a, StrongId b) noexcept {
        return a.value_ == b.value_;
    }

    friend constexpr bool operator!=(StrongId a, StrongId b) noexcept {
        return !(a == b);
    }

    friend constexpr bool operator<(StrongId a, StrongId b) noexcept {
        return a.value_ < b.value_;
    }

  private:
    underlying_type value_ = 0;
};

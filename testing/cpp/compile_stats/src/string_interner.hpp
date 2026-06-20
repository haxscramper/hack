#pragma once

#include <cstdint>
#include <deque>
#include <fmt/format.h>
#include <string>
#include <string_view>
#include <unordered_map>

#include "strong_id.hpp"
#include <boost/serialization/strong_typedef.hpp>


using StrId = StrongId<struct StrIdTag>;

template <>
struct std::hash<StrId> {
    std::size_t operator()(StrId id) const noexcept {
        return std::hash<uint32_t>{}(id.raw());
    }
};

template <>
struct fmt::formatter<StrId> {
    constexpr auto parse(fmt::format_parse_context& ctx) { return ctx.begin(); }
    auto           format(StrId const& p, fmt::format_context& ctx) const {
        return fmt::format_to(ctx.out(), "StrId({})", p.raw());
    }
};


class StringInterner {
  public:
    static std::size_t toIndex(StrId id) noexcept {
        return static_cast<std::size_t>(id.raw());
    }


    StrId intern(std::string_view value) {
        auto it = id_to_string.find(value);
        if (it != id_to_string.end()) { return it->second; }

        const StrId id = static_cast<StrId>(strings.size());
        strings.emplace_back(value);
        const std::string_view stored_view = strings.back();
        id_to_string.emplace(stored_view, id);
        return id;
    }

    const std::string& get(StrId id) const { return strings.at(toIndex(id)); }

    std::size_t size() const noexcept { return strings.size(); }

  private:
    std::deque<std::string>                     strings;
    std::unordered_map<std::string_view, StrId> id_to_string;
};

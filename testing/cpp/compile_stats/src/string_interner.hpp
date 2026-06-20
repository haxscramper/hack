#pragma once

#include <cstdint>
#include <deque>
#include <string>
#include <string_view>
#include <unordered_map>

#include <boost/serialization/strong_typedef.hpp>
#include "strong_id.hpp"


using StrId = StrongId<struct StrIdTag>;


class StringInterner {
  public:
    static std::size_t toIndex(StrId id) noexcept {
        return static_cast<std::size_t>(id.raw());
    }


    StrId intern(std::string_view value) {
        auto it = id_by_string_.find(value);
        if (it != id_by_string_.end()) { return it->second; }

        const StrId id = static_cast<StrId>(strings_.size());
        strings_.emplace_back(value);
        const std::string_view stored_view = strings_.back();
        id_by_string_.emplace(stored_view, id);
        return id;
    }

    const std::string& get(StrId id) const {
        return strings_.at(toIndex(id));
    }

    std::size_t size() const noexcept { return strings_.size(); }

  private:
    std::deque<std::string>                     strings_;
    std::unordered_map<std::string_view, StrId> id_by_string_;
};

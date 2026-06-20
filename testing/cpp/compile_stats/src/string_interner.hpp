#pragma once

#include <cstdint>
#include <deque>
#include <string>
#include <string_view>
#include <unordered_map>

class StringInterner {
  public:
    using Id = std::uint32_t;

    Id intern(std::string_view value) {
        auto it = id_by_string_.find(value);
        if (it != id_by_string_.end()) { return it->second; }

        const Id id = static_cast<Id>(strings_.size());
        strings_.emplace_back(value);
        const std::string_view stored_view = strings_.back();
        id_by_string_.emplace(stored_view, id);
        return id;
    }

    const std::string& get(Id id) const { return strings_.at(id); }

    std::size_t size() const noexcept { return strings_.size(); }

  private:
    std::deque<std::string>                  strings_;
    std::unordered_map<std::string_view, Id> id_by_string_;
};

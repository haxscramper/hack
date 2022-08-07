#pragma once

#include <vector>
#include <optional>
#include <functional>
#include <memory>
#include <string>

template <typename T>
using Vec = std::vector<T>;
using Str = std::string;

template <typename T>
using CR = const T&;

template <typename T>
using Opt = std::optional<T>;

template <typename T>
using UPtr = std::unique_ptr<T>;

template <typename T>
using Func = std::function<T>;

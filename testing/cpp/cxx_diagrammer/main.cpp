#include <iostream>
#include <coroutine>
#include <vector>
#include <memory>
#include <numeric>
#include <string>
#include <tuple>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <CLI/App.hpp>
#include <CLI/Formatter.hpp>
#include <CLI/Config.hpp>

// clang-format off
template <typename T>
struct generator {
    struct promise_type {
        T                   current_value;
        std::suspend_always yield_value(T value) { this->current_value = value; return {}; }
        std::suspend_always initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        generator           get_return_object() { return generator{this}; };
        void                unhandled_exception() { std::terminate(); }
        void                return_void() {}
    };

    class iterator
    {
        std::coroutine_handle<promise_type> coro;
        bool                                done;

      public:
        iterator(std::coroutine_handle<promise_type> _coro, bool _done)
            : coro(_coro), done(_done) {}

        bool      operator==(iterator const& _right) const { return done == _right.done; }
        bool      operator!=(iterator const& _right) const { return !(*this == _right); }
        T const&  operator*() const { return coro.promise().current_value; }
        T const*  operator->() const { return &(operator*()); }
        T&        operator*() { return coro.promise().current_value; }
        T*        operator->() { return &(operator*()); }
        iterator& operator++() { coro.resume(); done = coro.done(); return *this; }
    };

    iterator begin() { p.resume(); return {p, p.done()}; }
    iterator end() { return {p, true}; }
    generator(generator const&) = delete;
    generator(generator&& rhs) : p(rhs.p) { rhs.p = nullptr; }
    ~generator() { if (p) { p.destroy(); } }

  private:
    explicit generator(promise_type* p)
        : p(std::coroutine_handle<promise_type>::from_promise(*p)) {}

    std::coroutine_handle<promise_type> p;
};

// clang-format on

generator<int> values() {
    co_yield 12;
    co_yield 24;
}

template <typename T>
std::vector<T> operator+(std::vector<T> lhs, const std::vector<T>& rhs) {
    std::copy(std::begin(rhs), std::end(rhs), std::back_inserter(lhs));
    return lhs;
}

class shape
{
    std::vector<shape> nested;
    const std::string  name;

  public:
    const std::string& get_name() const { return name; }
    shape(
        const std::string&           _name,
        std::initializer_list<shape> _nested = {})
        : name(_name), nested(_nested) {}

    generator<shape*> items() {
        co_yield this;
        for (auto& item : nested) {
            for (auto& sub : item.items()) {
                co_yield sub;
            }
        }
    }

    generator<std::tuple<shape*, std::vector<int>>> pairs() {
        co_yield {this, {}};
        int idx = 0;
        for (auto& item : nested) {
            for (auto& [sub, index] : item.pairs()) {
                auto tmp = index + std::vector<int>{idx};
                co_yield {sub, tmp};
            }
            ++idx;
        }
    }
};

struct cli_options {
    std::vector<std::string> filenames;
};

int main(int argc, const char** argv) {
    CLI::App    app{"cxx_diagrammer"};
    cli_options values;
    app.add_option("file", values.filenames)
        ->required(true)
        ->description("one or more input files");

    CLI11_PARSE(app, argc, argv);

    shape top{
        "top",
        {shape{"first"}, shape{"sub", {shape{"second"}, shape{"third"}}}}};

    for (auto& [it, path] : top.pairs()) {
        fmt::print("name: {} path: {}\n", it->get_name(), path);
    }
}

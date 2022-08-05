#include <exception>
#include <string>
#include <git2.h>
#include <fmt/core.h>

namespace git {
struct exception : public std::exception {
    std::string message;
    inline exception(int error, const char* funcname) {
        const git_error* e = git_error_last();
        message            = fmt::format(
            "Error {}/{} while calling {}: {}",
            error,
            funcname,
            e->klass,
            e->message);
    }

    virtual const char* what() const noexcept override {
        return message.c_str();
    }
};
} // namespace git

#include <iostream>
#define __GIT_THROW_EXCEPTION(code, function)                             \
    throw git::exception(code, function);

namespace git {
#include "gitwrap.hpp"
}

int main() {
    git::libgit2_init();

    std::cout << "1\n";
    return 0;
}

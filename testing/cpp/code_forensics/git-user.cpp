#include <exception>
#include <git2/config.h>
#include <iostream>
#define __GIT_THROW_EXCEPTION(code, function)                             \
    throw "git failed with code???";

namespace git {
#include "gitwrap.hpp"
}

int main() {
    std::cout << "1\n";
    return 0;
}

#include "lib.hpp"
#include <iostream>

WithConstructor::WithConstructor(int arg) {
    std::cout << "Called with constructor, single argument\n";
}


WithConstructor::WithConstructor(int arg1, int arg2) {
    std::cout << "Called with constructor, two arguments\n";
}

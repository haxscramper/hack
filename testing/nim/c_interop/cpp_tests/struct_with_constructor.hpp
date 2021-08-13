#include <iostream>

struct Struct {
    int field;

    Struct() {
        std::cout << "Constructor was called\n";
    }
    ~Struct() {
        std::cout << "Destructor was called\n";
    }
};

#include <iostream>
#include <stdexcept>

#include <backward.hpp>

void d() { throw std::range_error("Range"); }
void c() { d(); }
void b() { c(); }
void a() { b(); }

int main(int argc, char** argv) {
    std::cout << "Hell" << std::endl;
    a();
    return 0;
}

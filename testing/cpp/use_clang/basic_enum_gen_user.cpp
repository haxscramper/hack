#include "basic_enum.hpp"
#include "basic_enum_aux.tmp.hpp"
#include "basic_enum_aux.tmp.cpp"

#include <iostream>

int main() { std::cout << to_string(Name::Field1) << std::endl; }

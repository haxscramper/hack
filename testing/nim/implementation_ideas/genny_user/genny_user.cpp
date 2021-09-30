#include "generated/genny_main.hpp"
#include <iostream>

int main() {
    Obj obj(12);
    std::cout << "Object data is " << obj.get_data() << "\n";
}

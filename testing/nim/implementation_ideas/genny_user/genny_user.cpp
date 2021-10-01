#include "generated/genny_main.hpp"
#include <iostream>

int main() {
    Obj obj;
    obj.field = 16;
    std::cout << "Object data is " << obj.get_data() << "\n";

    Obj::items_iter iter = obj.items_begin();
}

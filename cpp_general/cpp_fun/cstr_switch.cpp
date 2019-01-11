#define cstr_switch(input_c_string)                                       \
    const char* string_to_test = input_c_string;                          \
    if (false)

#define cstr_case(input_c_string)                                         \
    }                                                                     \
    else if (string_to_test == input_c_string) {


#define cstr_default                                                      \
    }                                                                     \
    else {

#include <iostream>

int main() {
    cstr_switch("Hello two") {
        cstr_case("Hello one") {
            std::cout << "Hello one\n";
        };
        cstr_case("Hello two") {
            std::cout << "Hello two\n";
        };
    }
    return 0;
}

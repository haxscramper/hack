#include <cstring>
#include <functional>
#include <iostream>
#include <memory>
#include <stdio.h>
#include <typeinfo>

struct C {
    void method() {
        puts("Method");
    }
};

template <class T>
char* as_bytes(T& t) {
    return *((char**)&t);
}

/// Print object binary representation (bits)
template <class T>
void bit_print(
    const T&     t,
    const size_t base          = 2,
    const size_t bytes_per_row = 4) {

    // TODO print T's name
    puts("----");

    char* c = as_bytes(t);
    for (size_t byte = 0; byte < sizeof(T); ++byte) {
        for (size_t bit = 1 << 8; bit; bit >>= 1) {
            if (bit & c[byte]) {
                putchar('1');
            } else {
                putchar('0');
            }

            if (bit == 1 << 4) {
                putchar('\'');
            }
        }

        if ((byte + 1) % bytes_per_row == 0) {
            putchar('\n');
        } else {
            putchar(' ');
        }
    }

    puts("----");
}


struct Ptr {
    static const size_t size = sizeof(&C::method);
    using arr                = char[size];
    arr c;

    template <class T, typename... Args>
    Ptr(void (T::*method)(Args...)) {
        bit_print(c);
        std::memcpy(&method, &c, size);
        puts("Copied pointer");
        bit_print(method);
    }

    template <class T, typename... Args>
    void invoke(T* obj, Args... args) {
        void (T::*method)(Args...);
        puts("c:");
        bit_print(c);

        std::memcpy(&c, &method, size);

        bit_print(method);
        puts("Retrieved method");

        (obj->*method)(args...);
    }
};

int main() {
    printf("Method pointer size: %zu\n", sizeof(&C::method));

    Ptr p(&C::method);
    C   c;

    p.invoke(&c);

    printf("Done\n");
    return 0;
}

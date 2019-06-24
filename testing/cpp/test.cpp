#include <cstring>
#include <stdio.h>

struct C {
    void method(char c) {
        putchar(c);
        puts("Method");
    }
};

struct Ptr {
    void member_method();

    static const size_t size = sizeof(&Ptr::member_method);
    using arr                = char[size];
    arr c;

    template <class T, typename... Args>
    Ptr(void (T::*method)(Args...)) {
        std::memcpy(&c, &method, size);
    }

    // TODO Support returning values from slot?
    template <class T, typename... Args>
    void invoke(T* obj, Args... args) {
        void (T::*method)(Args...);
        std::memcpy(&method, &c, size);

        (obj->*method)(args...);
    }
};


int main() {
    Ptr p(&C::method);
    C   c;
    C   o;

    p.invoke(&o, '#');

    return 0;
}

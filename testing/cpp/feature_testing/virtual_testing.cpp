#include <iostream>

struct Base {
    virtual void method() {
        std::cout << "base implementation" << std::endl;
    }
    Base() { method(); }
};

struct Derived : public Base {
    Derived() { method(); }
    virtual void method() override {
        std::cout << "override implementation" << std::endl;
    }
};

int main() { Derived d; }

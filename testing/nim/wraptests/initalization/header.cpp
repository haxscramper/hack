#include "header.hpp"
#include <iostream>

void aggregateParam(Aggregate arg) {
    std::cout << __func__ << " " << arg.fld1 << " '" << arg.fld2 << "'\n";
}


List::List(std::initializer_list<int> arg) {
    for (const auto& val : arg) {
        std::cout << "- " << val << "\n";
    }
}

void initListParam(List arg) {}

template <class T>
int Functor<T>::operator()(T arg, T val) {}

template <typename T>
void funcArg(typename What<T>::functor functor) {
    What<T>::functor(functor);
}

void test() {
    auto what1 = What<int>();
    auto what2 = What<int>(What<int>::functor());
}

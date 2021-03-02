#pragma once
#include <initializer_list>

struct Aggregate {
    int  fld1;
    char fld2;
};

void aggregateParam(Aggregate arg = {12, '1'});

struct List {
    List(std::initializer_list<int> arg);
};

void initListParam(List arg = {1, 2, 3, 4, 5});

template <class T>
struct Functor {
    int operator()(T arg, T val);
};

template <class T>
struct What {
    using functor = Functor<T>;

    functor f;
    What(functor f = functor()) {}
};

template <typename T>
struct P {
    T f;
};

template <typename T>
struct S {
    typedef P<T> value_type;

    value_type get(T arg) {
        return P<T>{arg};
    };
};

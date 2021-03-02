template <typename T>
struct Functor {
    bool operator()(T arg) { (void)(arg); }
};


// Template type with default generic parameter
template <typename T, typename D = Functor<T>>
struct Gen {
    using FunctorT = D;

    D functor;

    int _fld;

    Gen(int arg, D _functor = FunctorT()) : _fld(arg), functor(_functor){};
};

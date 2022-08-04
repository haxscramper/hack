struct A;
struct B;
struct A {
    struct B* b;
};
struct B {
    struct A* a;
};

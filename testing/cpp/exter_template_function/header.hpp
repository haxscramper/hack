struct RegularClass {
    template <typename T>
    int templateMethod() {
        return 2;
    }
};

extern template int RegularClass::templateMethod<float>();

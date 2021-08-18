struct Opc {
    virtual void eval(int* result) = 0;
};
struct OpcInc : Opc {
    void eval(int* result) {
        ++(*result);
    }
};
struct OpcDec : Opc {
    void eval(int* result) {
        --(*result);
    }
};
struct OpcMul2 : Opc {
    void eval(int* result) {
        (*result) *= 2;
    }
};
struct OpcDiv2 : Opc {
    void eval(int* result) {
        (*result) /= 2;
    }
};
struct OpcAdd7 : Opc {
    void eval(int* result) {
        (*result) += 7;
    }
};
struct OpcNeg : Opc {
    void eval(int* result) {
        (*result) = -(*result);
    }
};

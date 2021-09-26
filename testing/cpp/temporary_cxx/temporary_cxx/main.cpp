#include <cstring>
#include <iostream>

template <typename Out, typename In>
Out nuclear_cast(In in) {
    static_assert(
        sizeof(Out) == sizeof(In),
        "Cannot nuclear cast objects of different size");
    Out result;
    std::memcpy(&result, &in, sizeof(result));
    return result;
}

struct CxxParent {
    char other;
};

struct CxxStruct : public CxxParent {
    int field;
    CxxStruct(int _field) : field(_field) {
    }
};

struct CStruct {
    unsigned char padding[sizeof(CxxStruct)];
};

CxxStruct* toCxx(CStruct* c) {
    return (CxxStruct*)(c);
}


CStruct z(int arg) {
    return nuclear_cast<CStruct>(CxxStruct(arg));
}

int main() {
    CStruct c = z(12);
    std::cout << toCxx(&c)->field << "\n";

    return 0;
}

#include <iostream>

struct Obj {
    float f1;
    char  f3;
};

struct CbPayload {
    Obj* p;
};

void callback(CbPayload payload) {
    Obj& obj = *(payload.p);
    obj.f3   = '1';
}

int main() {
    Obj obj;
    std::cout << obj.f3 << "\n";
    CbPayload payload;
    payload.p = &obj;
    callback(payload);
    std::cout << obj.f3 << "\n";
}

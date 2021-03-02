struct S {};

S operator""_S(const char* s, unsigned long len) { return S(); }

// int main() {
//     S           val    = "hello"_S;
//     const char* strval = "123";
//     S val2             = operator""_S(strval, 4);
// }

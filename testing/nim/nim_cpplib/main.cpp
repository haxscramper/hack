// main.cpp
#include "mylib.h"
#include <iostream>

using namespace std;

int main() {
    NimMain();
    parseCSVListLine("one,two,three");
    parseCSVListLine("one,two,three", 2);

    TestVar v;
    v.kind = true;
    v.a    = 12;
    testVar(v);

    return 0;
}

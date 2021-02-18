#include <emscripten.h>
#include <stdio.h>

void testCall() {
    printf("Function is called\n");
}

void printNumber(int arg) {
    printf("Printing number%d\n", arg);
}

int getReturn(int arg) {
    return arg * arg + 100;
}

int main(int argc, char** argv) {
    EM_ASM(InitWrappers());
}

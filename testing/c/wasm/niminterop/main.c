#include "nimcache/mainlib.h"
#include <emscripten.h>
#include <stdio.h>


int main(int argc, char** argv) {
    NimMain();
    EM_ASM(InitWrappers());
}

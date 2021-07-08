#include "lib.h"
#include <stdio.h>

typedef NimStringDesc NStr;

int main() {
    NimMain();

    NStr str = n_string("hello world test\n");

    printf(n_concat(str, n_string("hwllo world")));
}

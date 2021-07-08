#include "lib.h"
#include <stdio.h>

typedef NimStringDesc* NStr;

int main() {
    NimMain();

    NStr str = n_string("base string: ");

    for (int i = 0; i < 10; ++i) {
        printf(n_cstr(n_concat3(str, n_str_int(i), n_string("\n"))));
    }
}

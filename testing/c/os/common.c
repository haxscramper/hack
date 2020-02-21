#include "common.h"

int get_int() {
    return 12;
}

void mlog(char* msg) {
    pprefix();
    printf("%s\n", msg);
}


void milog(char* msg, int value) {
    pprefix();
    printf("%s %d\n", msg, value);
}

void pprefix() {
    printf("\033[32mLOG:\033[0m ");
}

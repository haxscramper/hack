#include "common.h"

int get_int() {
    return 12;
}

void mlog(char* msg) {
    pprefix();
    printf("%s\n", msg);
}


void merr(char* msg) {
    printf("\033[31mERR:\033[0m %s\n", msg);
}

void mlerr() {
    merr(strerror(errno));
}


void milog(char* msg, int value) {
    pprefix();
    printf("%s %d\n", msg, value);
}

void pprefix() {
    /* printf("\033[32mLOG:\033[0m "); */
    printf("LOG: ");
}

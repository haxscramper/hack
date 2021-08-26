#include <stdbool.h>
#include <stdio.h>

struct PosStr {
    bool isSlice;
    union {
        struct {
            int* str;
        } _isSlice_1;
        struct {
            int sliceIdx;
        } _isSlice_2;
    };
    bool bufferActive;
};


void hshow(struct PosStr* str) {
    if (!(((1
            & ((unsigned short int)1
               << ((unsigned long long int)((*str).isSlice) & 7U)))
           != 0))) {
        puts("Bad kind");
    }
}

void main() {
    struct PosStr str;
    str.isSlice = false;
    hshow(&str);
}

#include "clib.h"

Node* newNode() {
    return (Node*)(malloc(sizeof(Node)));
}

void freeNode(Node* node) {
    puts("Free node using library function");
}

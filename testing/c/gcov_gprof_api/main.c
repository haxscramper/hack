#include <dlfcn.h>
#include <stdio.h>
#include <sys/gmon.h>
#include <sys/gmon_out.h>

extern struct __bb* __bb_head;


void load() {
    for (int i = 0; i < 0xffffff; ++i) {}
}


int main() {
    load();
    struct __bb* grp = __bb_head;
}

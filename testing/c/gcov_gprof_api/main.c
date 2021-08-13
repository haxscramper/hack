#include <dlfcn.h>
#include <stdio.h>
#include <sys/gmon.h>
#include <sys/gmon_out.h>

int main() {
    void* dlopen_res = dlopen("libc.so.6", RTLD_LAZY);
    if (dlopen_res == NULL) {
        printf("dlopen_res is NULL: %s\n", dlerror());
        return 1;
    }

    void* bb_so = dlsym(dlopen_res, "__bb_head");
    if (bb_so == NULL) {
        printf("bb_so is NULL: %s\n", dlerror());
        return 1;
    }
}
//    struct __bb* bb_head = (struct __bb*)bb_so;

//    for (struct __bb* grp = bb_head; grp; grp = grp->next) {
//        puts(grp->filename);
//    }

//    return 0;
//}

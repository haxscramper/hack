#include <dlfcn.h>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUF_LEN 256

int main() {
    std::ifstream maps;
    maps.open("/proc/self/maps");

    std::string buf;
    while (std::getline(maps, buf)) {
        std::stringstream tmp{buf};
        std::string       location, permissions, dev, inode, source;
        int               offset;

        tmp >> location >> permissions >> offset >> dev >> inode >> source;

        std::cout << source << " @ " << offset << "\n";
    }


    void* dlopen_res = dlopen("./shared.so", RTLD_LAZY);
    if (dlopen_res == NULL) {
        printf("dlopen_res is NULL: %s\n", dlerror());
        return 1;
    }

    if (dlsym(dlopen_res, "visible_sym") == NULL) {
        printf("bb_so is NULL: %s\n", dlerror());
        return 1;
    } else {
        printf("'visible_sym' open ok\n");
    }


    if (dlsym(dlopen_res, "hidden_sym") == NULL) {
        printf("bb_so is NULL: %s\n", dlerror());
        return 1;
    }
}

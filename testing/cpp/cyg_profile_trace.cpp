#include <stdio.h>
#include <cmath>
#include <cstring>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <dlfcn.h>
#include <cassert>

FILE* trace_out;

extern "C" __attribute__((no_instrument_function)) void __cyg_profile_func_enter(
    void* this_fn,
    void* call_site) {
    Dl_info di;
    dladdr(this_fn, &di);
    if (trace_out != nullptr) {
        fprintf(
            trace_out,
            "> %p %p %s\n",
            this_fn,
            call_site,
            di.dli_sname ? di.dli_sname : "<unknown>");
    }
}

extern "C" __attribute__((no_instrument_function)) void __cyg_profile_func_exit(
    void* this_fn,
    void* call_site) {
    if (trace_out != nullptr) {
        fprintf(trace_out, "< %p %p\n", this_fn, call_site);
    }
}

template <int N>
int add_static(int Q) {
    return N + Q;
}

void void_has_q() { printf("'q' found in string\n"); }

void rec_value(const char* param, int& depth) {
    bool res = false;
    for (int i = 0; i < strlen(param); ++i) {
        if (param[i] == 'q') {
            void_has_q();
            res = true;
        }
    }

    if (res) {
        rec_value(param + 1, depth);
        depth = add_static<12>(depth);
    }
}

bool eq_names(char* str1, char* str2) {
    auto l1 = strlen(str1);
    auto l2 = strlen(str2);
    for (int offset = (l1 < l2 ? l1 : l2) - 1; 0 <= offset; --offset) {
        if (str1[l1 - offset] == str2[l2 - offset]) {
            if (str1[offset] == '/') { return true; }
        } else {
            return false;
        }
    }
    return false;
}

std::size_t find_offset(char* inbin, pid_t pid) {
    char buf[1024];
    sprintf(buf, "/proc/%d/maps", pid);
    FILE* file = fopen(buf, "r");

    unsigned int from = 0, to = 0, offset = 0, major = 0, minor = 0,
                 inode = 0;
    char flags[4];
    char exec[256];
    while (fgets(buf, sizeof(buf), file)) {
        sscanf(
            buf,
            "%x-%x %4c %x %x:%x %d %s",
            &from,
            &to,
            flags,
            &offset,
            &major,
            &minor,
            &inode,
            &exec);

        if (flags[2] == 'x' && eq_names(&exec[0], inbin)) {
            printf("found %x %x\n", from, to);
        }
    }
    return 0;
}

int main(int argc, char** argv) {
    pid_t pid = getpid();
    printf("rec_value: %p\n", rec_value);
    printf("started process id %d\n", pid);
    find_offset(argv[0], pid);

    trace_out = fopen(argc == 1 ? "cyg_profile_trace.log" : argv[1], "w");
    auto parameter = 2 < argc ? argv[2] : "noqneq";

    int depth = add_static<9>(0);
    rec_value(parameter, depth);

    fclose(trace_out);
    printf("Execution done\n");
}

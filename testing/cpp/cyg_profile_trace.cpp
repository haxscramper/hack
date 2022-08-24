#include <stdio.h>
#include <cstring>
#include <cassert>

FILE* trace_out;

extern "C" __attribute__((no_instrument_function)) void __cyg_profile_func_enter(
    void* this_fn,
    void* call_site) {
    if (trace_out != nullptr) {
        fprintf(trace_out, "> %p %p\n", this_fn, call_site);
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

void void_has_q() { printf("'q' found in string"); }

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

int main(int argc, char** argv) {
    trace_out = fopen(argc == 1 ? "cyg_profile_trace.log" : argv[1], "w");
    auto parameter = 2 < argc ? argv[2] : "noqneq";

    int depth = add_static<9>(0);
    rec_value(parameter, depth);

    fclose(trace_out);
    printf("Execution done");
}

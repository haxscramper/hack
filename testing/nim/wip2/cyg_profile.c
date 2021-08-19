#include <stdbool.h>
#include <stddef.h>

#include "cyg_profile.h"

#ifdef __cplusplus
extern "C" {
#endif

__attribute__((no_instrument_function)) int __store_event(
    struct int_list** fubar,
    struct event      value) {
    size_t x = *fubar ? fubar[0]->size : 0;
    size_t y = x + 1;

    if ((x & y) == 0) {
        void* temp = realloc(
            *fubar, sizeof **fubar + (x + y) * sizeof fubar[0]->value[0]);
        if (!temp) {
            return 1;
        }
        *fubar = temp; // or, if you like, `fubar[0] = temp;`
    }

    fubar[0]->value[x] = value;
    fubar[0]->size     = y;
    return 0;
}

struct int_list* array      = NULL;
bool             do_profile = true;


__attribute__((no_instrument_function)) struct int_list* __cyg_get_profile() {
    return array;
}

__attribute__((no_instrument_function)) void __cyg_disable_profile() {
    do_profile = false;
}

__attribute__((no_instrument_function)) void __cyg_profile_func_enter(
    void* func,
    void* caller) {
    if (do_profile) {
        struct event ev = {
            .kind = ek_enter, .func = func, .caller = caller};
        __store_event(&array, ev);
    }
}
__attribute__((no_instrument_function)) void __cyg_profile_func_exit(
    void* func,
    void* caller) {
    if (do_profile) {
        struct event ev = {
            .kind = ek_exit, .func = func, .caller = caller};
        __store_event(&array, ev);
    }
}

#ifdef __cplusplus
};
#endif

enum event_kind
{
    ek_enter,
    ek_exit
};

struct event {
    enum event_kind kind;
    void*           func;
    void*           caller;
};

struct int_list {
    int          size;
    struct event value[];
};

__attribute__((no_instrument_function)) void __cyg_disable_profile();
__attribute__((no_instrument_function)) struct int_list* __cyg_get_profile();

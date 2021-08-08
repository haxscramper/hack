#include "papi.h"
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

// Make sure to create some cache misses:
void do_misses() {
    int     i;
    int     sz   = 30 * 1000 * 1000;
    double* data = (double*)malloc(sz * sizeof(double));
    for (i = 0; i < sz; i++)
        data[i] = i * 100;
    printf("  do_misses: initialized 30M doubles\n");
}


int main(int argc [[maybe_unused]], char** argv [[maybe_unused]]) {


    int               retval;
    int               EventSet = PAPI_NULL;
    long long         values[2];
    char              descr[PAPI_MAX_STR_LEN];
    PAPI_event_info_t evinfo;
    //    PAPI_mh_level_t*  L;

    int       i           = 0;
    const int eventlist[] = {
        PAPI_TOT_CYC, PAPI_L3_TCM
        /* PAPI_L1_DCA, */
        /* PAPI_L1_DCM, */
        /* PAPI_L1_DCH */
    };

    printf("Initializing...\n");
    if ((retval = PAPI_library_init(PAPI_VER_CURRENT))
        != PAPI_VER_CURRENT) {
        printf("error\n");
        exit(1);
    }

    printf("Done initializing...\n");

    retval = PAPI_create_eventset(&EventSet);
    if (retval != PAPI_OK) {
        exit(3);
    }

    printf("Created eventset\n");

    if (PAPI_event_code_to_name(eventlist[i], descr) != PAPI_OK) {
        exit(4);
    }
    if (PAPI_add_event(EventSet, eventlist[i]) != PAPI_OK) {
        exit(5);
    }

    printf("Added event: %s\n", descr);

    if (PAPI_get_event_info(eventlist[i], &evinfo) != PAPI_OK) {
        exit(6);
    }

    printf(
        "\nEvent: %s\nShort: %s\nLong: %s\n\n",
        evinfo.symbol,
        evinfo.short_descr,
        evinfo.long_descr);


    retval = PAPI_start(EventSet);
    if (retval != PAPI_OK) {
        exit(7);
    }

    retval = PAPI_read(EventSet, &values[0]);
    if (retval != PAPI_OK) {
        exit(8);
    }
    printf(" Read initial counter: %lld\n", values[0]);

    retval = PAPI_reset(EventSet);
    if (retval != PAPI_OK) {
        exit(9);
    }

    do_misses();

    retval = PAPI_read(EventSet, &values[0]);
    if (retval != PAPI_OK) {
        exit(8);
    }
    printf(" Read after memory traffic: %lld\n", values[0]);

    retval = PAPI_read(EventSet, &values[0]);
    if (retval != PAPI_OK) {
        exit(10);
    }

    retval = PAPI_read(EventSet, &values[1]);
    if (retval != PAPI_OK) {
        exit(11);
    }

    printf(" Read counter after reset: %lld\n", values[0]);
    printf(" And then consecutively: %lld\n", values[1]);

    retval = PAPI_stop(EventSet, NULL);
    if (retval != PAPI_OK) {
        exit(12);
    }

    retval = PAPI_remove_event(EventSet, eventlist[i]);
    if (retval != PAPI_OK) {
        exit(13);
    }

    retval = PAPI_destroy_eventset(&EventSet);
    if (retval != PAPI_OK) {
        exit(14);
    }

    return 0;
}

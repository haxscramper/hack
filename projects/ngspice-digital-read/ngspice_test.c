#include <ctype.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h> // Required by `sharedspice.h`
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "sharedspice.h"

int veccount = 0;

int ng_initdata(pvecinfoall intdata, int ident, void* userdata) {
    printf("Pre-simulation callback for '%s'\n", intdata->type);
    veccount = intdata->veccount;
    printf(
        "In total there are %d in this simulation\n", intdata->veccount);
    for (int i = 0; i < intdata->veccount; i++) {
        printf("  [%d] %s\n", i, intdata->vecs[i]->vecname);
    }
    return 0;
}

int ng_getchar(char* outputreturn, int ident, void* userdata) {
    printf("@ %s\n", outputreturn);
    return 0;
}


int main() {
    int ret = ngSpice_Init(
        ng_getchar, NULL, NULL, NULL, ng_initdata, NULL, NULL);

    char** ca = (char**)malloc(sizeof(char*) * 8);

    // First line is used as title for circuit
    ca[0] = strdup("fail test");

    // Describe circuit line by line
    ca[1] = strdup("V1 0 1 5");
    ca[2] = strdup("V2 0 2 5");
    ca[3] = strdup("R1 0 1 10");
    ca[4] = strdup("R2 0 2 10");

    // Specify simulation parameters
    ca[5] = strdup(".dc v1 0 5 1");

    // End of netlist
    ca[6] = (".end");
    ca[7] = NULL;

    ngSpice_Circ(ca);

    ngSpice_Command("run"); // Run simulation

    // Get name of the current plot
    char* curplot = ngSpice_CurPlot();

    // Get names of all vectors in plot
    char** vecarray = ngSpice_AllVecs(curplot);

    puts("After simulation values are:");
    // Print length of first vector
    for (int i = 0; i < veccount; ++i) {
        char  plotvec[256];
        char* vecname = vecarray[i];

        sprintf(plotvec, "%s.%s", curplot, vecname);
        pvector_info myvec     = ngGet_Vec_Info(plotvec);
        int          veclength = myvec->v_length;

        printf("%s\n", plotvec);
        for (int k = 0; k < veclength; ++k) {
            printf("    %f\n", myvec->v_realdata[k]);
        }
    }
    puts("Done");
}

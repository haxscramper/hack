#include <ctype.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h> // Required by `sharedspice.h`
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "sharedspice.h"


static bool errorflag    = false; // DOC
int         vecgetnumber = 0;     // DOC
bool        no_bg        = true;  // DOC

/// Case-insensentive strings equality comparison
/// Used when printing stdout
int ng_getchar(char* outputreturn, int ident, void* userdata) {
    printf("@ %s\n", outputreturn);
    return 0;
}

int ng_getstat(char* outputreturn, int ident, void* userdata) {
    printf("# %s\n", outputreturn);
    return 0;
}

int ng_exit(
    int   exitstatus,
    bool  immediate,
    bool  quitexit,
    int   ident,
    void* userdata) {
    puts("ngspice exit requested");
    return exitstatus;
}

int ng_initdata(pvecinfoall intdata, int ident, void* userdata) {
    printf("Pre-simulation callback for '%s'\n", intdata->type);
    for (int i = 0; i < intdata->veccount; i++) {
        printf("  Vector: %s\n", intdata->vecs[i]->vecname);
    }
    return 0;
}


static bool has_break  = false;
int         testnumber = 0;
void        alterp(int sig);

int main() {

    // Init ngspice
    int ret = ngSpice_Init(
        ng_getchar, ng_getstat, ng_exit, NULL, ng_initdata, NULL, NULL);


    ngSpice_Command("circbyline fail test");
    ngSpice_Command("circbyline V1 0 1 5");
    ngSpice_Command("circbyline V2 0 2 5");
    ngSpice_Command("circbyline R1 0 1 10");
    ngSpice_Command("circbyline R2 0 2 10");
    ngSpice_Command("circbyline .dc v1 0 5 1");
    ngSpice_Command("circbyline .end");
    ngSpice_Command("run");

    char*  curplot  = ngSpice_CurPlot();
    char** vecarray = ngSpice_AllVecs(curplot);
    /* get length of first vector */
    if (vecarray) {
        char  plotvec[256];
        char* vecname = vecarray[0];

        sprintf(plotvec, "%s.%s", curplot, vecname);
        pvector_info myvec     = ngGet_Vec_Info(plotvec);
        int          veclength = myvec->v_length;

        printf(
            "\nActual length of vector %s is %d\n\n", plotvec, veclength);
    }
}

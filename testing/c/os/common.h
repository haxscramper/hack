#include <errno.h>
#include <stdio.h>
#include <string.h>

int get_int();

void mlog(char* msg);
void merr(char* msg);
void milog(char* msg, int value);
void mlerr();

void pprefix();

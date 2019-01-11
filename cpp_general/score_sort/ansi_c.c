#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>

int str_len_comp(const void* _lhs, const void* _rhs) {
    return strlen(*(char**)_lhs) - strlen(*(char**)_rhs);
    // return (strcmp(*(char**)_lhs, *(char**)_rhs));
}

int main() {
    char* str_arr[] = {"Hello",
                       "One",
                       "2",
                       "sadfasd"};
    qsort(
        str_arr,
        sizeof(str_arr) / sizeof(str_arr[0]),
        sizeof(char*),
        str_len_comp);

    for (int i = 0; i < sizeof(str_arr) / sizeof(str_arr[0]); ++i) {
        printf("%lu | %s\n", strlen(str_arr[i]), str_arr[i]);
    }

    return 0;
}

#include <stdio.h>

int main() {
    char* string = "random string to reverse";

    printf("Before: %s\n", string);


    size_t len = 0;

    char* str = string;

    while (*(str++) != '\0') {}

    while (*string != '\0') {
        printf("-- %c\n", *str);
        char tmp = *str;
        *str     = *string;
        *string  = tmp;
        --str;
        ++string;



    }

    printf("After: %s", string);

    return 0;
}


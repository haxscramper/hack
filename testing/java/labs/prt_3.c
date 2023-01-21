#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    char* num_str;
    if (argc < 2) {
        num_str = "12";
    } else {
        num_str = argv[1];
    }

    int num = atoi(num_str);
    if (num == 0) {
        putchar('0');
    } else {
        char buf[32];
        int  i = 0;
        while (num != 0) {
            buf[++i] = ((num % 2 == 1 ? '1' : '0'));
            num /= 2;
        }
        int n = i;
        while (i > 0) {
            putchar(buf[i--]);
        }
    }
}

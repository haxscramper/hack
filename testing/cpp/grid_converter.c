#include <stdio.h>

int main(int argc, char** argv) {
    char c;
    if (argc == 2) {
        FILE* file = fopen(argv[1], "r");
        while((c = (char)fgetc(file)) != EOF) {
            putchar(c == '1' ? 'F' : c);
        }
    } else {
        while ((c = getchar()) != EOF) {
            putchar(c == '1' ? 'F' : c);
        }
    }
}

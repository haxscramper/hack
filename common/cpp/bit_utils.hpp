#include <stdio.h>

template <class T>
char* as_bytes(T& t) {
    return *((char**)&t);
}

/// Print object binary representation (bits)
template <class T>
void bit_print(
    const T&     t,
    const size_t base          = 2,
    const size_t bytes_per_row = 4) {

    // TODO print T's name

    char* c = *((char**)&t);
    for (size_t byte = 0; byte < sizeof(T); ++byte) {
        for (size_t bit = 1 << 7; bit; bit >>= 1) {
            if (bit & c[byte]) {
                putchar('1');
            } else {
                putchar('0');
            }

            if (bit == 1 << 4) {
                putchar('\'');
            }
        }

        if ((byte + 1) % bytes_per_row == 0) {
            putchar('\n');
        } else {
            putchar(' ');
        }
    }
}

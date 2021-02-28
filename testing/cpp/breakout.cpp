#include <iostream>

int main() {
    while (true) {
        if (1 == 1) {
            switch (0) {
                case 0: break;
            }

            puts("After switch break");
        }
        puts("After if");
    }
    puts("After while");
};

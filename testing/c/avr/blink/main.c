#include <avr/io.h>
#include <util/delay.h>


int main() {
    DDRB = 0x20;
    while (1) {
        PORTB = 0x20;
        _delay_ms(500);
        PORTB = 0;
        _delay_ms(500);
    }
}

#include <avr/io.h>
#include <util/delay.h>

int main() {
  DDRB = DDRB | 0x20;
  for(;;) {
    PORTB = PORTB | 0x20;
    _delay_ms(100);
    PORTB = PORTB & ~0x20;
    _delay_ms(100);
  }
}

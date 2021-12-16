MCU=atmega32u4
CFLAGS=-Wall -mmcu=$(MCU) -Os
LDFLAGS=-Wl,-gc-sections -Wl,-relax
CC=avr-gcc

all: blink.hex
	./reset.py
	avrdude -v -p $(MCU) -cavr109 -P/dev/ttyACM0 -c arduino -b57600 -D -Uflash:w:blink.hex:i

clean:
	rm -f *.o *.hex *.obj *.hex

%.hex: %.obj
	avr-objcopy -R .eeprom -O ihex $< $@

%obj: blink.c
	$(CC) $(CFLAGS) blink.c $(LDFLAGS) -o $@

.PHONY: all

MCU=atmega32u4
CFLAGS=-Wall -mmcu=$(MCU) -Os
LDFLAGS=-Wl,-gc-sections -Wl,-relax
CC=avr-gcc

all: main.hex
	avrdude -B10 -F -p atmega32u4 -c usbasp \
    -U flash:w:main.hex \
    -U hfuse:w:0xd8:m \
    -U efuse:w:0xcb:m

clean:
	rm -f *.o *.hex *.obj *.hex

%.hex: %.obj
	avr-objcopy -R .eeprom -O ihex $< $@

%obj: main.c
	$(CC) $(CFLAGS) main.c $(LDFLAGS) -o $@

.PHONY: all

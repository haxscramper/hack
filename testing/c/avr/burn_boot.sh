#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

LOAD="-p atmega32u4 -c usbasp"

avrdude $LOAD -B 8 -u -e -U lock:w:0x3F:m -v
avrdude $LOAD -v
avrdude $LOAD -u -U efuse:w:0xFD:m -v
avrdude $LOAD -u -U hfuse:w:0xDC:m -v
avrdude $LOAD -u -U lfuse:w:0xFF:m -v
avrdude $LOAD -U flash:w:ATmegaBOOT_644P.hex -v
avrdude $LOAD -U lock:w:0x0F:m -v

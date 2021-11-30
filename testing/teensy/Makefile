# set your MCU type here, or make command line `make MCU=MK20DX256`
MCU=MK66FX1M0

# make it lower case
LOWER_MCU := $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$(MCU)))))))))))))))))))))))))))
MCU_LD = $(LOWER_MCU).ld





OPTIONS = -DF_CPU=48000000 -DUSB_SERIAL -DLAYOUT_US_ENGLISH -DUSING_MAKEFILE
OPTIONS += -D__$(MCU)__ -DARDUINO=10805 -DTEENSYDUINO=144
CPUARCH = cortex-m4

# Other Makefiles and project templates for Teensy 3.x:
#
# https://github.com/apmorton/teensy-template
# https://github.com/xxxajk/Arduino_Makefile_master
# https://github.com/JonHylands/uCee

ROOT_DIR      ?= $(abspath ../../../../../..)

ARDUINOPATH    = $(ROOT_DIR)/arduino-1.8.15
TOOLSPATH      = $(ARDUINOPATH)/hardware/tools
LIBRARYPATH    = $(ARDUINOPATH)/libraries
COMPILERPATH   = $(ARDUINOPATH)/hardware/tools/arm/bin
TEENSY_LIBS    = $(ARDUINOPATH)/hardware/teensy/avr/libraries
ARDUINO_LIBS   = $(ARDUINOPATH)/hardware/arduino/avr/libraries
EXAMPLE_DIR    = $(ARDUINOPATH)/hardware/teensy/avr/cores/teensy3
DEPS_PATH      = $(ROOT_DIR)/deps

$(info Root is                       $(ROOT_DIR))
$(info Dependency path iss           $(DEPS_PATH))
$(info Arduino installation path is  $(ARDUINOPATH))
$(info Using teensy headers at       $(TEENSY_LIBS))
$(info Tools installation path is    $(TOOLSPATH))
$(info Compiler path is              $(COMPILERPATH))

MAKEFILE_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

CPPFLAGS = -fmax-errors=1 -Wall -g -Os -mcpu=$(CPUARCH) -mthumb -MMD \
  -D__AVR__ \
	$(OPTIONS) \
	-I$(MAKEFILE_DIR) \
	-I$(TEENSY_LIBS)/Wire \
	-I$(TEENSY_LIBS)/SPI \
	-I$(TEENSY_LIBS)/SoftwareSerial \
	-I$(TEENSY_LIBS)/OneWire \
	-I$(DEPS_PATH)/U8g2/src \
	-I$(DEPS_PATH)/DHT \
	-I$(TOOLSPATH)/avr/avr/include/util

CXXFLAGS = -std=gnu++14 -felide-constructors -fno-exceptions -fno-rtti
CFLAGS =

LDFLAGS = -Os -Wl,--gc-sections,--defsym=__rtc_localtime=0 --specs=nano.specs -mcpu=$(CPUARCH) -mthumb -T$(MCU_LD)
LIBS = -lm

CC         = $(COMPILERPATH)/arm-none-eabi-gcc
CXX        = $(COMPILERPATH)/arm-none-eabi-g++
OBJCOPY    = $(COMPILERPATH)/arm-none-eabi-objcopy
SIZE       = $(COMPILERPATH)/arm-none-eabi-size

C_FILES := \
  $(wildcard $(EXAMPLE_DIR)/*.c) \
	$(wildcard $(DEPS_PATH)/U8g2/src/clib/*.c)

CPP_FILES := \
  $(wildcard $(EXAMPLE_DIR)/*.cpp) \
  $(DEPS_PATH)/U8g2/src/U8x8lib.cpp \
	$(DEPS_PATH)/DHT/DHT.cpp \
	$(TEENSY_LIBS)/SPI/SPI.cpp \
  $(TEENSY_LIBS)/Wire/WireKinetis.cpp \
	$(TEENSY_LIBS)/SoftwareSerial/SoftwareSerial.cpp \
	$(TEENSY_LIBS)/OneWire/OneWire.cpp


OBJS := $(CPP_FILES:.cpp=.o) $(C_FILES:.c=.o)

TARGET = main

all: $(TARGET).hex

$(TARGET).elf: $(OBJS) $(MCU_LD)
	@$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

%.hex: %.elf
	$(SIZE) $<
	$(OBJCOPY) -O ihex -R .eeprom $< $@
ifneq (,$(wildcard $(TOOLSPATH)))
	@$(TOOLSPATH)/teensy_post_compile -file=$(basename $@) -path=$(shell pwd) -tools=$(TOOLSPATH)
	-$(TOOLSPATH)/teensy_reboot
endif

#@echo C $<
%.o: %.c
	@$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

#@echo CXX $<
%.o: %.cpp
	@$(CC) -c $(CXXFLAGS) $(CPPFLAGS) $< -o $@


-include $(OBJS:.o=.d)

clean:
	@rm -f $(OBJS) *.d $(TARGET).elf $(TARGET).hex

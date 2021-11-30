#include <Arduino.h>
#include <Wire.h>
#include <U8g2lib.h>
#include <SPI.h>
#include <OneWire.h>
#include <SoftwareSerial.h>
#include <stdlib.h>

const int speed = 140;

OneWire  ds(10);


void setup(void) {
  Serial.begin(9600);
}

void loop(void) {
  byte i;
  byte present = 0;
  byte data[12];
  byte addr[8];

  if ( !ds.search(addr)) {
    Serial.print("No more addresses.\n");
    ds.reset_search();
    delay(250);
    return;
  }

  Serial.print("R=");
  for( i = 0; i < 8; i++) {
    Serial.print(addr[i], HEX);
    Serial.print(" ");
  }

  if ( OneWire::crc8( addr, 7) != addr[7]) {
      Serial.print("CRC is not valid!\n");
      return;
  }

  if ( addr[0] != 0x10) {
      Serial.print("Device is not a DS18S20 family device.\n");
      return;
  }

  // The DallasTemperature library can do all this work for you!

  ds.reset();
  ds.select(addr);
  ds.write(0x44,1);         // start conversion, with parasite power on at the end

  delay(1000);     // maybe 750ms is enough, maybe not
  // we might do a ds.depower() here, but the reset will take care of it.

  present = ds.reset();
  ds.select(addr);
  ds.write(0xBE);         // Read Scratchpad

  Serial.print("P=");
  Serial.print(present,HEX);
  Serial.print(" ");
  for ( i = 0; i < 9; i++) {           // we need 9 bytes
    data[i] = ds.read();
    Serial.print(data[i], HEX);
    Serial.print(" ");
  }
  Serial.print(" CRC=");
  Serial.print( OneWire::crc8( data, 8), HEX);
  Serial.println();
}


U8G2_SSD1306_128X32_UNIVISION_F_HW_I2C u8g2(U8G2_R0);

extern "C" int main(void) {
	// setup();
	// while (1) {
	// 	loop();
	// }
	pinMode(14, OUTPUT);
	u8g2.begin();

	int temp = 0;
	int humidity = 0;

	char tempBuf[7];
	char humidityBuffer[7];

	while (1) {
		digitalWriteFast(14, HIGH);
		delay(speed);
		digitalWriteFast(14, LOW);
		delay(speed);

		u8g2.clearBuffer();
		u8g2.setFont(u8g2_font_logisoso16_tr);


		u8g2.drawStr(0, 16, "temp");
		itoa(temp, tempBuf, 10);
		u8g2.drawStr(16 * 5, 16, tempBuf);

		u8g2.drawStr(0, 32, "humid");
		itoa(humidity, humidityBuffer, 10);
		u8g2.drawStr(16 * 5, 32, humidityBuffer);



		u8g2.sendBuffer();
		delay(speed);

	}
}


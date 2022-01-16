#include <Arduino.h>
#include <DHT.h>
#include <SPI.h>
#include <SoftwareSerial.h>
#include <U8g2lib.h>
#include <Wire.h>
#include <stdlib.h>

const int speed       = 140;
const int dhtPin      = 10;
const int tooHightPin = 14;
const int tooLowPin   = 15;


DHT dht(dhtPin, DHT11);

U8G2_SSD1306_128X32_UNIVISION_F_HW_I2C u8g2(U8G2_R0);


void readEnvironment(int targetHumidity, int targetTemp, int threshold) {
    int humidity = (int)dht.readHumidity();
    int temp     = (int)dht.readTemperature();

    if ((targetHumidity + threshold < humidity)
        || (temp < targetTemp - threshold)) {
        digitalWriteFast(tooHightPin, HIGH);
        digitalWriteFast(tooLowPin, LOW);

    } else if (
        (humidity < targetHumidity - threshold)
        || (targetTemp + threshold < temp)) {
        digitalWriteFast(tooHightPin, LOW);
        digitalWriteFast(tooLowPin, HIGH);

    } else {
        digitalWriteFast(tooHightPin, LOW);
        digitalWriteFast(tooLowPin, LOW);
    }

    u8g2.clearBuffer();
    u8g2.setFont(u8g2_font_logisoso16_tr);


    if (temp < targetTemp - threshold) {
        u8g2.drawStr(0, 16, "temp high");
    } else if (targetTemp + threshold < temp) {
        u8g2.drawStr(0, 16, "temp low");
    } else {
        u8g2.drawStr(0, 16, "temp ok");
    }

    char tempBuf[7];
    char humidityBuffer[7];

    itoa(temp, tempBuf, 10);
    u8g2.drawStr(16 * 6, 16, tempBuf);

    if (targetHumidity + threshold < humidity) {
        u8g2.drawStr(0, 32, "humid high");
    } else if (humidity < targetHumidity - threshold) {
        u8g2.drawStr(0, 32, "humid low");
    } else {
        u8g2.drawStr(0, 32, "humid ok");
    }

    itoa(humidity, humidityBuffer, 10);
    u8g2.drawStr(16 * 6, 32, humidityBuffer);

    u8g2.sendBuffer();
}

void parseCommand(
    const String& cmd,
    int&          humidity,
    int&          temp,
    int&          threshold) {
    if (cmd.startsWith("temp ")) {
        temp = cmd.substring(5, cmd.length() - 1).toInt();
        Serial.print("Set target temperature to '");
        Serial.print(temp, DEC);
        Serial.print("' C");

    } else if (cmd.startsWith("humid ")) {
        humidity = cmd.substring(6, cmd.length() - 1).toInt();
        Serial.print("Set target humidity to '");
        Serial.print(humidity, DEC);
        Serial.print("' %");

    } else if (cmd.startsWith("tolerance ")) {
        threshold = cmd.substring(cmd.length() - 1).toInt();
        Serial.print("Set threshold to '+-");
        Serial.print(threshold, DEC);
        Serial.print("'");

    } else {
        Serial.print("Unexpected input command '");
        Serial.print(cmd);
        Serial.print(
            "' - wanted 'temp NN#', 'tolerance NN' or 'humid NN#'");
    }
}

extern "C" int main(void) {
    int targetHumidity = 40;
    int targetTemp     = 24;
    int threshold      = 2;

    pinMode(tooHightPin, OUTPUT);
    pinMode(tooLowPin, OUTPUT);

    Serial.begin(9600);
    u8g2.begin();
    dht.begin();

    String val;
    while (1) {
        Serial.write(".");
        if (Serial.available()) {
            char command = Serial.read();
            if (command != '\n') {
                val += command;
            }

            if (command == '#') {
                parseCommand(val, targetHumidity, targetTemp, threshold);
                val = "";
            }
        }

        readEnvironment(targetHumidity, targetTemp, threshold);
        delay(speed);
    }
}

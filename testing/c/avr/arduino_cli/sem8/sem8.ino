#include <Adafruit_SSD1306.h>
#include <Arduino.h>
#include <DHT.h>
#include <SPI.h>
#include <SoftwareSerial.h>
#include <Wire.h>
#include <stdlib.h>

// Подключение датчика температуры
const int dhtPin = 10;

// Вывод сигнала "слишком высокое значение"
const int tooHightPin = 9;

// Вывод сигнала "слишком низкое значение"
const int tooLowPin = 8;

// Создание подключение я датчику температуры
DHT dht(dhtPin, DHT11);

// Создание подключение к дисплею
Adafruit_SSD1306 display(128, 32, &Wire, 4);

#define RED "\033[31m"
#define NONE "\033[0m"

void readEnvironment(int targetHumidity, int targetTemp, int threshold) {
    int humidity = (int)dht.readHumidity();
    int temp     = (int)dht.readTemperature();

    Serial.print("temp: ");
    Serial.print(temp);
    Serial.print(", humididty: ");
    Serial.print(humidity);
    Serial.print("\n");

    // Проверка значений температуры и влажности, установка соответсвующих
    // выходных сигналов.
    if ((targetHumidity + threshold < humidity)
        || (targetTemp + threshold < temp)) {
        digitalWrite(tooHightPin, HIGH);
        digitalWrite(tooLowPin, LOW);
    } else if (
        (humidity < targetHumidity - threshold)
        || (temp < targetTemp - threshold)) {
        digitalWrite(tooHightPin, LOW);
        digitalWrite(tooLowPin, HIGH);
    } else {
        digitalWrite(tooHightPin, LOW);
        digitalWrite(tooLowPin, LOW);
    }

    display.clearDisplay();
    // Установка шрифта для дисплея
    // display.setFont(display_font_logisoso16_tr);


    const int SZ = 10;
    display.setCursor(0, SZ);
    // Вывод значения температуры
    if (targetTemp + threshold < temp) {
        display.println(F("temp high"));
    } else if (temp < targetTemp - threshold) {
        display.println(F("temp low"));
    } else {
        display.println(F("temp ok"));
    }

    char tempBuf[7];
    char humidityBuffer[7];

    itoa(temp, tempBuf, 10);
    display.setCursor(SZ * 8, SZ);
    display.println(tempBuf);

    display.setCursor(0, SZ * 2);
    // Вывод значения влажности на дисплей
    if (targetHumidity + threshold < humidity) {
        display.println(F("humid high"));
    } else if (humidity < targetHumidity - threshold) {
        display.println(F("humid low"));
    } else {
        display.println(F("humid ok"));
    }

    itoa(humidity, humidityBuffer, 10);
    display.setCursor(SZ * 8, SZ * 2);
    display.println(humidityBuffer);
    display.display();
}

void parseCommand(
    const String& cmd,
    int&          humidity,
    int&          temp,
    int&          threshold) {
    // Обработка входной комманды управления настройкой
    if (cmd.startsWith("temp ")) {
        // Установка целевой температуры
        temp = cmd.substring(5, cmd.length()).toInt();
        Serial.print("Set target temperature to '");
        Serial.print(temp, DEC);
        Serial.print("' C");

    } else if (cmd.startsWith("humid ")) {
        // Установка целевой влажности
        humidity = cmd.substring(6, cmd.length()).toInt();
        Serial.print("Set target humidity to '");
        Serial.print(humidity, DEC);
        Serial.print("' %");

    } else if (cmd.startsWith("tolerance ")) {
        // Установка диапазона отклонений для измеряемых значений
        threshold = cmd.substring(cmd.length()).toInt();
        Serial.print("Set threshold to '+-");
        Serial.print(threshold, DEC);
        Serial.print("'");

    } else {
        // Вывод сообщения об ошибке
        Serial.print(RED "Unexpected input command '");
        Serial.print(cmd);
        Serial.print(
            "' - wanted '!temp NN!', '!tolerance NN!' or '!humid "
            "NN!'" NONE);
    }
}

// Начальные значение целевой температуры, влажности и диапазона
// отклонений в измеряемых значениях.
int    targetHumidity = 40;
int    targetTemp     = 24;
int    threshold      = 2;
String val;

void setup() {
    pinMode(tooHightPin, OUTPUT);
    pinMode(tooLowPin, OUTPUT);
    Serial.begin(9600);
    display.begin();
    dht.begin();

    // display.setTextSize(2);
    display.setTextColor(SSD1306_WHITE);
}


// Скорость проверки состояния окружающей среды
const int speed = 250;

// Бесконечный цикл проверки состояния окружающей среды
void loop() {
    if (Serial.available()) {
        String val;
        while (0 < Serial.available()) {
            val += (char)Serial.read();
        }
        // Обнаружен конец комманды управления, обрабатываем входные
        // данные
        int start = val.indexOf('!');
        while (start != -1) {
            int    end = val.indexOf('!', start + 1);
            String cmd = val.substring(start + 1, end);
            Serial.print("Found command: ");
            Serial.println(cmd);
            parseCommand(cmd, targetHumidity, targetTemp, threshold);
            int tmp = val.indexOf('!', end + 1);
            if (tmp != -1) {
                start = end;
                end   = tmp;
            } else {
                start = -1;
            }
        }
    }

    // Чтение состояния окружающей среды и установка сообщения на
    // дисплее.
    readEnvironment(targetHumidity, targetTemp, threshold);
    delay(speed);
}

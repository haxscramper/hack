#!/usr/bin/env

wget https://downloads.arduino.cc/arduino-1.8.15-linux64.tar.xz
wget https://www.pjrc.com/teensy/td_154/TeensyduinoInstall.linux64
wget https://www.pjrc.com/teensy/00-teensy.rules
sudo cp 00-teensy.rules /etc/udev/rules.d/
tar -xf arduino-1.8.15-linux64.tar.xz
chmod 755 TeensyduinoInstall.linux64
./TeensyduinoInstall.linux64 --dir=arduino-1.8.15

mkdir deps
cd deps
git clone https://github.com/adafruit/DHT-sensor-library.git DHT
wget https://github.com/olikraus/U8g2_Arduino/archive/master.zip
unzip master.zip
mv U8g2_Arduino-master/ u8g2

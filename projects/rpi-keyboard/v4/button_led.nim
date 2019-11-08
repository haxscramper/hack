import wiringPiNim, os
import common

if piSetup() >= 0:
  echo "Pi setup ok"
else:
  echo "Pi setup failed"
  quit 1

const buttonOutPin = 0
const buttonInPin = 1
const ledOutPin = 2

setPinModeOut(buttonOutPin)
setPinModeOut(ledOutPin)
setPinModeIn(buttonInPin)

while true:
  digitalWrite(buttonOutPin, true)
  sleep(5)

  digitalWrite(ledOutPin, digitalRead(buttonInPin))

  sleep(300)


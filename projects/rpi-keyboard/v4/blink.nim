import wiringPiNim, os

# Setting the pin for a LED
const led = 0

proc flashLed() =
  if piSetup() >= 0:
    # Setting the LEDs pin to output
    piPinModeOutput(led)
    echo "RPi is setup and ready for use"

    while true:
      piDigitalWrite(led, 1)
      sleep(800)

      piDigitalWrite(led, 0)
      sleep(800)

flashLed()

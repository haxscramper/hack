import RPi.GPIO as GPIO 
GPIO.setwarnings(False) 
GPIO.setmode(GPIO.BOARD)
import time

pin_num =  16

GPIO.setup(pin_num, GPIO.IN, pull_up_down=GPIO.PUD_DOWN) 

finished = False

import main_keyboard

while not finished:
    if GPIO.input(pin_num) == GPIO.HIGH:
        time.sleep(2)
        if GPIO.input(pin_num) == GPIO.LOW:
            print "Starting keyboard"
            reload(main_keyboard)
        elif GPIO.input(pin_num) == GPIO.HIGH:
            finished = True


print "Shutting down"



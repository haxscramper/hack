import RPi.GPIO as GPIO # Import Raspberry Pi GPIO library
GPIO.setwarnings(False) # Ignore warning for now
GPIO.setmode(GPIO.BOARD) # Use physical pin numbering
GPIO.setup(10, GPIO.IN, pull_up_down=GPIO.PUD_DOWN) 


already_pressed = False

while True: # Run forever
    button_state = GPIO.input(10)
    if button_state == GPIO.HIGH and not already_pressed:
        print("Button was pushed!")
        already_pressed = True
    elif button_state == GPIO.LOW and already_pressed:
        print("Button was released!")
        already_pressed = False

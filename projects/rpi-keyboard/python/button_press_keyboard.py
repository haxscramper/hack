import RPi.GPIO as GPIO # Import Raspberry Pi GPIO library
GPIO.setwarnings(False) # Ignore warning for now
GPIO.setmode(GPIO.BOARD) # Use physical pin numbering
GPIO.setup(10, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

col_pins = [33, 35, 37]
row_pins = [36, 38, 40]

already_pressed = {}

for row in row_pins:
    already_pressed[row] = {}
    for col in col_pins:
        already_pressed[row][col] = False

def scan_keys():
    for pin in row_pins:
        GPIO.setup(pin, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

    for col in col_pins:
        GPIO.setup(col, GPIO.OUT)
        GPIO.output(col, GPIO.HIGH)

        for row in row_pins:
            if GPIO.input(row) == GPIO.HIGH and not already_pressed[row][col]:
                print "Button", row, col
                already_pressed[row][col] = True
            elif GPIO.input(row) == GPIO.LOW and already_pressed[row][col]:
                already_pressed[row][col] = False

        GPIO.output(col, GPIO.LOW)

while True:
    scan_keys()

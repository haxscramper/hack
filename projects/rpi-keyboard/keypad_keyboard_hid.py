import RPi.GPIO as GPIO # Import Raspberry Pi GPIO library
GPIO.setwarnings(False) # Ignore warning for now
GPIO.setmode(GPIO.BOARD) # Use physical pin numbering
GPIO.setup(10, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
from collections import defaultdict
import json
from pprint import pprint



# Loading config
def get_zero():
    return chr(0)


def get_zero_key():
    return {
        "name" : "none",
        "code" : chr(0),
        "first_repeat_after" : -1,
        "repeat_after" : -1
        }

def get_zero_dict():
    return defaultdict(get_zero_key)

def _decode_list(data):
    rv = []
    for item in data:
        if isinstance(item, unicode):
            item = item.encode('utf-8')
        elif isinstance(item, list):
            item = _decode_list(item)
        elif isinstance(item, dict):
            item = _decode_dict(item)
        rv.append(item)
    return rv

def _decode_dict(data):
    rv = {}
    for key, value in data.iteritems():
        if isinstance(key, unicode):
            key = key.encode('utf-8')
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        elif isinstance(value, list):
            value = _decode_list(value)
        elif isinstance(value, dict):
            value = _decode_dict(value)
        rv[key] = value
    return rv

keyboard_settings = json.loads(
        open("keymap.json", "r").read(),
        object_hook = _decode_dict
        )

button_mapping = keyboard_settings["mappings"]

logical_to_physical = json.loads(
        open("logical_to_physical.json", "r").read(), 
        object_hook=_decode_dict
        )

phys_row = logical_to_physical["rows"]
phys_col = logical_to_physical["cols"]

row_pins = [int(val) for key, val in phys_row.items()]
col_pins = [int(val) for key, val in phys_col.items()]

phys_row = {key : int(val) for key, val in phys_row.items()}
phys_col = {key : int(val) for key, val in phys_col.items()}

modifier_keys = keyboard_settings["settings"]["modifier_keys"]
for key, settings in modifier_keys.items():
    settings["row"] = phys_row[settings["row"]]
    settings["col"] = phys_col[settings["col"]]

char_keymap = defaultdict(get_zero_dict)

for row in button_mapping:
    def_dict = defaultdict(get_zero_key)

    for col in button_mapping[row]:
        def_dict[phys_col[col]]["code"] = chr(button_mapping[row][col]["code"])
        def_dict[phys_col[col]]["name"] = button_mapping[row][col]["name"]
        if "repeat_after" in button_mapping[row][col]:
            def_dict[phys_col[col]]["repeat_after"] \
                    = button_mapping[row][col]["repeat_after"]
            def_dict[phys_col[col]]["first_repeat_after"] \
                    = button_mapping[row][col]["first_repeat_after"]
        else:
            def_dict[phys_col[col]]["first_repeat_after"] = -1
            def_dict[phys_col[col]]["repeat_after"] = -1
    
    char_keymap[phys_row[row]] = def_dict


# Settng up buttons
already_pressed = {}
NULL_CHAR = chr(0)

for row in row_pins:
    already_pressed[row] = {}
    for col in col_pins:
        already_pressed[row][col] = 0 


# Write control functions
def write_report(report):
    with open('/dev/hidg0', 'rb+') as fd:
        fd.write(report.encode())


def release_keys():
    write_report(NULL_CHAR*8)


def get_modifiers():
    result = 0
    
    if already_pressed                 \
        [modifier_keys["ctrl"]["row"]] \
        [modifier_keys["ctrl"]["col"]]:
        result = result | ( 1 )

    if already_pressed                 \
       [modifier_keys["shift"]["row"]] \
       [modifier_keys["shift"]["col"]]:
       result = result | ( 1 << 1 )

    if already_pressed               \
       [modifier_keys["alt"]["row"]] \
       [modifier_keys["alt"]["col"]]:
       result = result | ( 1 << 2 )

    if already_pressed                \
       [modifier_keys["meta"]["row"]] \
       [modifier_keys["meta"]["col"]]:
       result = result | ( 1 << 3 )

    return chr(result)


def get_keys(row, col):
    char_key_code = char_keymap[row][col]["code"]
    modifier_key_code = get_modifiers();
    return modifier_key_code + NULL_CHAR + char_key_code + NULL_CHAR*5

def scan_keys():
    state_changed = False
    for pin in row_pins:
        GPIO.setup(pin, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)

    for col in col_pins:
        GPIO.setup(col, GPIO.OUT)
        GPIO.output(col, GPIO.HIGH)

        for row in row_pins:
            repeat_after = char_keymap[row][col]["repeat_after"]
            first_repeat_after = char_keymap[row][col]["first_repeat_after"]

            press_count = already_pressed[row][col]
            press_required = press_count == 0
            press_continues = first_repeat_after == press_count

            if GPIO.input(row) == GPIO.HIGH:
                already_pressed[row][col] = press_count + 1
                if press_required or press_continues:
                    write_report(get_keys(row, col))
                    release_keys()
                    state_changed = True
            elif GPIO.input(row) == GPIO.LOW and not press_required:
                already_pressed[row][col] = 0

            if press_count >= first_repeat_after + repeat_after \
                    and first_repeat_after != -1:
                already_pressed[row][col] = first_repeat_after

            elif press_count >= repeat_after and repeat_after != -1 and first_repeat_after == -1:
                already_pressed[row][col] = 0

        GPIO.output(col, GPIO.LOW)


while True:
    scan_keys()

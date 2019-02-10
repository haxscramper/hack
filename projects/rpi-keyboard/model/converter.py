#!/usr/bin/env python
import json
from typing import *


class Key:
    def __init__(self):
        self.width = 1
        self.height = 1
        self.x_pos = 0
        self.y_pos = 0


class Row:
    def __init__(self):
        self.keys: List[Key] = []
        self.rotation_x = 0
        self.rotation_y = 0
        self.rotation_angle = 0
        self.pos_x = 0
        self.pos_y = 0


def json2keyboard(path: str) -> List[Row]:
    model_json = json.loads(open(path).read())
    keyboard: List[Row] = []

    key_count = 0
    current_y = 0
    current_rot = 0
    current_rot_x = 0
    current_rot_y = 0

    for kbd_row in model_json[1:]:
        row: Row = Row()

        # X position is reset after each row
        row.pos_x = kbd_row[0]["x"]

        # Y position is incremented
        if "y" in kbd_row[0]:
            current_y = current_y + kbd_row[0]["y"]
        else:
            current_y = current_y + 1

        row.pos_y = current_y

        # Rotation is carried on, not incremented
        if "r" in kbd_row[0]:
            current_rot = current_rot + kbd_row[0]["r"]

        row.rotation_angle = current_rot

        # Rotation center is carried on, not incremented
        if "rx" in kbd_row[0]:
            current_rot_x = kbd_row[0]["rx"]

        row.rotation_x = current_rot_x

        if "ry" in kbd_row[0]:
            current_rot_y = kbd_row[0]["ry"]

        row.rotation_y = current_rot_y

        idx = 1
        while idx < len(kbd_row[1:]):
            key: Key = Key()

            # Key description is omitted
            if (isinstance(kbd_row[idx], str) and
                (idx + 1 >= len(kbd_row) or \
                 isinstance(kbd_row[idx + 1], str))):

                idx = idx + 1

            # Key description is availiable
            else:
                json_key = kbd_row[idx + 1]
                idx = idx + 2
                if "w" in json_key:
                    key.width = json_key["w"]

                if "h" in json_key:
                    key.width = json_key["h"]

            key_count = key_count + 1
            row.keys.append(key)

        keyboard.append(row)

    print(key_count)

    return keyboard


keyboard: List[Row] = json2keyboard("raw_data.json")

with open("keyboard.scad", "w+") as file:
    file.write("include <keyboard_lib.scad>;\n\n\n")
    row_num: int = 1
    for row in keyboard:
        start_x: int = row.pos_x
        start_y: int = row.pos_y

        keylist: List[List[int]] = []

        for key in row.keys:
            keylist.append([start_x, start_y, key.width, key.height])
            start_x = start_x + key.width

        file.write("// Row num {}\n".format(row_num))
        file.write(
            "row({}, {}, {});\n\n".format(keylist, 90 - row.rotation_angle,
                                          [row.rotation_x, row.rotation_y, 0]))

        row_num = row_num + 1

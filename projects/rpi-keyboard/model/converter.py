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

    for  json_row in model_json["rows"]:
        row: Row = Row()

        if "r" in json_row:
            row.rotation_angle = json_row["r"]

        if "rx" in json_row:
            row.rotation_x = json_row["rx"]

        if "ry" in json_row:
            row.rotation_y = json_row["y"]

        row.pos_x = json_row["x"]
        row.pos_y = json_row["y"]

        current_x = 0

        for json_key in json_row["keys"]:
            key: Key = Key()

            if "w" in json_key:
                key.width = json_key["w"]

            key.x_pos = current_x

            current_x = current_x+ key.width

            if "h" in json_key:
                key.height = json_key["h"]

            row.keys.append(key)

        keyboard.append(row)


    return keyboard


keyboard: List[Row] = json2keyboard("kbd_layout.json")

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
        file.write("row({}, {}, {});\n\n".format(
            keylist, row.rotation_angle, [row.rotation_x, row.rotation_y, 0]))

        row_num = row_num + 1

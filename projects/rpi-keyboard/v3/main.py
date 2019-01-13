#!/usr/bin/env python

# Temporary disabled
# import RPi.GPIO as GPIO  # Import Raspberry Pi GPIO library
# GPIO.setwarnings(False)  # Ignore warning for now
# GPIO.setmode(GPIO.BOARD)  # Use physical pin numbering
# GPIO.setup(10, GPIO.IN, pull_up_down=GPIO.PUD_DOWN)
from collections import defaultdict
import json
from pprint import pprint

from enum import Enum
from typing import Dict, List, Optional, MutableMapping, Tuple
from copy import deepcopy
from itertools import product


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


class StateChange(Enum):
    Changedpressed = 1
    Changedreleased = 2
    Changedrepeat = 3
    Idlepresed = 4
    Idlereleased = 5


class Key:
    def __init__(self, _row: int, _col: int):
        self.row: int = _row
        self.column: int = _col
        self.state_change: StateChange = StateChange.Idlereleased
        self.is_valid: bool = False
        self.repetitions: Optional[List[int]] = None


class Keypad:
    def __init__(self):
        self.rows: MutableMapping[int, int] = {}
        self.cols: MutableMapping[int, int] = {}
        self.keymap: List[List[Key]] = [[]]
        self.pressed: MutableMapping[Key, int] = {}
        self._test_pressed: List[Tuple[int, int]] = []

    def scan(self) -> List[Key]:
        active: List[Key] = []  # List ok keys that physically pressed

        # Scan button grid
        print(" -> Scanning button grid")
        # Test code
        active = active + [
            self.keymap[row][col] for row, col in self._test_pressed
        ]

        # Work code
        for logic_col, physic_col in self.cols.items():
            for logic_row, physic_row in self.rows.items():
                key: Key = self.keymap[logic_row][logic_col]
                if not key.is_valid:
                    continue

                gpio_high: bool = False
                if gpio_high:
                    active.append(key)

        print(" -> Active keys")
        for key in active:
            print("    ", key.column, key.row)

        res: List[Key] = []  # List of keys that changed between scans

        for key, _ in self.pressed.items():
            if not key in active:
                key.state_change = StateChange.Changedreleased
                res.append(key)

        for key in res:
            self.pressed.pop(key)

        # Process each key that is currently active and keys that
        # have been deactivated
        for key in active:
            press_count: int = 0
            if key in self.pressed:
                press_count = self.pressed[key]

            if press_count == 0:
                key.state_change = StateChange.Changedpressed
                press_count = press_count + 1
            else:
                press_count = 0
                key.state_change = StateChange.Changedreleased

            res.append(key)
            self.pressed[key] = press_count

        print("")

        self._test_pressed = []
        return res

    def mapping_from_json(self, path: str) -> None:
        pass


class PressedModifiers:
    def __init__(self):
        self.shift: bool = False
        self.ctrl: bool = False
        self.meta: bool = False
        self.super: bool = False
        self.hyper: bool = False

        self.right_shift: bool = False
        self.right_ctrl: bool = False
        self.right_meta: bool = False
        self.right_super: bool = False

    def logical_or_pressed(self, other):
        self.shift = self.shift or other.shift
        self.ctrl = self.ctrl or other.ctrl
        self.meta = self.meta or other.meta
        self.super = self.super or other.super
        self.hyper = self.hyper or other.hyper

        self.right_shift = self.right_shift or other.right_shift
        self.right_ctrl = self.right_ctrl or other.right_ctrl
        self.right_meta = self.right_meta or other.right_meta
        self.right_super = self.right_super or other.right_super


class KeyAction:
    def __init__(self, _default_code: int):
        self.name: str = "none"
        self.defaultCode: int = _default_code
        self.modified_codes: MutableMapping[PressedModifiers, int] = {}
        self.modifiers: PressedModifiers = PressedModifiers()

    def get_key_code(self, modifiers: PressedModifiers) -> int:
        return self.defaultCode


class HidReport:
    def __init__(self):
        self.mod_l_shift: bool = False
        self.mod_l_ctrl: bool = False
        self.mod_l_meta: bool = False
        self.mod_l_super: bool = False

        self.mod_r_shift: bool = False
        self.mod_r_ctrl: bool = False
        self.mod_r_meta: bool = False
        self.mod_r_super: bool = False

        self.report_codes: List[int] = []


class ActionResolver:
    def __init__(self):
        self.actions: MutableMapping[Tuple[int, int], KeyAction] = {}
        self.pressed_modifiers: MutableMapping[Tuple[int, int],
                                               PressedModifiers] = {}
        self.external_modifiers: PressedModifiers = PressedModifiers()

    def config_from_json(self) -> None:
        print("Parsing config for action resolver")
        config = json.loads(
            open("keymap.json", "r").read(), object_hook=_decode_dict)

        mappings = config["mappings"]
        for row, val in mappings.items():
            for col, val in mappings[row].items():
                modifiers: PressedModifiers = PressedModifiers()

                if "modifiers" in val:
                    for mod_key in val["modifiers"]:
                        if mod_key == "ctrl":
                            modifiers.ctrl = True
                        elif mod_key == "shift":
                            modifiers.shift = True
                        elif mod_key == "meta":
                            modifiers.meta = True
                        elif mod_key == "super":
                            modifiers.hyper = True
                        elif mod_key == "hyper":
                            modifiers.hyper = True
                        # TODO Add right modifiers

                action: KeyAction = KeyAction(int(val["code"]))
                action.name = val["name"]
                action.modified_codes = {}
                action.modifiers = PressedModifiers()

    def get_active_keys(self, keys: List[Key]) -> List[int]:
        print("--- Determine which codes to send")
        changed_modifiers: List[Key] = []
        for phys_key in keys:
            pos: Tuple[int, int] = (phys_key.row, phys_key.column)
            print(" -> Key at", pos, "changed state")
            if not pos in self.actions:
                print("    No actions for pos", pos)
                continue

            action = self.actions[pos]

            if phys_key.state_change == StateChange.Changedpressed:
                print("    New state: pressed")
                self.pressed_modifiers[pos] = action.modifiers
            elif phys_key.state_change == StateChange.Changedreleased:
                print("    New state: released")
                if pos in self.pressed_modifiers:
                    self.pressed_modifiers.pop(pos)

        key_modifiers: PressedModifiers = deepcopy(self.external_modifiers)
        for key, val in self.pressed_modifiers.items():
            key_modifiers.logical_or_pressed(val)

        result_codes: List[int] = []

        for key in keys:
            pos: Tuple[int, int] = (key.row, key.column)
            if not pos in self.actions:
                continue

            action: KeyAction = self.actions[pos]
            if key.state_change == StateChange.Changedpressed or \
               key.state_change == StateChange.Changedrepeat:
                result_codes.append(action.get_key_code(key_modifiers))

        return result_codes

    def get_modifiers(self) -> PressedModifiers:
        res: PressedModifiers = deepcopy(self.external_modifiers)
        for pos, val in self.pressed_modifiers.items():
            res.logical_or_pressed(val)

        return res



central_pressed: List[List[Tuple[int,int]]] = [
    [(0,0), (0,1), (0,3), (5,4)],
    [(0,0), (0,1)],
    [(0,0), (0,3), (5,4)],
    [(0,0), (0,1), (0,3), (5,4)],
    [(0,3), (5,4)],
    [(3,4)]
] # yapf: disable

numpad_pressed: List[List[Tuple[int, int]]] = [
    [(3,4)],
    [(3,4), (6,5)],
    [(5,3), (3,4)],
    [(4,5)],
    [(4,5)],
    [(4,5)]
] # yapf: disable


def main_loop():
    central_keypad = Keypad()
    numpad_keypad = Keypad()
    central_resolver = ActionResolver()
    keypad_resolver = ActionResolver()

    central_keypad.keymap = [[Key(x, y) for x in range(100)]
                             for y in range(100)]

    numpad_keypad.keymap = [[Key(x, y) for x in range(100)]
                            for y in range(100)]

    central_resolver.actions = {(x, y): KeyAction(100 * x + y)
                                for x, y in product(range(100), range(100))}

    keypad_resolver.actions = {(x, y): KeyAction(100 * x + y)
                               for x, y in product(range(100), range(100))}

    for central, numpad in zip(central_pressed, numpad_pressed):
        print("=== ===")
        central_keypad._test_pressed = central
        numpad_keypad._test_pressed = numpad

        print("--- Central keypad")
        central_changes = central_keypad.scan()
        active_keys = central_resolver.get_active_keys(central_changes)
        keypad_resolver.external_modifiers = central_resolver.get_modifiers()

        print("--- Numpad keypad")
        numpad_changes = numpad_keypad.scan()
        active_keys = active_keys + keypad_resolver.get_active_keys(
            numpad_changes)
        modifier_keys = keypad_resolver.get_modifiers()

        print (" -> After current scan sending codes")
        for code in active_keys:
            print(code)

        print("")


if __name__ == '__main__':
    main_loop()

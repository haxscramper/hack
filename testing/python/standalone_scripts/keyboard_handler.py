#!/usr/bin/env python

# 1) Add the user to the input group `sudo usermod -aG input <user>`
# 2) Re-loging
# 3) List the input devices `ls -l /dev/input/by-id/*-event-kbd`
# 4) `evtest /dev/input/by-id/usb-1189_USB_Composite_Device_CD70134330393538-if01-event-kbd` to know
#    which events the keyboard emits
# 5) configure the script as necessary

import subprocess
import sys

from evdev import InputDevice, UInput, ecodes

device = InputDevice(sys.argv[1])

remap = {
    ecodes.KEY_M: ecodes.KEY_PLAYPAUSE,
    ecodes.KEY_O: ecodes.KEY_MUTE,
}


def run_custom_action() -> None:
    subprocess.Popen(["wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%+"])


actions = {
    ecodes.KEY_F13: run_custom_action,
}

output_capabilities = {
    ecodes.EV_KEY: sorted(set(remap.values())),
}

with UInput(output_capabilities, name="custom-external-keyboard") as output:
    with device.grab_context():
        for event in device.read_loop():
            if event.type != ecodes.EV_KEY:
                continue

            action = actions.get(event.code)
            if action is not None:
                if event.value == 1:
                    action()
                continue

            output_code = remap.get(event.code)
            if output_code is not None:
                output.write(ecodes.EV_KEY, output_code, event.value)
                output.syn()

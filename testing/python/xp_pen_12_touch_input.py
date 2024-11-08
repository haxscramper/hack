#!/usr/bin/env python3
import evdev
from evdev import InputDevice, categorize, ecodes
import asyncio
import sys

def print_capabilities(device):
    """
    Print device capabilities in a readable format.
    """
    print("Device capabilities:")
    for ev_type, ev_codes in device.capabilities().items():
        ev_type_str = evdev.ecodes.EV[ev_type]
        print(f"\nEvent type: {ev_type_str}")
        
        # Handle absolute axes differently
        if ev_type == evdev.ecodes.EV_ABS:
            for code in ev_codes:
                # For absolute axes, we get a tuple of (code, AbsInfo)
                code_name = evdev.ecodes.ABS[code[0]]
                print(f"  {code_name}:")
                print(f"    Min: {code[1].min}")
                print(f"    Max: {code[1].max}")
                print(f"    Fuzz: {code[1].fuzz}")
                print(f"    Flat: {code[1].flat}")
        else:
            # Handle other event types
            for code in ev_codes:
                if ev_type in (evdev.ecodes.EV_LED, evdev.ecodes.EV_SND, evdev.ecodes.EV_KEY):
                    try:
                        code_name = evdev.ecodes.keys[code]
                        print(f"  {code_name}")
                    except KeyError:
                        print(f"  Unknown code {code}")
                else:
                    print(f"  Code {code}")

def find_touch_bar_device():
    """
    Attempt to find the touch bar input device among all input devices.
    Returns the device path if found, None otherwise.
    """
    devices = [evdev.InputDevice(path) for path in evdev.list_devices()]
    for device in devices:
        # XP-Pen devices often have "TABLET" or the model number in their name
        if "XP-PEN" in device.name.upper() or "TABLET" in device.name.upper() or "UGTABLET" in device.name.upper():
            print(f"Found potential tablet device: {device.name}")
            print_capabilities(device)
            return device.path
    return None

async def monitor_touch_bar(device_path):
    """
    Monitor the touch bar input events and print them for analysis.
    """
    try:
        device = InputDevice(device_path)
        print(f"\nMonitoring touch bar events from: {device.name}")
        print("Press Ctrl+C to stop")

        async for event in device.async_read_loop():
            # Print all events with their decoded names when possible
            if event.type == ecodes.EV_ABS:
                try:
                    code_name = evdev.ecodes.ABS[event.code]
                    print(f"Absolute axis event - {code_name}: {event.value}")
                except KeyError:
                    print(f"Absolute axis event - code {event.code}: {event.value}")
            elif event.type == ecodes.EV_KEY:
                try:
                    code_name = evdev.ecodes.keys[event.code]
                    print(f"Key event - {code_name}: {event.value}")
                except KeyError:
                    print(f"Key event - code {event.code}: {event.value}")
            elif event.type == ecodes.EV_MSC:
                print(f"Misc event - code: {event.code}, value: {event.value}")

    except PermissionError:
        print("Permission denied. Try running with sudo or add user to input group:")
        print("sudo usermod -a -G input $USER")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

def main():
    # Find the touch bar device
    device_path = find_touch_bar_device()
    if not device_path:
        print("Could not find XP-Pen tablet device")
        sys.exit(1)

    # Run the monitoring loop
    try:
        asyncio.run(monitor_touch_bar(device_path))
    except KeyboardInterrupt:
        print("\nMonitoring stopped")

if __name__ == "__main__":
    main()

from enum import StrEnum


class DeviceKind(StrEnum):
    STREAMDECK = "streamdeck"
    EVDEV = "evdev"


class LayerDirection(StrEnum):
    FORWARD = "forward"
    BACKWARD = "backward"

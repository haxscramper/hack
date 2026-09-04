from pathlib import Path
from beartype import beartype
from beartype.typing import List
from StreamDeck.DeviceManager import DeviceManager
from StreamDeck.Devices.StreamDeck import StreamDeck
import yaml

from custom_input.application import ApplicationConfig
from custom_input.devices.device import InputDevice
from custom_input.devices.device_schema import EvdevDeviceConfig, StreamDeckDeviceConfig
from custom_input.devices.evdev_device import EvdevInputDevice
from custom_input.devices.streamdeck_device import StreamDeckInputDevice


@beartype
def load_devices(config_path: Path) -> List[InputDevice]:
    resolved_path = config_path.expanduser().resolve()
    with resolved_path.open("r") as file:
        data = yaml.safe_load(file)

    config = ApplicationConfig.model_validate(data)
    config_directory = resolved_path.parent
    devices: List[InputDevice] = []
    streamdeck_configs = [
        device for device in config.devices
        if isinstance(device, StreamDeckDeviceConfig)
    ]
    streamdecks: List[StreamDeck] = DeviceManager().enumerate()
    used_streamdecks: set[int] = set()

    for streamdeck in streamdecks:
        streamdeck.open()

    for device_config in config.devices:
        match device_config:
            case EvdevDeviceConfig():
                devices.append(
                    EvdevInputDevice.create(
                        device_config,
                        config_directory,
                    ))
            case StreamDeckDeviceConfig():
                matching_index = find_streamdeck(
                    device_config,
                    streamdecks,
                    used_streamdecks,
                )
                used_streamdecks.add(matching_index)
                devices.append(
                    StreamDeckInputDevice.create(
                        device_config,
                        streamdecks[matching_index],
                        config_directory,
                    ))

    for index, streamdeck in enumerate(streamdecks):
        if index not in used_streamdecks:
            streamdeck.close()

    configured_identifiers = [device.identifier for device in devices]
    duplicate_identifiers = sorted({
        identifier
        for identifier in configured_identifiers
        if configured_identifiers.count(identifier) != 1
    })
    if duplicate_identifiers:
        raise ValueError(
            f"Input configuration has duplicate device identifiers: "
            f"{duplicate_identifiers}")

    return devices


@beartype
def find_streamdeck(
    config: StreamDeckDeviceConfig,
    decks: List[StreamDeck],
    used_indices: set[int],
) -> int:
    available: List[str] = []

    for index, deck in enumerate(decks):
        model = deck.deck_type()
        serial = deck.get_serial_number()
        available.append(f"{model} serial={serial}")

        if index in used_indices:
            continue

        model_matches = model == config.model
        serial_matches = (config.serial is None or serial == config.serial)
        if model_matches and serial_matches:
            return index

    raise ValueError(f"No StreamDeck matches device {config.id!r}, model "
                     f"{config.model!r}, serial {config.serial!r}. "
                     f"Available devices: {available}")

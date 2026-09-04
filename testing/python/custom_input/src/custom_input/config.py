from pathlib import Path
from beartype import beartype
from loguru import logger
from beartype.typing import List
from StreamDeck.DeviceManager import DeviceManager
from StreamDeck.Devices.StreamDeck import StreamDeck

from custom_input.actions.action import ActionCreateContext
from custom_input.application import ApplicationConfig, DeviceConfig
from custom_input.config_loader import load_yaml
from custom_input.devices.device import InputDevice
from custom_input.devices.evdev_device import EvdevDeviceConfig, EvdevInputDevice
from custom_input.devices.streamdeck_device import StreamDeckDeviceConfig, StreamDeckInputDevice


def init_devices(
    config: ApplicationConfig,
    config_path: Path,
    streamdecks: List[StreamDeck],
    used_streamdecks: set[int],
) -> List[InputDevice]:
    action_create_context = ActionCreateContext(config_dir=config_path.parent)

    devices: List[InputDevice] = list()

    for device_config in config.devices:
        match device_config:
            case EvdevDeviceConfig():
                devices.append(
                    EvdevInputDevice.create(
                        device_config,
                        action_create_context,
                    ))
            case StreamDeckDeviceConfig():
                matching_index = find_streamdeck(
                    device_config,
                    streamdecks,
                    used_streamdecks,
                )

                if matching_index is None:
                    continue

                used_streamdecks.add(matching_index)
                devices.append(
                    StreamDeckInputDevice.create(
                        device_config,
                        streamdecks[matching_index],
                        action_create_context,
                    ))

    return devices


@beartype
def load_devices(config_path: Path) -> List[InputDevice]:
    config = load_yaml(ApplicationConfig, config_path)
    devices: List[InputDevice] = []
    streamdecks: List[StreamDeck] = DeviceManager().enumerate()
    used_streamdecks: set[int] = set()

    for streamdeck in streamdecks:
        streamdeck.open()
        streamdeck.reset()

    try:
        devices = init_devices(
            config,
            config_path=config_path,
            streamdecks=streamdecks,
            used_streamdecks=used_streamdecks,
        )

    except Exception as e:
        for deck in streamdecks:
            deck.close()

        raise e from None

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
) -> int | None:
    available: List[str] = []

    for index, deck in enumerate(decks):
        model = deck.deck_type()
        serial = deck.get_serial_number()
        available.append(f"model={model!r} serial={serial!r}")

        if index in used_indices:
            continue

        model_matches = model == config.model
        serial_matches = (config.serial is None or serial == config.serial)
        if model_matches and serial_matches:
            return index

    logger.warning(f"No StreamDeck matches device {config.id!r}, model "
                   f"{config.model!r}, serial {config.serial!r}. "
                   f"Available devices: {available}")

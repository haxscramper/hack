from pathlib import Path
from beartype import beartype
from beartype.typing import Iterator, List
from evdev import InputDevice as EvdevDevice
from evdev import ecodes

from custom_input.action_factory import create_action
from custom_input.devices.device import (
    DeviceBinding,
    DeviceEvent,
    DeviceLayer,
    InputControl,
    InputDevice,
)
from custom_input.schema import EvdevDeviceConfig


@beartype
def resolve_evdev_code(value: int | str, field_name: str) -> int:
    if isinstance(value, int):
        return value

    resolved = ecodes.ecodes.get(value)
    if resolved is None:
        raise ValueError(f"Unknown evdev {field_name} name {value!r}")

    if not isinstance(resolved, int):
        raise ValueError(
            f"Evdev {field_name} name {value!r} resolves to multiple "
            f"codes: {resolved}")

    return resolved


@beartype
class EvdevInputDevice(InputDevice):

    def __init__(
        self,
        config: EvdevDeviceConfig,
        device: EvdevDevice,
        layers: List[DeviceLayer],
    ) -> None:
        super().__init__(config.id, layers)
        self.device = device
        self.grabbed = config.grab

        if self.grabbed:
            self.device.grab()

    @classmethod
    def create(
        cls,
        config: EvdevDeviceConfig,
        config_directory: Path,
    ) -> "EvdevInputDevice":
        path = (config.path if config.path.is_absolute() else
                config_directory / config.path).expanduser().resolve()

        layers: List[DeviceLayer] = []
        for layer_config in config.layers:
            bindings = [
                DeviceBinding(
                    control=InputControl(
                        event_type=resolve_evdev_code(
                            binding.event_type,
                            "event type",
                        ),
                        code=resolve_evdev_code(
                            binding.code,
                            "event code",
                        ),
                    ),
                    values=binding.values,
                    action=create_action(
                        binding.action,
                        config_directory,
                    ),
                ) for binding in layer_config.bindings
            ]
            layers.append(
                DeviceLayer(
                    name=layer_config.name,
                    bindings=bindings,
                ))

        return cls(
            config=config,
            device=EvdevDevice(path),
            layers=layers,
        )

    def events(self) -> Iterator[DeviceEvent]:
        for event in self.device.read_loop():
            yield DeviceEvent(
                control=InputControl(
                    event_type=event.type,
                    code=event.code,
                ),
                value=event.value,
            )

    def close(self) -> None:
        if self.grabbed:
            self.device.ungrab()

        self.device.close()

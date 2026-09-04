from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import StrEnum
from threading import Lock
from beartype import beartype
from beartype.typing import Any, Iterator, List, Optional, Set


@dataclass(frozen=True)
class InputControl:
    event_type: int
    code: int


@dataclass(frozen=True)
class DeviceEvent:
    control: InputControl
    value: int


@dataclass(frozen=True)
class DeviceBinding:
    control: InputControl
    values: Set[int]
    action: Any


class LayerDirection(StrEnum):
    FORWARD = "forward"
    BACKWARD = "backward"


@dataclass(frozen=True)
class DeviceLayer:
    name: str
    bindings: List[DeviceBinding]


@beartype
class InputDevice(ABC):

    def __init__(self, identifier: str, layers: List[DeviceLayer]) -> None:
        if not layers:
            raise ValueError(
                f"Input device {identifier!r} must define at least one layer")

        layer_names = [layer.name for layer in layers]
        duplicate_names = sorted(
            {name
             for name in layer_names if layer_names.count(name) != 1})
        if duplicate_names:
            raise ValueError(
                f"Input device {identifier!r} has duplicate layer names: "
                f"{duplicate_names}")

        self.identifier = identifier
        self.layers = layers
        self.layer_index = 0
        self.layer_lock = Lock()

    @abstractmethod
    def events(self) -> Iterator[DeviceEvent]:
        raise NotImplementedError

    @abstractmethod
    def close(self) -> None:
        raise NotImplementedError

    def action_for(self, event: DeviceEvent) -> Optional[Any]:
        with self.layer_lock:
            layer = self.layers[self.layer_index]

        for binding in layer.bindings:
            if binding.control == event.control and event.value in binding.values:
                return binding.action

        return None

    def change_layer(self, direction: LayerDirection) -> None:
        with self.layer_lock:
            match direction:
                case LayerDirection.FORWARD:
                    self.layer_index = (self.layer_index + 1) % len(
                        self.layers)
                case LayerDirection.BACKWARD:
                    self.layer_index = (self.layer_index - 1) % len(
                        self.layers)

            layer_index = self.layer_index

        self.layer_changed(layer_index)

    def current_layer(self) -> DeviceLayer:
        with self.layer_lock:
            return self.layers[self.layer_index]

    def layer_changed(self, layer_index: int) -> None:
        return None

    def record_activity(self) -> None:
        return None

    def tick(self) -> None:
        return None

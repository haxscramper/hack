from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import StrEnum
from threading import Lock
from beartype import beartype
from beartype.typing import Any, Iterator, List, Optional, Set, Union
from pathlib import Path
from pydantic import BaseModel, Field

ButtonIdentifier = Union[int, str]


@dataclass(frozen=True)
class PhysicalPosition:
    x: float
    y: float
    width: float
    height: float


@dataclass(frozen=True)
class PhysicalButton:
    identifier: ButtonIdentifier
    control_event_type: int
    control_code: int
    position: PhysicalPosition


@dataclass(frozen=True)
class ActionDisplay:
    name: str
    configuration: str
    image_path: Optional[Path]


@dataclass(frozen=True)
class ButtonDisplay:
    identifier: ButtonIdentifier
    position: PhysicalPosition
    actions: List[ActionDisplay]


@dataclass(frozen=True)
class LayerDisplay:
    name: str
    buttons: List[ButtonDisplay]


@dataclass(frozen=True)
class DeviceDisplay:
    identifier: str
    layers: List[LayerDisplay]


class PhysicalPositionConfig(BaseModel):
    x: float
    y: float
    width: float = Field(gt=0)
    height: float = Field(gt=0)


class PhysicalButtonConfig(BaseModel):
    pos: PhysicalPositionConfig
    button_id: int | str
    event_type: int | str = "EV_KEY"


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

    def __init__(
        self,
        identifier: str,
        layers: List[DeviceLayer],
        physical_buttons: List[PhysicalButton],
    ) -> None:
        if not layers:
            raise ValueError(
                f"Input device {identifier!r} must define at least one layer")

        if not physical_buttons:
            raise ValueError(
                f"Input device {identifier!r} must define at least one "
                f"physical button")

        layer_names = [layer.name for layer in layers]
        duplicate_names = sorted(
            {name
             for name in layer_names if layer_names.count(name) != 1})
        if duplicate_names:
            raise ValueError(
                f"Input device {identifier!r} has duplicate layer names: "
                f"{duplicate_names}")

        physical_controls = [(
            button.control_event_type,
            button.control_code,
        ) for button in physical_buttons]
        duplicate_controls = sorted({
            control
            for control in physical_controls
            if physical_controls.count(control) != 1
        })
        if duplicate_controls:
            raise ValueError(
                f"Input device {identifier!r} has duplicate physical "
                f"controls: {duplicate_controls}")

        self.identifier = identifier
        self.layers = layers
        self.physical_buttons = physical_buttons
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

    def create_display(
        self,
        layer_names: Optional[List[str]],
    ) -> DeviceDisplay:
        with self.layer_lock:
            current_layer = self.layers[self.layer_index]

        if layer_names is None:
            selected_layers = [current_layer]
        else:
            available_layers = {layer.name: layer for layer in self.layers}
            missing_layers = [
                name for name in layer_names if name not in available_layers
            ]
            if missing_layers:
                raise ValueError(
                    f"Device {self.identifier!r} does not define layers "
                    f"{missing_layers}. Available layers are "
                    f"{list(available_layers)}")

            selected_layers = [available_layers[name] for name in layer_names]

        displays = [
            self.create_layer_display(layer) for layer in selected_layers
        ]
        return DeviceDisplay(
            identifier=self.identifier,
            layers=displays,
        )

    def create_layer_display(
        self,
        layer: DeviceLayer,
    ) -> LayerDisplay:
        buttons: List[ButtonDisplay] = []

        for physical_button in self.physical_buttons:
            control = InputControl(
                event_type=physical_button.control_event_type,
                code=physical_button.control_code,
            )
            bindings = [
                binding for binding in layer.bindings
                if binding.control == control
            ]
            actions = [
                ActionDisplay(
                    name=binding.action.action_name,
                    configuration=binding.action.format(),
                    image_path=None,
                ) for binding in bindings
            ]
            buttons.append(
                ButtonDisplay(
                    identifier=physical_button.identifier,
                    position=physical_button.position,
                    actions=actions,
                ))

        return LayerDisplay(
            name=layer.name,
            buttons=buttons,
        )

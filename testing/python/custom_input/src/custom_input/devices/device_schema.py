from pathlib import Path
from beartype.typing import (
    Annotated,
    Dict,
    List,
    Literal,
    Set,
    Union,
    Any,
)
from pydantic import BaseModel, Field
from custom_input.models import DeviceKind


class TextConfig(BaseModel):
    text: str
    font_size: int = 14
    color: str = "white"
    background_color: str = "black"


class StreamDeckButtonConfig(BaseModel):
    action: dict[str, Any]
    image_path: Path | None = None
    title_text: str | TextConfig | None = None


class StreamDeckLayerConfig(BaseModel):
    name: str
    buttons: Dict[int, StreamDeckButtonConfig]


class StreamDeckDeviceConfig(BaseModel):
    type: Literal[DeviceKind.STREAMDECK]
    id: str
    model: str
    serial: str | None = None
    idle_seconds: float = 60.0
    layers: List[StreamDeckLayerConfig]


class EvdevBindingConfig(BaseModel):
    event_type: int | str = "EV_KEY"
    code: int | str
    values: Set[int] = Field(default_factory=lambda: {1})
    action: dict[str, Any]


class EvdevLayerConfig(BaseModel):
    name: str
    bindings: List[EvdevBindingConfig]


class EvdevDeviceConfig(BaseModel):
    type: Literal[DeviceKind.EVDEV]
    id: str
    path: Path
    grab: bool = False
    layers: List[EvdevLayerConfig]


DeviceConfig = Annotated[
    Union[
        StreamDeckDeviceConfig,
        EvdevDeviceConfig,
    ],
    Field(discriminator="type"),
]

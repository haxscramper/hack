from dataclasses import dataclass
from pathlib import Path
from queue import Queue
from threading import Lock
import time
from beartype import beartype
from beartype.typing import Dict, Iterator, List, Optional
from PIL import Image, ImageDraw, ImageFont
from StreamDeck.Devices.StreamDeck import StreamDeck
from StreamDeck.ImageHelpers import PILHelper

from custom_input.devices.device import (
    DeviceBinding,
    DeviceEvent,
    DeviceLayer,
    InputControl,
    InputDevice,
)
from custom_input.devices.device_schema import StreamDeckButtonConfig, StreamDeckDeviceConfig


@dataclass(frozen=True)
class StreamDeckLayerVisuals:
    buttons: Dict[int, StreamDeckButtonConfig]


@beartype
class StreamDeckInputDevice(InputDevice):

    def __init__(
        self,
        config: StreamDeckDeviceConfig,
        deck: StreamDeck,
        layers: List[DeviceLayer],
        visuals: List[StreamDeckLayerVisuals],
        config_directory: Path,
    ) -> None:
        super().__init__(config.id, layers)
        self.deck = deck
        self.visuals = visuals
        self.config_directory = config_directory
        self.idle_seconds = config.idle_seconds
        self.event_queue: Queue[Optional[DeviceEvent]] = Queue()
        self.image_cache: Dict[int, Dict[int, bytes]] = {}
        self.activity_lock = Lock()
        self.last_activity = time.monotonic()
        self.dimmed = False

        self.deck.reset()
        self.deck.set_key_callback(self.key_callback)
        self.generate_images()
        self.update_display()

    @classmethod
    def create(
        cls,
        config: StreamDeckDeviceConfig,
        deck: StreamDeck,
        config_directory: Path,
    ) -> "StreamDeckInputDevice":
        layers: List[DeviceLayer] = []
        visuals: List[StreamDeckLayerVisuals] = []

        for layer_config in config.layers:
            bindings = [
                DeviceBinding(
                    control=InputControl(event_type=0, code=key),
                    values={1},
                    action=create_action(
                        button.action,
                        config_directory,
                    ),
                ) for key, button in layer_config.buttons.items()
            ]
            layers.append(
                DeviceLayer(
                    name=layer_config.name,
                    bindings=bindings,
                ))
            visuals.append(
                StreamDeckLayerVisuals(buttons=layer_config.buttons, ))

        return cls(
            config=config,
            deck=deck,
            layers=layers,
            visuals=visuals,
            config_directory=config_directory,
        )

    def key_callback(
        self,
        deck: StreamDeck,
        key: int,
        state: bool,
    ) -> None:
        self.event_queue.put(
            DeviceEvent(
                control=InputControl(event_type=0, code=key),
                value=int(state),
            ))

    def events(self) -> Iterator[DeviceEvent]:
        while True:
            event = self.event_queue.get()
            if event is None:
                return
            yield event

    def record_activity(self) -> None:
        with self.activity_lock:
            self.last_activity = time.monotonic()
            was_dimmed = self.dimmed
            self.dimmed = False

        if was_dimmed:
            self.deck.set_brightness(100)
            self.update_display()

    def tick(self) -> None:
        with self.activity_lock:
            elapsed = time.monotonic() - self.last_activity
            should_dim = (self.idle_seconds < elapsed and not self.dimmed)
            if should_dim:
                self.dimmed = True

        if should_dim:
            self.deck.set_brightness(0)

    def layer_changed(self, layer_index: int) -> None:
        self.update_display()

    def update_display(self) -> None:
        with self.layer_lock:
            layer_index = self.layer_index

        self.deck.set_brightness(100)
        for key in range(self.deck.key_count()):
            self.deck.set_key_image(
                key,
                self.image_cache[layer_index][key],
            )

    def generate_images(self) -> None:
        empty_image = self.create_empty_image()

        for layer_index, layer in enumerate(self.visuals):
            images: Dict[int, bytes] = {}

            for key in range(self.deck.key_count()):
                button = layer.buttons.get(key)
                if button is None:
                    images[key] = empty_image
                else:
                    images[key] = self.create_button_image(
                        button,
                        layer_index,
                        key,
                    )

            self.image_cache[layer_index] = images

    def create_button_image(
        self,
        button: StreamDeckButtonConfig,
        layer_index: int,
        key: int,
    ) -> bytes:
        if button.image_path is not None:
            path = (self.config_directory /
                    button.image_path).expanduser().resolve()

            if not path.exists():
                raise ValueError(
                    f"StreamDeck image for device {self.identifier!r}, "
                    f"layer {layer_index}, key {key} does not exist: {path}")

            with Image.open(path) as source:
                image = source.convert("RGBA")
                background = Image.new("RGB", image.size, "black")
                background.paste(image, mask=image.getchannel("A"))
                image = background
        elif button.title_text is not None:
            image = self.create_text_image(button.title_text)
        else:
            image = self.create_default_image(layer_index, key)

        image = image.resize(self.deck.key_image_format()["size"])
        return PILHelper.to_native_format(self.deck, image)

    def create_text_image(
        self,
        title: str | TextConfig,
    ) -> Image.Image:
        size = self.deck.key_image_format()["size"]

        match title:
            case str():
                text = title
                font_size = 14
                color = "white"
                background_color = "black"
            case TextConfig():
                text = title.text
                font_size = title.font_size
                color = title.color
                background_color = title.background_color

        image = Image.new("RGB", size, background_color)
        draw = ImageDraw.Draw(image)
        font = self.load_font(font_size)
        box = draw.textbbox((0, 0), text, font=font)
        width = box[2] - box[0]
        height = box[3] - box[1]
        position = (
            (size[0] - width) // 2,
            (size[1] - height) // 2,
        )
        draw.text(position, text, font=font, fill=color)
        return image

    def create_default_image(
        self,
        layer_index: int,
        key: int,
    ) -> Image.Image:
        size = self.deck.key_image_format()["size"]
        image = Image.new("RGB", size, "black")
        draw = ImageDraw.Draw(image)
        layer_font = self.load_font(24)
        key_font = self.load_font(48)
        layer_text = str(layer_index)
        key_text = str(key)
        layer_box = draw.textbbox((0, 0), layer_text, font=layer_font)
        key_box = draw.textbbox((0, 0), key_text, font=key_font)
        layer_width = layer_box[2] - layer_box[0]
        layer_height = layer_box[3] - layer_box[1]
        key_width = key_box[2] - key_box[0]
        key_height = key_box[3] - key_box[1]
        total_height = layer_height + key_height + 5
        layer_position = (
            (size[0] - layer_width) // 2,
            (size[1] - total_height) // 2,
        )
        key_position = (
            (size[0] - key_width) // 2,
            layer_position[1] + layer_height + 5,
        )
        draw.text(
            layer_position,
            layer_text,
            font=layer_font,
            fill="green",
        )
        draw.text(
            key_position,
            key_text,
            font=key_font,
            fill="red",
        )
        return image

    def create_empty_image(self) -> bytes:
        image = Image.new(
            "RGB",
            self.deck.key_image_format()["size"],
            "black",
        )
        return PILHelper.to_native_format(self.deck, image)

    def load_font(self, size: int) -> ImageFont.FreeTypeFont:
        try:
            return ImageFont.truetype("arial.ttf", size)
        except OSError:
            return ImageFont.load_default(size=size)

    def close(self) -> None:
        self.event_queue.put(None)
        self.deck.reset()
        self.deck.close()

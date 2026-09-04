import hashlib
import math
import re
import subprocess
import tkinter
import tkinter.font
from pathlib import Path

from beartype import beartype
from beartype.typing import List, Optional, Tuple
from PIL import Image, ImageTk
from pydantic import BaseModel, Field

from custom_input.actions.action import (
    Action,
    ActionCreateContext,
    ActionTriggerContext,
)
from custom_input.devices.device import ActionDisplay, DeviceDisplay, LayerDisplay

BUTTON_UNIT_SIZE = 40
MAXIMUM_MONITOR_FRACTION = 0.8


@beartype
def action_color(actions: List[ActionDisplay]) -> str:
    if not actions:
        return "#e8e8e8"

    digest = hashlib.sha256(actions[0].name.encode("utf-8")).digest()
    red = 120 + digest[0] % 100
    green = 120 + digest[1] % 100
    blue = 120 + digest[2] % 100
    return f"#{red:02x}{green:02x}{blue:02x}"


@beartype
def layer_bounds(layer: LayerDisplay) -> Tuple[float, float, float, float]:
    if not layer.buttons:
        raise ValueError(
            f"Layer {layer.name!r} has no physical buttons to display")

    minimum_x = min(button.position.x for button in layer.buttons)
    minimum_y = min(button.position.y for button in layer.buttons)
    maximum_x = max(button.position.x + button.position.width
                    for button in layer.buttons)
    maximum_y = max(button.position.y + button.position.height
                    for button in layer.buttons)

    return minimum_x, minimum_y, maximum_x, maximum_y


@beartype
def active_monitor_bounds(root: tkinter.Tk, ) -> Tuple[int, int, int, int]:
    pointer_x = root.winfo_pointerx()
    pointer_y = root.winfo_pointery()
    output = subprocess.run(
        ["xrandr", "--listactivemonitors"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout

    geometry_pattern = re.compile(r"(\d+)/\d+x(\d+)/\d+([+-]\d+)([+-]\d+)")

    for line in output.splitlines()[1:]:
        match = geometry_pattern.search(line)
        if match is None:
            continue

        width = int(match.group(1))
        height = int(match.group(2))
        x = int(match.group(3))
        y = int(match.group(4))

        if (x <= pointer_x < x + width and y <= pointer_y < y + height):
            return x, y, width, height

    raise RuntimeError(f"Could not find the monitor containing pointer "
                       f"position ({pointer_x}, {pointer_y})")


@beartype
def load_overlay_image(
    image_path: Path,
    width: int,
    height: int,
) -> ImageTk.PhotoImage:
    with Image.open(image_path) as source:
        image = source.convert("RGBA")
        image.thumbnail(
            (width, height),
            Image.Resampling.LANCZOS,
        )
        return ImageTk.PhotoImage(image.copy())


@beartype
def show_device_overlay(display: DeviceDisplay) -> None:
    if not display.layers:
        raise ValueError(
            f"Device {display.identifier!r} has no layers to display")

    root = tkinter.Tk()
    root.withdraw()
    root.title(f"Input layout: {display.identifier}")
    root.attributes("-topmost", True)
    root.configure(background="white")
    root.update_idletasks()

    def close_overlay(event: tkinter.Event) -> None:
        root.destroy()

    root.bind("<Escape>", close_overlay)

    monitor_x, monitor_y, monitor_width, monitor_height = (
        active_monitor_bounds(root))
    maximum_window_width = int(monitor_width * MAXIMUM_MONITOR_FRACTION)
    maximum_window_height = int(monitor_height * MAXIMUM_MONITOR_FRACTION)

    margin = 40
    device_title_height = 40
    layer_title_height = 40
    layer_gap = 24

    device_title_font = tkinter.font.Font(
        root=root,
        family="Sans",
        size=20,
        weight="bold",
    )
    layer_title_font = tkinter.font.Font(
        root=root,
        family="Sans",
        size=17,
        weight="bold",
    )
    button_font = tkinter.font.Font(
        root=root,
        family="Sans",
        size=10,
    )

    layer_geometry: List[Tuple[LayerDisplay, float, float, float, float]] = []
    maximum_layout_width = 0.0
    total_layout_height = 0.0

    for layer in display.layers:
        minimum_x, minimum_y, maximum_x, maximum_y = (layer_bounds(layer))
        layout_width = maximum_x - minimum_x
        layout_height = maximum_y - minimum_y
        maximum_layout_width = max(
            maximum_layout_width,
            layout_width,
        )
        total_layout_height += layout_height
        layer_geometry.append((
            layer,
            minimum_x,
            minimum_y,
            maximum_x,
            maximum_y,
        ))

    fixed_width = margin * 2
    fixed_height = (margin * 2 + device_title_height +
                    layer_title_height * len(display.layers) + layer_gap *
                    (len(display.layers) - 1))

    available_layout_width = maximum_window_width - fixed_width
    available_layout_height = maximum_window_height - fixed_height
    if available_layout_width <= 0 or available_layout_height <= 0:
        raise ValueError(
            "The active monitor is too small to display the overlay")

    scale = min(
        BUTTON_UNIT_SIZE,
        available_layout_width / maximum_layout_width,
        available_layout_height / total_layout_height,
    )

    widest_title = max(
        device_title_font.measure(display.identifier),
        max(layer_title_font.measure(layer.name) for layer in display.layers),
    )
    content_width = max(
        maximum_layout_width * scale,
        float(widest_title),
    )

    window_width = min(
        maximum_window_width,
        math.ceil(fixed_width + content_width),
    )
    window_height = min(
        maximum_window_height,
        math.ceil(fixed_height + total_layout_height * scale),
    )
    window_x = monitor_x + (monitor_width - window_width) // 2
    window_y = monitor_y + (monitor_height - window_height) // 2

    root.geometry(f"{window_width}x{window_height}"
                  f"{window_x:+d}{window_y:+d}")

    canvas = tkinter.Canvas(
        root,
        width=window_width,
        height=window_height,
        background="white",
        highlightthickness=0,
    )
    canvas.pack(fill=tkinter.BOTH, expand=True)

    image_references: List[ImageTk.PhotoImage] = []

    canvas.create_text(
        window_width / 2,
        margin + device_title_height / 2,
        text=display.identifier,
        fill="black",
        font=device_title_font,
    )

    current_y = margin + device_title_height

    for layer_index, (
            layer,
            minimum_x,
            minimum_y,
            maximum_x,
            maximum_y,
    ) in enumerate(layer_geometry):
        layout_width = (maximum_x - minimum_x) * scale
        layout_height = (maximum_y - minimum_y) * scale

        canvas.create_text(
            window_width / 2,
            current_y + layer_title_height / 2,
            text=layer.name,
            fill="black",
            font=layer_title_font,
        )

        origin_x = (window_width - layout_width) / 2
        origin_y = current_y + layer_title_height

        canvas.create_rectangle(
            origin_x - 8,
            origin_y - 8,
            origin_x + layout_width + 8,
            origin_y + layout_height + 8,
            outline="#b0b0b0",
            width=2,
        )

        for button in layer.buttons:
            position = button.position
            left = origin_x + (position.x - minimum_x) * scale
            top = origin_y + (position.y - minimum_y) * scale
            right = left + position.width * scale
            bottom = top + position.height * scale
            color = action_color(button.actions)

            canvas.create_rectangle(
                left,
                top,
                right,
                bottom,
                fill=color,
                outline="black",
                width=2,
            )

            image_action = next(
                (action for action in button.actions
                 if action.image_path is not None),
                None,
            )
            text_top = top + 8

            if image_action is not None:
                image_height = max(
                    1,
                    int((bottom - top) * 0.45),
                )
                image_width = max(
                    1,
                    int(right - left - 16),
                )
                image = load_overlay_image(
                    image_action.image_path,
                    image_width,
                    image_height,
                )
                image_references.append(image)
                canvas.create_image(
                    (left + right) / 2,
                    top + 8,
                    image=image,
                    anchor=tkinter.N,
                )
                text_top += image.height() + 6

            identifier = str(button.identifier)
            action_text = "\n\n".join(action.configuration
                                      for action in button.actions)
            text = identifier
            if action_text:
                text = f"{identifier}\n{action_text}"

            canvas.create_text(
                (left + right) / 2,
                text_top,
                text=text,
                fill="black",
                width=max(1, int(right - left - 12)),
                justify=tkinter.CENTER,
                anchor=tkinter.N,
                font=button_font,
            )

        current_y = origin_y + layout_height
        if layer_index < len(display.layers) - 1:
            current_y += layer_gap

    root.deiconify()
    root.focus_force()
    root.mainloop()


class ShowDeviceLayersActionConfig(BaseModel):
    layers: Optional[List[str]] = Field(
        default=None,
        min_length=1,
    )


@beartype
class ShowDeviceLayersAction(Action[ShowDeviceLayersActionConfig]):
    schema = ShowDeviceLayersActionConfig
    action_name = "show_device_layers"

    def __init__(
        self,
        cfg: ShowDeviceLayersActionConfig,
        create_ctx: ActionCreateContext,
    ) -> None:
        super().__init__(cfg, create_ctx)

    def execute(self, context: ActionTriggerContext) -> None:
        display = context.device.create_display(self.config.layers)
        show_device_overlay(display)

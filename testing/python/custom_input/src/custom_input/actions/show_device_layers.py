from beartype import beartype
from beartype.typing import List, Optional
from pydantic import BaseModel, Field

from custom_input.actions.action import (
    Action,
    ActionCreateContext,
    ActionTriggerContext,
)

import hashlib
import tkinter
from pathlib import Path
from beartype import beartype
from beartype.typing import List, Tuple
from PIL import Image, ImageTk

from custom_input.devices.device import ActionDisplay, DeviceDisplay, LayerDisplay


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
def layer_bounds(layer: LayerDisplay, ) -> Tuple[float, float, float, float]:
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
    root.title(f"Input layout: {display.identifier}")
    root.attributes("-fullscreen", True)
    root.attributes("-topmost", True)
    root.configure(background="white")

    def close_overlay(event: tkinter.Event) -> None:
        root.destroy()

    root.bind("<Escape>", close_overlay)
    root.focus_force()
    root.update_idletasks()

    screen_width = root.winfo_screenwidth()
    screen_height = root.winfo_screenheight()
    canvas = tkinter.Canvas(
        root,
        width=screen_width,
        height=screen_height,
        background="white",
        highlightthickness=0,
    )
    canvas.pack(fill=tkinter.BOTH, expand=True)

    margin = 40
    title_height = 40
    layer_gap = 24
    available_height = (screen_height - margin * 2 - title_height - layer_gap *
                        (len(display.layers) - 1))
    panel_height = available_height / len(display.layers)
    image_references: List[ImageTk.PhotoImage] = []

    canvas.create_text(
        screen_width / 2,
        margin / 2,
        text=display.identifier,
        fill="black",
        font=("Sans", 20, "bold"),
    )

    for layer_index, layer in enumerate(display.layers):
        minimum_x, minimum_y, maximum_x, maximum_y = layer_bounds(layer)
        layout_width = maximum_x - minimum_x
        layout_height = maximum_y - minimum_y
        panel_top = (margin + title_height + layer_index *
                     (panel_height + layer_gap))
        content_top = panel_top + title_height
        content_height = panel_height - title_height
        content_width = screen_width - margin * 2
        scale = min(
            content_width / layout_width,
            content_height / layout_height,
        )
        rendered_width = layout_width * scale
        rendered_height = layout_height * scale
        origin_x = (screen_width - rendered_width) / 2
        origin_y = (content_top + (content_height - rendered_height) / 2)

        canvas.create_text(
            screen_width / 2,
            panel_top + title_height / 2,
            text=layer.name,
            fill="black",
            font=("Sans", 17, "bold"),
        )
        canvas.create_rectangle(
            origin_x - 8,
            origin_y - 8,
            origin_x + rendered_width + 8,
            origin_y + rendered_height + 8,
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
                font=("Sans", 10),
            )

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

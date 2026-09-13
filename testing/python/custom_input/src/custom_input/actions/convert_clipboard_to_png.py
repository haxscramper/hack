import io
import os
import subprocess
import tempfile
from pathlib import Path
from urllib.parse import unquote, urlparse

from beartype import beartype
from PIL import Image, ImageOps
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext


@beartype
def clipboard_targets() -> list[str]:
    result = subprocess.run(
        ["xclip", "-selection", "clipboard", "-target", "TARGETS", "-out"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.splitlines()


@beartype
def clipboard_data(target: str) -> bytes:
    result = subprocess.run(
        ["xclip", "-selection", "clipboard", "-target", target, "-out"],
        check=True,
        capture_output=True,
    )
    return result.stdout


@beartype
def clipboard_path(targets: list[str]) -> Path:
    target = "text/uri-list" if "text/uri-list" in targets else "UTF8_STRING"
    text = clipboard_data(target).decode().strip()

    if target == "text/uri-list":
        text = next(line for line in text.splitlines()
                    if line and not line.startswith("#"))

    parsed = urlparse(text)
    if parsed.scheme == "file":
        return Path(unquote(parsed.path))

    return Path(os.path.expanduser(text)).resolve()


class ConvertClipboardToPNGActionConfig(BaseModel):
    pass


@beartype
class ConvertClipboardToPNGAction(Action[ConvertClipboardToPNGActionConfig]):
    schema = ConvertClipboardToPNGActionConfig
    action_name = "convert_clipboard_to_png"

    def execute(self, context: ActionTriggerContext) -> None:
        targets = clipboard_targets()
        image_target = next(
            (target for target in targets if target.startswith("image/")),
            None,
        )

        if image_target is not None:
            image = Image.open(io.BytesIO(clipboard_data(image_target)))
        else:
            image = Image.open(clipboard_path(targets))

        image = ImageOps.exif_transpose(image).convert("RGBA")

        descriptor, output_path = tempfile.mkstemp(
            prefix="clipboard-image-",
            suffix=".png",
            dir="/tmp",
        )
        os.close(descriptor)

        image.save(output_path, format="PNG")

        subprocess.run(
            ["xclip", "-selection", "clipboard", "-in"],
            input=output_path.encode(),
            check=True,
        )

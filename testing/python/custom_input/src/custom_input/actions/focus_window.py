from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from pydantic import BaseModel
from plumbum import local
from beartype.typing import Optional, List
from loguru import logger

from custom_input.actions.action import Action, ActionTriggerContext
from custom_input.models import ActionKind


@beartype
def find_window(
    windows: List[str],
    window_name: str,
) -> Optional[str]:
    for line in windows:
        if window_name.lower() in line.lower():
            return line.split()[0]

    return None


@beartype
def focus_window(window_name: str) -> None:
    xprop = local["xprop"]
    wmctrl = local["wmctrl"]

    current_desktop = int(xprop("-root", "_NET_CURRENT_DESKTOP").split()[-1])
    base_desktop = (current_desktop // 9) * 9
    windows = [
        line for line in wmctrl("-l", "-x").splitlines()
        if line.startswith("0x")
    ]

    logger.info(f"Current desktop: {current_desktop}")

    windows_on_current_group = [
        line for line in windows
        if (base_desktop <= int(line.split()[1])
            and int(line.split()[1]) < base_desktop + 9)
    ]

    window = find_window(windows_on_current_group, window_name)
    if window is None:
        window = find_window(windows, window_name)

    if window is None:
        logger.info(f"No window matching {window_name!r} was found")
        return

    wmctrl("-i", "-a", window)


class FocusWindowActionConfig(BaseModel):
    type: Literal[ActionKind.FOCUS_WINDOW]
    text: str


@beartype
class FocusWindowAction(Action[FocusWindowActionConfig]):
    schema = FocusWindowActionConfig
    action_name = "focus_window"

    def execute(self, context: ActionTriggerContext) -> None:
        context.application.submit(lambda: focus_window(self.config.text))

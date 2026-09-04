from pathlib import Path
from beartype import beartype
from beartype.typing import List, Literal
import pyautogui
from pydantic import BaseModel, field_validator

from custom_input.actions.action import Action, ActionCreateContext, ActionTriggerContext
from custom_input.models import ActionKind


class EmulateShortcutActionConfig(BaseModel):
    type: Literal[ActionKind.EMULATE_SHORTCUT]
    shortcut: str

    @field_validator("shortcut")
    @classmethod
    def validate_shortcut(cls, shortcut: str) -> str:
        keys = [key.strip() for key in shortcut.split("+")]
        if not keys or any(not key for key in keys):
            raise ValueError(
                f"Shortcut {shortcut!r} does not contain a valid key sequence")
        return shortcut


@beartype
class EmulateShortcutAction(Action[EmulateShortcutActionConfig]):
    schema = EmulateShortcutActionConfig
    action_name = "emulate_shortcut"

    def __init__(self, config: EmulateShortcutActionConfig,
                 create_ctx: ActionCreateContext) -> None:
        super().__init__(config=config, create_ctx=self.create_ctx)
        self.keys = [key.strip() for key in config.shortcut.split("+")]

    def execute(self, context: ActionTriggerContext) -> None:
        context.application.submit(lambda: pyautogui.hotkey(*self.keys))

from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext
from custom_input.types import ActionKind


class QuitActionConfig(BaseModel):
    type: Literal[ActionKind.QUIT]


@beartype
class QuitAction(Action[QuitActionConfig]):
    schema = QuitActionConfig
    action_name = "quit"

    def execute(self, context: ActionTriggerContext) -> None:
        context.application.request_quit()

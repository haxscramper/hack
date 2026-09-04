from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext
from custom_input.text_input import type_text


class TypeTextActionConfig(BaseModel):
    text: str


@beartype
class TypeTextAction(Action[TypeTextActionConfig]):
    schema = TypeTextActionConfig
    action_name = "type_text"

    def execute(self, context: ActionTriggerContext) -> None:
        context.application.submit(lambda: type_text(self.config.text))

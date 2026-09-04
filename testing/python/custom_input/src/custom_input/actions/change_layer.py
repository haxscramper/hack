from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext
from custom_input.types import ActionKind, LayerDirection


class ChangeLayerActionConfig(BaseModel):
    type: Literal[ActionKind.CHANGE_LAYER]
    direction: LayerDirection


@beartype
class ChangeLayerAction(Action[ChangeLayerActionConfig]):
    schema = ChangeLayerActionConfig
    action_name = "change_layer"

    def execute(self, context: ActionTriggerContext) -> None:
        context.device.change_layer(self.config.direction)

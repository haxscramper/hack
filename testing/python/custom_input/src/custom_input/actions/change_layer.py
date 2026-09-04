from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext
from custom_input.devices.device import LayerDirection


class ChangeLayerActionConfig(BaseModel):
    direction: LayerDirection


@beartype
class ChangeLayerAction(Action[ChangeLayerActionConfig]):
    schema = ChangeLayerActionConfig
    action_name = "change_layer"

    def execute(self, context: ActionTriggerContext) -> None:
        context.device.change_layer(self.config.direction)

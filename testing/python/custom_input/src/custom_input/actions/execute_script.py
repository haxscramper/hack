from pathlib import Path
from beartype import beartype
from beartype.typing import Literal
from plumbum import local
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionCreateContext, ActionTriggerContext
from custom_input.types import ActionKind
from custom_input.text_input import type_text


class ExecuteScriptActionConfig(BaseModel):
    type: Literal[ActionKind.EXECUTE_SCRIPT]
    script: str
    insert_output: bool = False


@beartype
class ExecuteScriptAction(Action[ExecuteScriptActionConfig]):

    def __init__(self, cfg: ExecuteScriptActionConfig,
                 create_ctx: ActionCreateContext) -> None:
        super().__init__(cfg, create_ctx)

    def execute(self, context: ActionTriggerContext) -> None:

        def run() -> None:
            shell = local["sh"]
            output = shell("-c", self.config.script)

            if self.config.insert_output:
                type_text(output)

        context.application.submit(run)

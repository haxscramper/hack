from pathlib import Path

from beartype import beartype
from beartype.typing import Literal
from plumbum import ProcessExecutionError, local
from pydantic import BaseModel

from custom_input.actions import show_notification
from custom_input.actions.action import (
    Action,
    ActionCreateContext,
    ActionTriggerContext,
)
from custom_input.text_input import type_text


class ExecuteScriptActionConfig(BaseModel):
    script: str
    insert_output: bool = False
    fault_tolerant: bool = False


@beartype
class ExecuteScriptAction(Action[ExecuteScriptActionConfig]):
    schema = ExecuteScriptActionConfig
    action_name = "execute_script"

    def __init__(self, cfg: ExecuteScriptActionConfig,
                 create_ctx: ActionCreateContext) -> None:
        super().__init__(cfg, create_ctx)

    def execute(self, context: ActionTriggerContext) -> None:

        def run() -> None:
            shell = local["sh"]

            output = ""

            if self.config.fault_tolerant:
                try:
                    output = shell("-c", self.config.script)

                except ProcessExecutionError as e:
                    show_notification.show_notification(
                        show_notification.ShowNotificationActionConfig(
                            summary=str(e),
                            urgency=show_notification.NotificationUrgency.
                            CRITICAL,
                        ))

            else:
                output = shell("-c", self.config.script)

            if self.config.insert_output:
                type_text(output)

        context.application.submit(run)

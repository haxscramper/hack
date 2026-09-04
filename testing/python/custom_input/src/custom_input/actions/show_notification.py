from enum import StrEnum
from beartype import beartype
from beartype.typing import List, Optional
from plumbum import local
from pydantic import BaseModel, Field

from custom_input.actions.action import (
    Action,
    ActionCreateContext,
    ActionTriggerContext,
)


class NotificationUrgency(StrEnum):
    LOW = "low"
    NORMAL = "normal"
    CRITICAL = "critical"


class ShowNotificationActionConfig(BaseModel):
    summary: str
    body: Optional[str] = None
    app_name: Optional[str] = None
    urgency: NotificationUrgency = NotificationUrgency.NORMAL
    expire_time_ms: Optional[int] = Field(default=None, ge=0)
    icon: Optional[str] = None
    categories: List[str] = Field(default_factory=list)
    hints: List[str] = Field(default_factory=list)
    replace_id: Optional[int] = Field(default=None, ge=0)
    transient: bool = False
    wait: bool = False


@beartype
class ShowNotificationAction(Action[ShowNotificationActionConfig]):
    schema = ShowNotificationActionConfig
    action_name = "show_notification"

    def __init__(
        self,
        cfg: ShowNotificationActionConfig,
        create_ctx: ActionCreateContext,
    ) -> None:
        super().__init__(cfg, create_ctx)

    def execute(self, context: ActionTriggerContext) -> None:

        def run() -> None:
            arguments = [
                "--urgency",
                self.config.urgency.value,
            ]

            if self.config.app_name is not None:
                arguments.extend([
                    "--app-name",
                    self.config.app_name,
                ])

            if self.config.expire_time_ms is not None:
                arguments.extend([
                    "--expire-time",
                    str(self.config.expire_time_ms),
                ])

            if self.config.icon is not None:
                arguments.extend([
                    "--icon",
                    self.config.icon,
                ])

            if self.config.categories:
                arguments.extend([
                    "--category",
                    ",".join(self.config.categories),
                ])

            for hint in self.config.hints:
                arguments.extend(["--hint", hint])

            if self.config.replace_id is not None:
                arguments.extend([
                    "--replace-id",
                    str(self.config.replace_id),
                ])

            if self.config.transient:
                arguments.append("--transient")

            if self.config.wait:
                arguments.append("--wait")

            arguments.append(self.config.summary)

            if self.config.body is not None:
                arguments.append(self.config.body)

            notify_send = local["notify-send"]
            notify_send(*arguments)

        context.application.submit(run)

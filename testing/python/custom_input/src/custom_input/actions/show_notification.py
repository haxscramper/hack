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
    expire_time_ms: Optional[int] = Field(default=5000, ge=0)
    icon: Optional[str] = None
    categories: List[str] = Field(default_factory=list)
    hints: List[str] = Field(default_factory=list)
    replace_id: Optional[int] = Field(default=None, ge=0)
    transient: bool = False
    wait: bool = False


@beartype
def show_notification(config: ShowNotificationActionConfig):
    arguments = [
        "--urgency",
        config.urgency.value,
    ]

    if config.app_name is not None:
        arguments.extend([
            "--app-name",
            config.app_name,
        ])

    if config.expire_time_ms is not None:
        arguments.extend([
            "--expire-time",
            str(config.expire_time_ms),
        ])

    if config.icon is not None:
        arguments.extend([
            "--icon",
            config.icon,
        ])

    if config.categories:
        arguments.extend([
            "--category",
            ",".join(config.categories),
        ])

    for hint in config.hints:
        arguments.extend(["--hint", hint])

    if config.replace_id is not None:
        arguments.extend([
            "--replace-id",
            str(config.replace_id),
        ])

    if config.transient:
        arguments.append("--transient")

    if config.wait:
        arguments.append("--wait")

    arguments.append(config.summary)

    if config.body is not None:
        arguments.append(config.body)

    notify_send = local["notify-send"]
    notify_send(*arguments)


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
            show_notification(self.config)

        context.application.submit(run)

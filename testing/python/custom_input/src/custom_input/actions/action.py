from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import TYPE_CHECKING, Any, ClassVar, Generic, TypeVar
from loguru import logger
from pydantic import BaseModel

from custom_input.devices.device import DeviceEvent

if TYPE_CHECKING:
    from custom_input.application import Application
    from custom_input.devices.device import InputDevice

ActionConfigType = TypeVar("ActionConfigType", bound=BaseModel)


@dataclass(frozen=True)
class ActionTriggerContext:
    application: "Application"
    device: "InputDevice"
    event: DeviceEvent


@dataclass
class ActionCreateContext:
    config_dir: Path


ACTION_TYPES = []


@beartype
class Action(ABC, Generic[ActionConfigType]):
    schema: ClassVar[type[BaseModel]]
    action_name: ClassVar[str]
    config: ActionConfigType
    create_ctx: ActionCreateContext

    IMPLEMENTATIONS: ClassVar[dict[str, type["Action[Any]"]]] = dict()

    def __init_subclass__(cls, **kwargs: Any) -> None:
        super().__init_subclass__(**kwargs)
        logger.info(
            f"Registered action name {cls.action_name!r} to type {cls}")
        Action.IMPLEMENTATIONS[cls.action_name] = cls

    def __init__(self, config: ActionConfigType,
                 create_ctx: ActionCreateContext):
        self.config = config
        self.create_ctx = create_ctx

    @staticmethod
    def from_config(
        cls_type: type["Action"],
        config: dict[str, Any],
        create_ctx: ActionCreateContext,
    ) -> "Action":
        return cls_type(
            config=cls_type.schema.model_validate(config),
            create_ctx=create_ctx,
        )

    @abstractmethod
    def execute(self, context: ActionTriggerContext) -> None:
        raise NotImplementedError

    def format(self) -> str:
        values = self.config.model_dump(exclude_none=True)
        configuration = ", ".join(f"{name}={value}"
                                  for name, value in values.items())

        if configuration:
            return f"{self.action_name}\n{configuration}"

        return self.action_name


def create_action(config: dict[str, Any],
                  context: ActionCreateContext) -> Action[Any]:
    impl_type = Action.IMPLEMENTATIONS[config["type"]]
    return impl_type(impl_type.schema.model_validate(config), context)

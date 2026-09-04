from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from enum import StrEnum
from queue import Empty, Queue
from threading import Event, Thread
from beartype import beartype
from beartype.typing import Callable, List, Optional
from loguru import logger

from beartype.typing import (List)
from pydantic import BaseModel
from enum import StrEnum
from pathlib import Path
from beartype.typing import (Annotated, List, Union)
from pydantic import BaseModel, Field

from custom_input.actions.action import ActionTriggerContext
from custom_input.devices.device import DeviceEvent, InputDevice
from custom_input.devices.evdev_device import EvdevDeviceConfig
from custom_input.devices.streamdeck_device import StreamDeckDeviceConfig


class DeviceMessageKind(StrEnum):
    EVENT = "event"
    ERROR = "error"
    ENDED = "ended"


@dataclass(frozen=True)
class DeviceMessage:
    kind: DeviceMessageKind
    device: InputDevice
    event: Optional[DeviceEvent] = None
    error: Optional[Exception] = None


DeviceConfig = Annotated[
    Union[
        StreamDeckDeviceConfig,
        EvdevDeviceConfig,
    ],
    Field(discriminator="type"),
]


class ApplicationConfig(BaseModel):
    devices: List[DeviceConfig]


@beartype
class Application:

    def __init__(self, devices: List[InputDevice]) -> None:
        self.devices = devices
        self.messages: Queue[DeviceMessage] = Queue()
        self.quit_requested = Event()
        self.executor = ThreadPoolExecutor()
        self.futures: List[Future[None]] = []
        self.threads: List[Thread] = []

    def submit(self, operation: Callable[[], None]) -> None:
        self.futures.append(self.executor.submit(operation))

    def request_quit(self) -> None:
        self.quit_requested.set()

    def read_device(self, device: InputDevice) -> None:
        try:
            for event in device.events():
                self.messages.put(
                    DeviceMessage(
                        kind=DeviceMessageKind.EVENT,
                        device=device,
                        event=event,
                    ))
        except Exception as error:
            self.messages.put(
                DeviceMessage(
                    kind=DeviceMessageKind.ERROR,
                    device=device,
                    error=error,
                ))
            return

        self.messages.put(
            DeviceMessage(
                kind=DeviceMessageKind.ENDED,
                device=device,
            ))

    def check_futures(self) -> None:
        pending: List[Future[None]] = []

        for future in self.futures:
            if future.done():
                future.result()
            else:
                pending.append(future)

        self.futures = pending

    def tick_devices(self) -> None:
        for device in self.devices:
            device.tick()

    def process_message(self, message: DeviceMessage) -> None:
        match message.kind:
            case DeviceMessageKind.EVENT:
                if message.event is None:
                    raise RuntimeError(
                        f"Device {message.device.identifier!r} emitted an "
                        f"event message without an event")

                message.device.record_activity()
                action = message.device.action_for(message.event)
                if action is not None:
                    logger.info(
                        f"Executing {type(action).__name__} from device "
                        f"{message.device.identifier!r}, control "
                        f"{message.event.control}, value "
                        f"{message.event.value}")
                    action.execute(
                        ActionTriggerContext(
                            application=self,
                            device=message.device,
                            event=message.event,
                        ))
            case DeviceMessageKind.ERROR:
                if message.error is None:
                    raise RuntimeError(
                        f"Device {message.device.identifier!r} emitted an "
                        f"error message without an exception")
                raise message.error
            case DeviceMessageKind.ENDED:
                raise RuntimeError(
                    f"Device {message.device.identifier!r} stopped "
                    f"producing events unexpectedly")

    def run(self) -> None:
        for device in self.devices:
            thread = Thread(
                target=self.read_device,
                args=(device, ),
                daemon=True,
                name=f"input-{device.identifier}",
            )
            self.threads.append(thread)
            thread.start()

        try:
            while not self.quit_requested.is_set():
                try:
                    message = self.messages.get(timeout=1.0)
                except Empty:
                    self.tick_devices()
                    self.check_futures()
                    continue

                self.process_message(message)
                self.tick_devices()
                self.check_futures()
        finally:
            self.close()

    def close(self) -> None:
        for device in self.devices:
            device.close()

        self.executor.shutdown(wait=True)

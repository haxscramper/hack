import json
import subprocess
import threading
import time
from collections import deque
from pathlib import Path

from beartype import beartype
from beartype.typing import List, Optional
from loguru import logger
from plumbum import local
from pydantic import BaseModel

from custom_input.actions.action import Action, ActionTriggerContext

_CYCLE_INTERVAL_SECONDS = 0.5
_STARTUP_INTERVAL_SECONDS = 2.0
_STARTUP_CALL_COUNT = 5
_AWESOME_FOCUS_SCRIPT = Path(__file__).with_name("focus_window.lua")

_focus_lock = threading.Lock()
_last_target: Optional[str] = None
_last_call_time = 0.0
_last_window_id: Optional[str] = None

_call_times: dict[str, deque[float]] = {}
_startup_triggered: set[str] = set()
_last_startup_call: dict[str, float] = {}


@beartype
def find_window(
    windows: List[str],
    window_name: str,
) -> Optional[str]:
    for line in windows:
        if window_name.lower() in line.lower():
            return line.split()[0]

    return None


@beartype
def find_windows(
    windows: List[str],
    window_name: str,
) -> List[str]:
    normalized_name = window_name.lower()

    return [
        line.split()[0] for line in windows if normalized_name in line.lower()
    ]


@beartype
def start_detached(command: list[str]) -> None:
    subprocess.Popen(
        command,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
        close_fds=True,
    )


@beartype
def update_startup_state(
    window_name: str,
    startup_command: Optional[list[str]],
    now: float,
) -> None:
    normalized_name = window_name.lower()
    previous_call = _last_startup_call.get(normalized_name)

    if previous_call is None or now - previous_call > _STARTUP_INTERVAL_SECONDS:
        _call_times[normalized_name] = deque()
        _startup_triggered.discard(normalized_name)

    _last_startup_call[normalized_name] = now

    call_times = _call_times.setdefault(normalized_name, deque())
    call_times.append(now)

    while call_times and now - call_times[0] > _STARTUP_INTERVAL_SECONDS:
        call_times.popleft()

    if (len(call_times) >= _STARTUP_CALL_COUNT
            and normalized_name not in _startup_triggered):
        _startup_triggered.add(normalized_name)

        if startup_command is not None:
            logger.info(f"Starting detached process: {startup_command!r}")
            start_detached(startup_command)


@beartype
def focus_awesome_window(window_id: str, reset_state: bool) -> None:
    awesome_client = local["awesome-client"]
    script_path = json.dumps(str(_AWESOME_FOCUS_SCRIPT))
    window_number = int(window_id, 16)
    reset_value = "true" if reset_state else "false"

    awesome_client(
        f"return dofile({script_path})({window_number}, {reset_value})")


@beartype
def focus_window(
    window_name: str,
    startup_command: Optional[list[str]] = None,
) -> None:
    global _last_call_time
    global _last_target
    global _last_window_id

    with _focus_lock:
        now = time.monotonic()
        normalized_name = window_name.lower()

        update_startup_state(window_name, startup_command, now)

        cycle = (_last_target == normalized_name
                 and now - _last_call_time <= _CYCLE_INTERVAL_SECONDS)

        xprop = local["xprop"]
        wmctrl = local["wmctrl"]

        current_desktop = int(
            xprop("-root", "_NET_CURRENT_DESKTOP").split()[-1])
        base_desktop = (current_desktop // 9) * 9
        windows = [
            line for line in wmctrl("-l", "-x").splitlines()
            if line.startswith("0x")
        ]

        logger.info(f"Current desktop: {current_desktop}")

        matching_windows = find_windows(windows, window_name)

        if not matching_windows:
            logger.info(f"No window matching {window_name!r} was found")
            _last_target = normalized_name
            _last_call_time = now
            _last_window_id = None
            return

        if cycle:
            if _last_window_id in matching_windows:
                current_index = matching_windows.index(_last_window_id)
                window_id = matching_windows[(current_index + 1) %
                                             len(matching_windows)]
            else:
                window_id = matching_windows[0]
        else:
            windows_on_current_group = [
                line for line in windows
                if (base_desktop <= int(line.split()[1])
                    and int(line.split()[1]) < base_desktop + 9)
            ]

            window_id = find_window(windows_on_current_group, window_name)
            if window_id is None:
                window_id = matching_windows[0]

        focus_awesome_window(window_id, reset_state=not cycle)

        _last_target = normalized_name
        _last_call_time = now
        _last_window_id = window_id


class FocusWindowActionConfig(BaseModel):
    text: str
    startup_command: Optional[list[str]] = None


@beartype
class FocusWindowAction(Action[FocusWindowActionConfig]):
    schema = FocusWindowActionConfig
    action_name = "focus_window"

    def execute(self, context: ActionTriggerContext) -> None:
        context.application.submit(lambda: focus_window(
            self.config.text,
            self.config.startup_command,
        ))

from dataclasses import dataclass
import enum
import json
import logging
from datetime import datetime, timezone

from beartype.typing import Any, Callable, Literal, Optional, Set
import os
import traceback
from rich.console import Console
from pathlib import Path

from platformdirs import user_cache_dir


def get_xdg_cache_dir(target: list[str]) -> Path:
    cache_dir = Path(user_cache_dir(appname="hax_index_service"))
    result = cache_dir.joinpath(*target)
    result.mkdir(parents=True, exist_ok=True)
    return result


@dataclass
class FrameInfo:
    filename: str
    line_no: int
    func_name: str
    args: dict[str, Any]
    arg_types: dict[str, str]
    runtime_types: dict[str, str]
    code_line: str


def get_custom_traceback_handler(
    truncate_value=True,
    show_args=True,
) -> Any:
    """
    Return custom traceback handler callback function

    :param truncate_value: pprint the full value even if it is wider than the terminal
        or truncate it at fixed width
    :param show_args: Show argument values in the function frames
    :return: Callback compatible with traceback hooks that returns formatted text
    """

    def impl(exc_type: Any, exc_value: Any, exc_traceback: Any) -> str:
        console = Console(record=True)

        current_tb = exc_traceback
        frames_info = []

        while current_tb is not None:
            frame = current_tb.tb_frame
            filename = frame.f_code.co_filename

            if not any(it in filename for it in [
                    "pykka",
                    "pluggy",
                    "_pytest",
                    "<@beartype",
                    "dagster",
            ]):
                line_no = current_tb.tb_lineno
                func_name = frame.f_code.co_name

                args = {}
                arg_types = {}
                runtime_types = {}

                if show_args:
                    arg_names = frame.f_code.co_varnames[:frame.f_code.co_argcount]
                    for arg_name in arg_names:
                        if arg_name in frame.f_locals:
                            args[arg_name] = frame.f_locals[arg_name]

                extracted_frame = traceback.extract_tb(current_tb, limit=1)[0]
                frames_info.append(
                    FrameInfo(
                        filename,
                        line_no,
                        func_name,
                        args,
                        arg_types,
                        runtime_types,
                        extracted_frame.line or "",
                    ))

            current_tb = current_tb.tb_next

        lines = []

        if not frames_info:
            lines.append(f"{exc_type.__name__} : {exc_value}")
            return "\n".join(lines)

        max_arg_name_width = 0
        if show_args:
            for frame_info in frames_info:
                if frame_info.args:
                    max_arg_name_width = max(
                        max_arg_name_width,
                        max(len(arg_name) for arg_name in frame_info.args.keys()),
                    )

        display_filenames = [
            Path(frame_info.filename).resolve().absolute() for frame_info in frames_info
        ]
        common_prefix = (os.path.commonpath(display_filenames)
                         if display_filenames else "")

        for frame_info in frames_info:
            display_filename = frame_info.filename
            if common_prefix:
                try:
                    display_filename = os.path.relpath(frame_info.filename, common_prefix)
                except ValueError:
                    display_filename = frame_info.filename

            header = (f'File {frame_info.filename}:{frame_info.line_no} '
                      f"in {frame_info.func_name}")
            lines.append(f"{header} : {frame_info.code_line}")

            if show_args:
                for arg_name, arg_value in frame_info.args.items():
                    type_info = ""
                    available_width = (console.width - max_arg_name_width -
                                       len(type_info) - 5)

                    if truncate_value:
                        arg_repr = repr(arg_value)
                        if available_width < len(arg_repr):
                            arg_repr = arg_repr[:available_width - 3] + "..."
                        lines.append(
                            f"  {arg_name}{type_info:<{max_arg_name_width - len(arg_name)}} = {arg_repr}"
                        )
                    else:
                        import pprint

                        formatted_value = pprint.pformat(arg_value, width=available_width)
                        value_lines = formatted_value.split("\n")

                        lines.append(
                            f"  {arg_name}{type_info:<{max_arg_name_width - len(arg_name)}} = {value_lines[0]}"
                        )
                        for line in value_lines[1:]:
                            lines.append(f"  {'':<{max_arg_name_width}}   {line}")

        lines.append(f"{exc_type.__name__} : {exc_value}")

        if hasattr(exc_value, "__notes__"):
            for note in exc_value.__notes__:
                lines.append(f"Note: {note}")

        if hasattr(exc_value, "__rich_msg__"):
            console.print(getattr(exc_value, "__rich_msg__"))
            rich_text = console.export_text(clear=False).rstrip()
            if rich_text:
                lines.append(rich_text)

        return "\n" + "\n".join(lines)

    return impl


def stfu_logs():
    for logger_name in [
            "openai._base_client",
            "git.cmd",
            "alembic",
            "git.util",
            # toggle this to see database interactions in tests
            "urllib3.connectionpool",
            # openai uses this for connection
            "httpcore.connection",
            # each individual resource actor creation and start
            "pykka",
            # execution of individual steps is printed to stderr?
            "dagster",
            "dagster.builtin",
            "asyncio",
            "PIL.Image",
            "PIL.PngImagePlugin",
            "PIL.TiffImagePlugin",
            "PIL",
            "httpcore.http11",
            "httpx",
            "faker.factory",
            "jax._src",
            "plumbum",
            "parso",
    ]:
        logger = logging.getLogger(logger_name)
        logger.setLevel(logging.CRITICAL + 1)
        logger.propagate = False
        logger.handlers.clear()


def dump_with_type(
    obj: Any,
    include_single_underscore_attrs: bool = False,
    include_double_underscore_attrs: bool = False,
    skip_cyclic_data: bool = True,
    override_callback: Optional[Callable] = None,
    with_stable_formatting: bool = True,
) -> Any:
    visited: Set[Any] = set()

    def aux(obj: Any) -> Any:
        if override_callback:
            override_result = override_callback(obj)
            if override_result:
                return override_result

        if isinstance(obj, (int, float, str, bool, type(None))):
            return obj

        elif isinstance(obj, (enum.Enum, enum.IntEnum, enum.StrEnum)):
            return str(obj)

        if skip_cyclic_data and id(obj) in visited:
            if with_stable_formatting:
                return f"cycle {type(obj)}"

            else:
                return f"cycle {type(obj)} {id(obj)}"

        visited.add(id(obj))

        # Handle different data types
        if isinstance(obj, dict):
            result = {}
            for key, value in obj.items():
                result[str(key)] = aux(value)

            return result

        elif isinstance(obj, (list, tuple, set, frozenset)):
            return [aux(item) for item in obj]

        elif callable(obj):
            return f"{obj}"

        else:

            def include_attr(name: str) -> bool:
                has_double = name.startswith("__")
                has_single = name.startswith("_")
                is_regular = not has_single
                return (has_double and include_double_underscore_attrs) or (
                    not has_double and has_single and
                    include_single_underscore_attrs) or is_regular

            try:
                hasattr(obj, "__dict__")

            except TypeError as e:
                if "Unregistered" in str(e):
                    return str(e)

                else:
                    raise e from None

            if hasattr(obj, "__dict__"):
                result = {}
                for key, value in obj.__dict__.items():
                    if include_attr(key):
                        result[key] = aux(value)
                result["__type__"] = str(type(obj))
                return result

            elif hasattr(obj, "__slots__"):
                result = {}
                for slot_name in obj.__slots__:
                    if include_attr(slot_name):
                        value = getattr(obj, slot_name)
                        result[slot_name] = aux(value)
                result["__type__"] = str(type(obj))
                return result

            elif hasattr(obj, "keys") and hasattr(obj, "__getitem__"):
                keys = obj.keys()
                if all(isinstance(it, str) for it in keys):
                    return {key: aux(obj[key]) for key in keys}

                else:
                    return [[aux(key), aux(obj[key])] for key in keys]

            elif hasattr(obj, "__len__") and hasattr(obj, "__getitem__"):
                return [aux(obj[i]) for i in range(0, len(obj))]

            elif hasattr(obj, "__iter__"):
                return [aux(item) for item in obj]

            elif hasattr(obj, "__str__") or hasattr(obj, "__repr__"):
                return f"{type(obj)} = {obj}"

            elif hasattr(obj, "__int__"):
                return f"{type(obj)} = {int(obj)}"

            else:
                return f"unhandled {type(obj)}"

    return aux(obj)


def dumps_with_type(
    obj: Any,
    include_single_underscore_attrs: bool = False,
    include_double_underscore_attrs: bool = False,
    skip_cyclic_data: bool = True,
    override_callback: Optional[Callable] = None,
    with_stable_formatting: bool = True,
) -> Any:
    return json.dumps(
        dump_with_type(
            obj=obj,
            include_single_underscore_attrs=include_single_underscore_attrs,
            include_double_underscore_attrs=include_double_underscore_attrs,
            skip_cyclic_data=skip_cyclic_data,
            override_callback=override_callback,
            with_stable_formatting=with_stable_formatting,
        ),
        indent=2,
        sort_keys=True,
    )


class ExceptionDump:
    """
    If context body raises an exception, add a full dump of the exception object
    """

    def __enter__(self) -> "ExceptionDump":
        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> Literal[False]:
        import json
        if exc_value is not None:
            if not hasattr(exc_value, "__notes__"):
                exc_value.__notes__ = []
            exc_value.__notes__.append(json.dumps(dump_with_type(exc_value), indent=2))

        return False


class ExceptionContextNote:
    """
    If context body raises an exception, add a note to it.
    """

    def __init__(self, note: str | Callable) -> None:
        self.note = note

    def __enter__(self) -> "ExceptionContextNote":
        return self

    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> Literal[False]:
        if exc_value is not None:
            if not hasattr(exc_value, "__notes__"):
                exc_value.__notes__ = []
            exc_value.__notes__.append(
                self.note if isinstance(self.note, str) else self.note())

        return False


def _human_age(dt: datetime, now: datetime) -> str:
    delta = now - dt
    seconds = int(delta.total_seconds())
    if seconds < 60:
        return f"{seconds} seconds ago"

    minutes = seconds // 60
    if minutes < 60:
        return f"{minutes} minutes ago"

    hours = minutes // 60
    if hours < 24:
        return f"{hours} hours ago"

    days = hours // 24
    return f"{days} days ago"


def _format_timestamp_relative(ts: float) -> str:
    dt = datetime.fromtimestamp(ts, tz=timezone.utc).astimezone()
    now = datetime.now(tz=dt.tzinfo)
    return f"{dt.isoformat(timespec='seconds')} ({_human_age(dt, now)})"
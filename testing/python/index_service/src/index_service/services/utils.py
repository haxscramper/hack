from dataclasses import dataclass
import logging
from beartype.typing import Any
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
                    arg_names = frame.f_code.co_varnames[:frame.f_code.
                                                         co_argcount]
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
                        max(
                            len(arg_name)
                            for arg_name in frame_info.args.keys()),
                    )

        display_filenames = [
            Path(frame_info.filename).resolve().absolute()
            for frame_info in frames_info
        ]
        common_prefix = (os.path.commonpath(display_filenames)
                         if display_filenames else "")

        for frame_info in frames_info:
            display_filename = frame_info.filename
            if common_prefix:
                try:
                    display_filename = os.path.relpath(frame_info.filename,
                                                       common_prefix)
                except ValueError:
                    display_filename = frame_info.filename

            header = (f'File "{display_filename}", line {frame_info.line_no}, '
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

                        formatted_value = pprint.pformat(arg_value,
                                                         width=available_width)
                        value_lines = formatted_value.split("\n")

                        lines.append(
                            f"  {arg_name}{type_info:<{max_arg_name_width - len(arg_name)}} = {value_lines[0]}"
                        )
                        for line in value_lines[1:]:
                            lines.append(
                                f"  {'':<{max_arg_name_width}}   {line}")

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
            "alembic.runtime.plugins",
            "alembic.runtime.migration",
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
            "httpcore.http11",
            "httpx",
            "faker.factory",
    ]:
        logger = logging.getLogger(logger_name)
        logger.disabled = True

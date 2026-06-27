import logging
import os
import re
import traceback
from dataclasses import dataclass

import pytest
from beartype.typing import Any
from rich.console import Console

from index_service.db import IndexDatabase
from index_service.harness import BaseResourceActor
from index_service.resources.flm_gemma import FlmSummaryResult, SummarizeRequest
from index_service.runtime import IndexRuntime

ARANGO_HOST = "http://localhost:8529"
ARANGO_USER = "root"
ARANGO_PASSWORD = "test"


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

            if not any(it in filename
                       for it in ["pykka", "pluggy", "_pytest", "<@beartype"]):
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

        display_filenames = [frame_info.filename for frame_info in frames_info]
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


handler = get_custom_traceback_handler(show_args=False, )


def pytest_configure(config: Any) -> None:
    "nodoc"
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
    ]:
        logger = logging.getLogger(logger_name)
        logger.disabled = True


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()

    if report.when != "call" or report.passed:
        return

    excinfo = call.excinfo
    if excinfo is None:
        return

    exc_type, exc_value, exc_tb = excinfo._excinfo
    report.longrepr = str(handler(exc_type, exc_value, exc_tb))


def _safe_name(raw: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_]+", "_", raw).lower()


@pytest.fixture
def db(request) -> IndexDatabase:
    db_name = f"index_test_{_safe_name(request.node.name)}"
    IndexDatabase.reset_database(
        host=ARANGO_HOST,
        db_name=db_name,
        username=ARANGO_USER,
        password=ARANGO_PASSWORD,
    )
    return IndexDatabase(
        host=ARANGO_HOST,
        db_name=db_name,
        username=ARANGO_USER,
        password=ARANGO_PASSWORD,
    )


class MockFlmGemmaResourceActor(BaseResourceActor):
    actor_id = "flm-gemma"

    def handle(self, request: SummarizeRequest) -> FlmSummaryResult:
        text = request.text.strip().replace("\n", " ")
        return FlmSummaryResult(summary=f"mock-summary: {text[:48]}")


class RuntimeWithMockFlm(IndexRuntime):

    def __init__(self) -> None:
        super().__init__(
            resource_overrides={"flm-gemma": MockFlmGemmaResourceActor})


@pytest.fixture
def runtime() -> RuntimeWithMockFlm:
    rt = RuntimeWithMockFlm()
    try:
        yield rt
    finally:
        rt.stop()

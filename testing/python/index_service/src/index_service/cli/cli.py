import logging
from pathlib import Path

import click

from index_service.gui.collection_views.comfy_input_builder import ComfyInputWidgetBuilder
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.window import MainWindow
from index_service.services.db import IndexDatabase
import sys
from beartype.typing import Any
import warnings
import traceback

from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer
from index_service.services.indexers.pdf_indexer import PdfIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.job_types import RunContext
from index_service.services.resources.pdf.pdf_extractor import PdfExtractor
from index_service.services.types import FileRef
from index_service.services.default_job_types import DEFAULT_INDEXER_TYPES, DEFAULT_RESOURCE_TYPES
from index_service.services.resources.wd_tagger import WdTagger
from index_service.services.job_runtime import IndexRuntime
from index_service.services.utils import get_custom_traceback_handler, stfu_logs
from PySide6.QtWidgets import QApplication

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)


def showwarning(message, category, filename, lineno, file=None, line=None):
    print(f"{filename}:{lineno}: {category.__name__}: {message}")
    traceback.print_stack()


warnings.showwarning = showwarning


def _db_options(f):
    f = click.option("--host",
                     default="http://localhost:8529",
                     show_default=True)(f)
    f = click.option("--db-name", required=True)(f)
    f = click.option("--username", default="root", show_default=True)(f)
    f = click.option("--password", default="test", show_default=True)(f)
    return f


@click.group()
def main() -> None:
    pass


def _expand_path(
    _ctx: click.Context,
    _param: click.Parameter,
    value: tuple[str, ...],
) -> tuple[Path, ...]:
    return tuple(Path(path).expanduser() for path in value)


@main.command()
@_db_options
@click.option(
    "--indexer",
    "indexers",
    multiple=True,
    default=[cls.asset_name for cls in DEFAULT_INDEXER_TYPES],
)
@click.argument(
    "paths",
    nargs=-1,
    type=click.Path(path_type=str),
    callback=_expand_path,
)
@click.option("--reset", default=False, show_default=True)
@click.option("--limit-total", default=None, show_default=True, type=click.INT)
@click.option("--limit-per-path",
              default=None,
              show_default=True,
              type=click.INT)
@click.option("--perf-trace-file", default=None, show_default=True)
def index(
    host: str,
    db_name: str,
    username: str,
    password: str,
    reset: bool,
    limit_total: int | None,
    limit_per_path: int | None,
    indexers: tuple[str, ...],
    paths: tuple[Path, ...],
    perf_trace_file: str | None = None,
) -> None:
    stfu_logs()
    handler = get_custom_traceback_handler(show_args=False)

    paths = tuple(p.expanduser().resolve().absolute() for p in paths)

    if reset:
        IndexDatabase.reset_database(
            host=host,
            db_name=db_name,
            username=username,
            password=password,
        )

    def impl(exc_type: Any, exc_value: Any, exc_traceback: Any):
        print(handler(exc_type, exc_value, exc_traceback))

    sys.excepthook = impl

    db = IndexDatabase(
        host=host,
        db_name=db_name,
        username=username,
        password=password,
    )

    count = 0
    ctx = RunContext()
    ctx.tracer.start()

    runner = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[t() for t in DEFAULT_INDEXER_TYPES] + [
            WdTagIndexer(),
            PdfIndexer(),
            FFProbeIndexer(),
        ],
        resource_types=[t() for t in DEFAULT_RESOURCE_TYPES] + [
            WdTagger.from_huggingface(),
            PdfExtractor(),
        ],
    )

    def index_file(file: Path):
        nonlocal count
        if limit_total and limit_total < count:
            return

        count += 1
        runner.run_indexer(db.as_ref(file), list(indexers))

    def run_indexing():
        for path in paths:
            if path.is_file():
                index_file(path)
            else:
                count_per_path = 0
                for file in path.rglob("*"):
                    if file.is_file():
                        if limit_per_path and limit_per_path < count_per_path:
                            continue

                        count_per_path += 1
                        index_file(file)

    try:
        run_indexing()

    finally:
        ctx.tracer.stop()
        if perf_trace_file:
            ctx.tracer.save(perf_trace_file)


@main.command()
@_db_options
def view(host: str, db_name: str, username: str, password: str) -> None:
    stfu_logs()
    app = QApplication(sys.argv)

    db = IndexDatabase(
        host=host,
        db_name=db_name,
        username=username,
        password=password,
    )

    win = MainWindow(
        db,
        collection_names=[t.asset_name for t in DEFAULT_INDEXER_TYPES] + [
            WdTagIndexer.asset_name,
            PdfIndexer.asset_name,
        ],
        builders=[
            ComfyInputWidgetBuilder(),
            WdTaggerWidgetBuilder(),
        ],
    )

    win.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()

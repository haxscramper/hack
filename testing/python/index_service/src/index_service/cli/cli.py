import logging
import sys
import traceback
import warnings
from datetime import datetime
from pathlib import Path

import click
from beartype.typing import Any
from PySide6.QtWidgets import QApplication

from index_service.gui.collection_views.comfy_input_builder import (
    ComfyInputWidgetBuilder, )
from index_service.gui.collection_views.exif_preview_builder import (
    ExifPreviewrWidgetBuilder, )
from index_service.gui.collection_views.wd_tagger_builder import WdTaggerWidgetBuilder
from index_service.gui.window import MainWindow
from index_service.services.core.db import IndexDatabase
from index_service.services.core.indexing_flow import run_indexing_per_root_plan
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import RunContext
from index_service.services.core.types import FileRef, RootRef
from index_service.services.default_job_types import (
    DEFAULT_INDEXER_TYPES,
    DEFAULT_RESOURCE_TYPES,
)
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer
from index_service.services.indexers.ffprobe_indexer import FFProbeIndexer
from index_service.services.indexers.image_generation import GenerationParamsIndexer
from index_service.services.indexers.pdf_indexer import PdfIndexer
from index_service.services.indexers.safetensor_indexer import SafetensorIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.resources.pdf.pdf_extractor import PdfExtractor
from index_service.services.resources.wd_tagger import WdTagger
from index_service.services.utils import (
    get_custom_traceback_handler,
    get_xdg_cache_dir,
    stfu_logs,
)

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)

log = logging.getLogger(__name__)


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


_INDEXER_TYPES = [t for t in DEFAULT_INDEXER_TYPES] + [
    WdTagIndexer,
    PdfIndexer,
    FFProbeIndexer,
    SafetensorIndexer,
    GenerationParamsIndexer,
    ExifMetadataIndexer,
    ComfyInputIndexer,
]


@main.command()
@_db_options
@click.option(
    "--indexer",
    "indexers",
    multiple=True,
    default=[cls.asset_name for cls in _INDEXER_TYPES],
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
@click.option("--enable-cache",
              multiple=True,
              default=[cls.asset_name for cls in _INDEXER_TYPES])
def index(
    host: str,
    db_name: str,
    username: str,
    password: str,
    reset: bool,
    enable_cache: tuple[str, ...],
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
    ctx = RunContext(db)
    ctx.start_trace()

    with ctx.trace_scope("create runner"):
        runner = IndexRuntime(
            ctx=ctx,
            db=db,
            indexer_types=[
                t(should_load_cache=t.asset_name in enable_cache)
                for t in _INDEXER_TYPES
            ],
            resource_types=[t() for t in DEFAULT_RESOURCE_TYPES] + [
                WdTagger.from_huggingface(),
                PdfExtractor(),
            ],
        )

    try:
        run_indexing_per_root_plan(
            db=db,
            runner=runner,
            ctx=ctx,
            paths=paths,
            indexers=indexers,
            limit_total=limit_total,
            limit_per_path=limit_per_path,
        )

    finally:
        assert ctx.writer
        perf_dir = get_xdg_cache_dir([
            "logs",
            "perf",
        ])
        perf_dir.mkdir(parents=True, exist_ok=True)

        ctx.writer.save(
            str(perf_dir.joinpath(f"{datetime.now().isoformat()}.json")))

        perf_files = sorted(
            perf_dir.glob("*.json"),
            key=lambda path: path.stat().st_mtime,
            reverse=True,
        )

        for old_file in perf_files[10:]:
            old_file.unlink()

        if perf_trace_file:
            log.info(f"Trace file {perf_trace_file}")
            ctx.writer.save(perf_trace_file)


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
        collection_names=[t.asset_name for t in _INDEXER_TYPES],
        builders=[
            ComfyInputWidgetBuilder(),
            WdTaggerWidgetBuilder(),
            ExifPreviewrWidgetBuilder(),
        ],
    )

    win.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()

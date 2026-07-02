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
from index_service.services.indexers.full_document import DocumentBlockIndexer
from index_service.services.indexers.image_generation import GenerationParamsIndexer
from index_service.services.indexers.pdf_indexer import PdfIndexer
from index_service.services.indexers.safetensor_indexer import SafetensorIndexer
from index_service.services.indexers.wd_indexer import WdTagIndexer
from index_service.services.log_config import JsonlFormatter, keep_last_files
from index_service.services.resources.flm_server import FlmServerResource
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
    DocumentBlockIndexer,
]

_RESOURCE_TYPES = [t for t in DEFAULT_RESOURCE_TYPES] + [
    FlmServerResource,
    WdTagger,
    PdfExtractor,
]


@main.command()
@_db_options
@click.option(
    "--indexer",
    "indexers",
    multiple=True,
    default=[cls.asset_name for cls in _INDEXER_TYPES],
    type=click.Choice([cls.asset_name for cls in _INDEXER_TYPES],
                      case_sensitive=True),
)
@click.option(
    "--resource",
    "resources",
    multiple=True,
    default=[cls.resource_key for cls in _RESOURCE_TYPES],
    type=click.Choice([cls.resource_key for cls in _RESOURCE_TYPES],
                      case_sensitive=True),
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
@click.option("--logfile",
              default=None,
              show_default=True,
              type=click.Path(path_type=str))
@click.option(
    "--logfile-format",
    type=click.Choice(["text", "json"], case_sensitive=True),
    default="text",
    show_default=True,
)
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
    resources: tuple[str, ...],
    paths: tuple[Path, ...],
    perf_trace_file: str | None = None,
    logfile: str | None = None,
    logfile_format: str = "text",
) -> None:
    stfu_logs()
    handler = get_custom_traceback_handler(show_args=False)

    run_text_dir = get_xdg_cache_dir(["logs", "run", "text"])
    run_json_dir = get_xdg_cache_dir(["logs", "run", "json"])
    perf_dir = get_xdg_cache_dir(["logs", "perf"])

    run_text_dir.mkdir(parents=True, exist_ok=True)
    run_json_dir.mkdir(parents=True, exist_ok=True)
    perf_dir.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().isoformat()
    text_log_format = (
        "%(asctime)s %(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s"
    )

    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG)

    run_text_file = run_text_dir / f"{timestamp}.log"
    run_text_handler = logging.FileHandler(run_text_file, mode="w+")
    run_text_handler.setFormatter(logging.Formatter(text_log_format))
    root_logger.addHandler(run_text_handler)

    run_json_file = run_json_dir / f"{timestamp}.jsonl"
    run_json_handler = logging.FileHandler(run_json_file, mode="w+")
    run_json_handler.setFormatter(JsonlFormatter())
    root_logger.addHandler(run_json_handler)

    if logfile:
        user_handler = logging.FileHandler(logfile, mode="w+")
        if logfile_format == "json":
            user_handler.setFormatter(JsonlFormatter())
        else:
            user_handler.setFormatter(logging.Formatter(text_log_format))
        root_logger.addHandler(user_handler)

    keep_last_files(run_text_dir, "*.log", 20)
    keep_last_files(run_json_dir, "*.jsonl", 20)
    keep_last_files(perf_dir, "*.json", 20)

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

    ctx = RunContext(db)
    ctx.start_trace()

    resource_instances = list()
    for t in _RESOURCE_TYPES:
        if t.resource_key in resources:
            for dep in t.required_resources:
                assert dep in resources, (
                    f"Indexer '{t.resource_key}' requires resource '{dep}' to be enabled"
                )

            if t == WdTagger:
                resource_instances.append(WdTagger.from_huggingface())
            else:
                resource_instances.append(t())

    indexer_instances = list()
    for t in _INDEXER_TYPES:
        if t.asset_name in indexers:
            for dep in t.required_resources:
                assert dep in resources, (
                    f"Indexer '{t.asset_name}' requires resource '{dep}' to be enabled"
                )

            for dep in t.required_assets:
                assert dep in indexers, (
                    f"Indexer '{t.asset_name}' requires indexer '{dep}' to be enabled"
                )

            indexer_instances.append(
                t(should_load_cache=t.asset_name in enable_cache))

    with ctx.trace_scope("create runner"):
        runner = IndexRuntime(
            ctx=ctx,
            db=db,
            indexer_types=indexer_instances,
            resource_types=resource_instances,
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

        ctx.writer.save(
            str(perf_dir.joinpath(f"{datetime.now().isoformat()}.json")))

        keep_last_files(perf_dir, "*.json", 20)

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

import logging
from pathlib import Path

import click

from index_service.services.dagster_defs import DagsterIndexRunner
from index_service.services.db import IndexDatabase
import sys
from beartype.typing import Any

from index_service.services.registry import DEFAULT_INDEXER_TYPES
from index_service.services.utils import get_custom_traceback_handler

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)

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
]:
    logger = logging.getLogger(logger_name)
    logger.disabled = True


@click.command()
@click.option("--host", default="http://localhost:8529", show_default=True)
@click.option("--db-name", required=True)
@click.option("--username", default="root", show_default=True)
@click.option("--password", default="test", show_default=True)
@click.option(
    "--indexer",
    "indexers",
    multiple=True,
    default=[cls.asset_name for cls in DEFAULT_INDEXER_TYPES],
)
@click.argument("paths",
                nargs=-1,
                type=click.Path(exists=True, path_type=Path))
@click.option("--reset", default=False, show_default=True)
@click.option("--limit-total", default=None, show_default=True, type=click.INT)
@click.option("--limit-per-path",
              default=None,
              show_default=True,
              type=click.INT)
def main(
    host: str,
    db_name: str,
    username: str,
    password: str,
    reset: bool,
    limit_total: int,
    limit_per_path: int,
    indexers: tuple[str, ...],
    paths: tuple[Path, ...],
) -> None:

    handler = get_custom_traceback_handler(show_args=False)

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

    db = IndexDatabase(host=host,
                       db_name=db_name,
                       username=username,
                       password=password)
    logging.info(f"starting indexing for {paths}")

    count = 0

    runner = DagsterIndexRunner(indexer_types=DEFAULT_INDEXER_TYPES)

    def index_file(file: Path):
        nonlocal count
        if limit_total and limit_total < count:
            return

        count += 1
        result = runner.run_index_file_job(
            arango=db,
            path=file,
            indexer_names=list(indexers),
        )
        assert result.success

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


if __name__ == "__main__":
    main()

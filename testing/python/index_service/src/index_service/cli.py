import logging
from pathlib import Path

import click

from index_service.dagster_defs import run_index_file_job
from index_service.db import IndexDatabase
import sys
from beartype.typing import Any

from index_service.utils import get_custom_traceback_handler

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
]:
    logger = logging.getLogger(logger_name)
    logger.disabled = True


@click.command()
@click.option("--host", default="http://localhost:8529", show_default=True)
@click.option("--db-name", required=True)
@click.option("--username", default="root", show_default=True)
@click.option("--password", default="test", show_default=True)
@click.option("--indexer", "indexers", multiple=True, required=True)
@click.argument("paths",
                nargs=-1,
                type=click.Path(exists=True, path_type=Path))
@click.option("--reset", default=False, show_default=True)
@click.option("--limit", default=None, show_default=True, type=click.INT)
def main(
    host: str,
    db_name: str,
    username: str,
    password: str,
    reset: bool,
    limit: int,
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

    def index_file(file: Path):
        nonlocal count
        if limit and limit < count:
            return

        count += 1
        result = run_index_file_job(
            arango=db,
            path=file,
            indexer_names=list(indexers),
        )
        assert result.success

    for path in paths:
        if path.is_file():
            index_file(path)

        else:
            for file in path.rglob("*"):
                if file.is_file():
                    index_file(file)


if __name__ == "__main__":
    main()

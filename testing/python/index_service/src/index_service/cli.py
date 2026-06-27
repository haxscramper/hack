import logging
from pathlib import Path

import click

from index_service.dagster_defs import run_index_file_job
from index_service.db import IndexDatabase

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
def main(
    host: str,
    db_name: str,
    username: str,
    password: str,
    indexers: tuple[str, ...],
    paths: tuple[Path, ...],
) -> None:
    db = IndexDatabase(host=host,
                       db_name=db_name,
                       username=username,
                       password=password)
    logging.info(f"starting indexing for {paths}")
    for path in paths:
        for file in path.rglob("*"):
            if not file.is_file():
                continue
            md5 = db.add_path(file)
            result = run_index_file_job(
                arango=db,
                md5=md5,
                paths=file,
                indexer_names=list(indexers),
            )
            assert result.success


if __name__ == "__main__":
    main()

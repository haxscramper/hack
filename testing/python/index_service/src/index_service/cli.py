import hashlib
from pathlib import Path

import click

from index_service.contracts import FileRef
from index_service.db import IndexDatabase
from index_service.runtime import IndexRuntime
import logging

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)


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
    runtime = IndexRuntime()
    logging.info(f"starting indexing for {paths}")
    try:
        for path in paths:
            for file in path.rglob("*"):
                md5 = db.add_path(file)
                db.ensure_file(md5, [str(path)])

                outputs = runtime.run_indexers(
                    FileRef(md5=md5, paths=[str(path)]), list(indexers))
                for out in outputs.values():
                    db.store_indexer_result(md5, out.indexer_id,
                                            out.result_type, out.result)
    finally:
        runtime.stop()

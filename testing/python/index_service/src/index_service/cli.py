import hashlib
from pathlib import Path

import click

from index_service.contracts import FileRef
from index_service.db import IndexDatabase
from index_service.runtime import IndexRuntime


def _md5(path: Path) -> str:
    digest = hashlib.md5()
    digest.update(path.read_bytes())
    return digest.hexdigest()


@click.command()
@click.option("--host", default="http://localhost:8529", show_default=True)
@click.option("--db-name", required=True)
@click.option("--username", default="root", show_default=True)
@click.option("--password", default="test", show_default=True)
@click.option("--indexer", "indexers", multiple=True, required=True)
@click.argument("files",
                nargs=-1,
                type=click.Path(exists=True, path_type=Path))
def main(
    host: str,
    db_name: str,
    username: str,
    password: str,
    indexers: tuple[str, ...],
    files: tuple[Path, ...],
) -> None:
    db = IndexDatabase(host=host,
                       db_name=db_name,
                       username=username,
                       password=password)
    runtime = IndexRuntime()
    try:
        for path in files:
            md5 = _md5(path)
            db.ensure_file(md5, [str(path)])
            outputs = runtime.run_indexers(FileRef(md5=md5, paths=[str(path)]),
                                           list(indexers))
            for out in outputs.values():
                db.store_indexer_result(md5, out.indexer_id, out.result_type,
                                        out.result)
    finally:
        runtime.stop()

from __future__ import annotations

from collections.abc import Sequence
from pathlib import Path
from dataclasses import dataclass

from pydantic import BaseModel
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import FileHash
from index_service.services.file_iteration import match_root, RootFilter
import logging

log = logging.getLogger(__name__)

AQL_FILE_PATHS = """
FOR file IN files
  FOR entry IN file.paths
    LET root = DOCUMENT("roots", entry.root.name)
    RETURN {
      path: CONCAT_SEPARATOR("/", root.path, entry.relative),
      hash: entry.hash.hash,
      root: entry.root.name,
      relative: entry.relative, 
    }
"""


@dataclass(slots=True, frozen=True)
class _FilePathRow:
    path: str
    hash: str
    root: str
    relative: str


def fetch_file_paths(
    ctx: RunContext,
    db: IndexDatabase,
    root_filters: Sequence[RootFilter],
) -> list[_FilePathRow]:
    with ctx.trace_scope("fetch file paths"):
        rows: list[_FilePathRow] = []

        for item in db.aql.execute(AQL_FILE_PATHS):  # type: ignore
            path = Path(item["path"])

            if not path.exists():
                continue

            path_str = path.as_posix()
            matched = match_root(path_str, root_filters)
            if matched is None:
                # The initial query pulls all the files from the DB, then the code
                # filters the paths to check if they belong to any of the target
                # roots here.
                continue

            root_filter, relative = matched
            if not relative:
                log.debug(f"could not find relative name for {path_str}")
                continue

            if root_filter.ignore_spec is not None and root_filter.ignore_spec.match_file(
                    relative):
                log.info(f"skipping {path_str} via filter")
                continue

            rows.append(
                _FilePathRow(
                    path=path_str,
                    hash=item["hash"],
                    root=item["root"],
                    relative=relative,
                ))

        return rows


def fetch_indexer_assets(
    ctx: RunContext,
    db: IndexDatabase,
    hashes: Sequence[str],
    indexers: Sequence[BaseIndexer],
) -> dict[str, dict[str, BaseModel]]:
    assets_by_hash: dict[str, dict[str, BaseModel]] = {
        file_hash: {} for file_hash in hashes
    }
    hash_objs = [FileHash(hash=file_hash) for file_hash in hashes]

    with ctx.trace_scope("fetch indexer results"):
        for indexer in indexers:
            available_hashes = [
                file_hash for file_hash in hash_objs
                if db.has_indexer_result(file_hash, indexer)
            ]

            if not available_hashes:
                continue

            with ctx.trace_scope(
                    "batch indexer",
                    asset_name=indexer.asset_name,
                    file_count=len(available_hashes),
            ):
                results = db.get_indexer_result_batch(list(available_hashes), indexer)

            for file_hash, result in zip(available_hashes, results, strict=True):
                if result is not None:
                    assets_by_hash[file_hash.hash][indexer.asset_name] = result

    return assets_by_hash

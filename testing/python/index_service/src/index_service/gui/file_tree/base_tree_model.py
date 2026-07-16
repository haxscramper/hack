from __future__ import annotations

import hashlib
import json
import logging
from collections.abc import Sequence
from pathlib import Path
from typing import Any, Optional
from dataclasses import dataclass

from beartype import beartype
from pydantic import BaseModel
from sqlalchemy import (
    Column,
    MetaData,
    Table,
    Text,
    create_engine,
    delete,
    inspect,
    select,
    bindparam,
)
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
from sqlalchemy.engine import Engine
from sqlalchemy.engine.url import URL

from index_service.gui.file_tree.file_tree_column import (
    FilePathResult,
    FileTreeColumnSpec,
    FileTreeNode,
)
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import FileHash
from index_service.services.pydantic_utils import model_to_json_data, model_from_json_data

log = logging.getLogger(__name__)

AQL_FILE_PATHS = """
FOR file IN files
  FOR entry IN file.paths
    LET root = DOCUMENT("roots", entry.root.name)
    RETURN {
      path: CONCAT_SEPARATOR("/", root.path, entry.relative),
      hash: entry.hash.hash
    }
"""

CACHE_SCHEMA_TABLE = "file_tree_column_schemas"
CACHE_FILE_TABLE = "file_tree_file_columns"
CACHE_PATH_TABLE = "file_tree_paths"


@dataclass(slots=True, frozen=True)
class _FilePathRow:
    path: str
    hash: str


def _column_schema_hash(column: FileTreeColumnSpec) -> str:
    schema = column.column_type.model_json_schema()

    encoded_schema = json.dumps(
        schema,
        sort_keys=True,
        separators=(",", ":"),
    ).encode()

    return hashlib.sha256(encoded_schema).hexdigest()


def _validate_columns(columns: Sequence[FileTreeColumnSpec]) -> None:
    names = [column.column_name for column in columns]

    if len(set(names)) != len(names):
        raise ValueError("File tree column names must be unique")

    if "hash" in names:
        raise ValueError(
            "'hash' is reserved by the file tree cache and cannot be a column name",)


def _create_cache_engine(cache_path: Path) -> Engine:
    cache_path.parent.mkdir(parents=True, exist_ok=True)

    return create_engine(
        URL.create(
            "sqlite+pysqlite",
            database=str(cache_path),
        ),
        hide_parameters=True,
    )


def _build_cache_tables(
    columns: Sequence[FileTreeColumnSpec],) -> tuple[MetaData, Table, Table, Table]:
    metadata = MetaData()

    schema_table = Table(
        CACHE_SCHEMA_TABLE,
        metadata,
        Column("column_name", Text, primary_key=True),
        Column("schema_hash", Text, nullable=False),
    )

    file_table = Table(
        CACHE_FILE_TABLE,
        metadata,
        Column("hash", Text, primary_key=True),
        *[Column(column.column_name, Text, nullable=False) for column in columns],
    )

    path_table = Table(
        CACHE_PATH_TABLE,
        metadata,
        Column("path", Text, primary_key=True),
        Column("hash", Text, nullable=False),
    )

    return metadata, schema_table, file_table, path_table


def _drop_cache_database(engine: Engine) -> None:
    existing_metadata = MetaData()
    existing_metadata.reflect(bind=engine)
    existing_metadata.drop_all(bind=engine)


def _initialize_cache(
    cache_path: Path,
    columns: Sequence[FileTreeColumnSpec],
) -> tuple[Engine, Table, Table, Table]:
    _validate_columns(columns)

    engine = _create_cache_engine(cache_path)
    metadata, schema_table, file_table, path_table = _build_cache_tables(columns)

    expected_column_names = {column.column_name for column in columns}
    expected_schema_hashes = {
        column.column_name: _column_schema_hash(column) for column in columns
    }

    inspector = inspect(engine)
    existing_tables = set(inspector.get_table_names())

    required_tables = {
        CACHE_SCHEMA_TABLE,
        CACHE_FILE_TABLE,
        CACHE_PATH_TABLE,
    }

    cache_needs_rebuild = not required_tables.issubset(existing_tables)

    if not required_tables.issubset(existing_tables):
        log.info(
            f"Existing DB tables {existing_tables} is not a subset of requried tables {required_tables}"
        )

    if not cache_needs_rebuild:
        file_table_columns = {
            column_info["name"] for column_info in inspector.get_columns(CACHE_FILE_TABLE)
        }

        expected_file_table_columns = {"hash", *expected_column_names}

        cache_needs_rebuild = (len(file_table_columns) != len(expected_file_table_columns)
                               or file_table_columns != expected_file_table_columns)

        if len(file_table_columns) != len(expected_file_table_columns):
            log.info(
                f"Expected file table columns len({expected_file_table_columns}) != file table columns len({file_table_columns})"
            )

        if file_table_columns != expected_file_table_columns:
            log.info(
                f"Expected file table columns {expected_file_table_columns} != file table columns {file_table_columns}"
            )

    if cache_needs_rebuild:
        log.info("Need to rebuild cache")
        _drop_cache_database(engine)
        metadata.create_all(engine)

        with engine.begin() as connection:
            connection.execute(
                schema_table.insert(),
                [{
                    "column_name": column_name,
                    "schema_hash": schema_hash,
                } for column_name, schema_hash in expected_schema_hashes.items()],
            )

        return engine, schema_table, file_table, path_table

    with engine.connect() as connection:
        cached_schema_hashes = dict(
            connection.execute(  # type: ignore
                select(
                    schema_table.c.column_name,
                    schema_table.c.schema_hash,
                ),).all(),)

    schema_changed = cached_schema_hashes != expected_schema_hashes

    if schema_changed:
        log.info("Schema changed, dropping tables")
        file_table.drop(engine)
        file_table.create(engine)

        with engine.begin() as connection:
            connection.execute(delete(schema_table))
            connection.execute(
                schema_table.insert(),
                [{
                    "column_name": column_name,
                    "schema_hash": schema_hash,
                } for column_name, schema_hash in expected_schema_hashes.items()],
            )

    log.info("OK")
    return engine, schema_table, file_table, path_table


def _fetch_file_paths(
    ctx: RunContext,
    db: IndexDatabase,
) -> list[_FilePathRow]:
    with ctx.trace_scope("fetch file paths"):
        rows: list[_FilePathRow] = []
        for item in db.aql.execute(AQL_FILE_PATHS):  # type: ignore
            rows.append(_FilePathRow(path=item["path"], hash=item["hash"]))
        return rows


def _store_path_hashes(
    engine: Engine,
    path_table: Table,
    file_paths: Sequence[_FilePathRow],
) -> None:
    path_hashes = {result.path: result.hash for result in file_paths}

    path_insert = sqlite_insert(path_table)
    path_upsert = path_insert.on_conflict_do_update(
        index_elements=[path_table.c.path],
        set_={"hash": path_insert.excluded.hash},
    )

    delete_path = delete(path_table).where(path_table.c.path == bindparam("stale_path"),)

    with engine.begin() as connection:
        cached_paths = set(connection.scalars(select(path_table.c.path),),)

        if path_hashes:
            connection.execute(
                path_upsert,
                [{
                    "path": path,
                    "hash": file_hash,
                } for path, file_hash in path_hashes.items()],
            )

        stale_paths = cached_paths - path_hashes.keys()

        if stale_paths:
            connection.execute(
                delete_path,
                [{
                    "stale_path": path
                } for path in stale_paths],
            )


def _get_uncached_hashes(
    engine: Engine,
    file_table: Table,
    file_paths: Sequence[_FilePathRow],
) -> list[str]:
    ordered_hashes = list(dict.fromkeys(result.hash for result in file_paths))

    with engine.connect() as connection:
        cached_hashes = set(connection.scalars(select(file_table.c.hash)))

    return [file_hash for file_hash in ordered_hashes if file_hash not in cached_hashes]


def _fetch_indexer_assets(
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
                assets_by_hash[file_hash.hash][indexer.asset_name] = result

    return assets_by_hash


def _store_missing_file_columns(
    ctx: RunContext,
    engine: Engine,
    file_table: Table,
    file_paths: Sequence[_FilePathRow],
    missing_hashes: Sequence[str],
    db: IndexDatabase,
    indexers: Sequence[BaseIndexer],
    columns: Sequence[FileTreeColumnSpec],
) -> None:
    if not missing_hashes:
        return

    representative_paths = {result.hash: result.path for result in file_paths}
    assets_by_hash = _fetch_indexer_assets(ctx, db, missing_hashes, indexers)

    rows: list[dict[str, str]] = []

    with ctx.trace_scope("populate file tree cache", file_count=len(missing_hashes)):
        for hash_value in missing_hashes:
            path = Path(representative_paths[hash_value])
            file_hash = FileHash(hash=hash_value)
            assets = assets_by_hash[hash_value]

            row: dict[str, str] = {"hash": hash_value}

            for column in columns:
                data = column.initColumnData(
                    path=path,
                    hash=file_hash,
                    is_directory=False,
                    assets=assets,
                    nested=[],
                )

                row[column.column_name] = json.dumps(
                    None if data is None else model_to_json_data(data),
                    separators=(",", ":"),
                )

            rows.append(row)

    with engine.begin() as connection:
        connection.execute(
            sqlite_insert(file_table).on_conflict_do_nothing(
                index_elements=[file_table.c.hash],),
            rows,
        )


def _populate_cache(
    ctx: RunContext,
    db: IndexDatabase,
    engine: Engine,
    file_table: Table,
    path_table: Table,
    file_paths: Sequence[_FilePathRow],
    indexers: Sequence[BaseIndexer],
    columns: Sequence[FileTreeColumnSpec],
) -> None:
    _store_path_hashes(engine, path_table, file_paths)

    missing_hashes = _get_uncached_hashes(
        engine,
        file_table,
        file_paths,
    )

    _store_missing_file_columns(
        ctx,
        engine,
        file_table,
        file_paths,
        missing_hashes,
        db,
        indexers,
        columns,
    )


def _load_flat_file_nodes(
    engine: Engine,
    file_table: Table,
    path_table: Table,
    root_directories: Sequence[Path],
    columns: Sequence[FileTreeColumnSpec],
) -> list[tuple[Path, FileTreeNode]]:
    query = (select(
        path_table.c.path,
        path_table.c.hash,
        *[file_table.c[column.column_name] for column in columns],
    ).select_from(path_table.join(
        file_table, path_table.c.hash == file_table.c.hash)).order_by(path_table.c.path))

    roots = sorted(
        ((str(root), root) for root in root_directories),
        key=lambda item: len(item[0]),
        reverse=True,
    )

    flat_nodes: list[tuple[Path, FileTreeNode]] = []

    with engine.connect() as connection:
        for row in connection.execute(query):
            row_data = row._mapping
            path_str = row_data["path"]

            root_path: Path | None = None
            for root_str, root in roots:
                if path_str == root_str or path_str.startswith(root_str + "/"):
                    root_path = root
                    break

            if root_path is None:
                continue

            path = Path(path_str)
            file_hash = FileHash(hash=row_data["hash"])
            node_columns: dict[str, Optional[BaseModel]] = {}

            for column in columns:
                json_data = json.loads(row_data[column.column_name])
                node_columns[column.column_name] = (None if json_data is None else
                                                    model_from_json_data(
                                                        json_data, column.column_type))

            flat_nodes.append((
                root_path,
                FileTreeNode(
                    path=path,
                    is_directory=False,
                    hash=file_hash,
                    columns=node_columns,
                ),
            ))

    return flat_nodes


def _build_directory_tree(
    flat_nodes: Sequence[tuple[Path, FileTreeNode]],
    columns: Sequence[FileTreeColumnSpec],
) -> list[FileTreeNode]:
    files_by_parent: dict[tuple[Path, Path], list[FileTreeNode]] = {}
    directory_paths: dict[Path, set[Path]] = {}

    for root_path, file_node in flat_nodes:
        directory_paths.setdefault(root_path, set()).add(root_path)

        relative_path = file_node.path.relative_to(root_path)
        parent_path = root_path

        for part in relative_path.parts[:-1]:
            next_path = parent_path / part
            directory_paths[root_path].add(next_path)
            parent_path = next_path

        files_by_parent.setdefault(
            (root_path, parent_path),
            [],
        ).append(file_node)

    def build_directory(
        root_path: Path,
        directory_path: Path,
    ) -> FileTreeNode:
        nested: list[FileTreeNode] = []

        child_directories = sorted(
            path for path in directory_paths[root_path]
            if path.parent == directory_path and path != directory_path)

        for child_path in child_directories:
            nested.append(build_directory(root_path, child_path))

        nested.extend(
            sorted(
                files_by_parent.get((root_path, directory_path), []),
                key=lambda node: node.path,
            ),)

        return FileTreeNode(
            path=directory_path,
            is_directory=True,
            hash=None,
            columns={
                column.column_name:
                    column.initColumnData(
                        path=directory_path,
                        hash=None,
                        is_directory=True,
                        assets={},
                        nested=nested,
                    ) for column in columns
            },
            nested=nested,
        )

    return [
        build_directory(root_path, root_path) for root_path in sorted(directory_paths)
    ]


def _load_file_tree_from_cache(
    ctx: RunContext,
    engine: Engine,
    file_table: Table,
    path_table: Table,
    root_directories: Sequence[Path],
    columns: Sequence[FileTreeColumnSpec],
) -> list[FileTreeNode]:
    with ctx.trace_scope("load file tree cache"):
        flat_nodes = _load_flat_file_nodes(
            engine,
            file_table,
            path_table,
            root_directories,
            columns,
        )

    with ctx.trace_scope("arrange file tree"):
        return _build_directory_tree(flat_nodes, columns)


@beartype
def build_file_tree(
    ctx: RunContext,
    db: IndexDatabase,
    root_directories: Sequence[Path],
    indexers: Sequence[BaseIndexer],
    columns: Sequence[FileTreeColumnSpec],
    cache_path: Path,
) -> list[FileTreeNode]:
    file_paths = _fetch_file_paths(ctx, db)

    log.info("Build file tree")

    engine, _, file_table, path_table = _initialize_cache(
        cache_path,
        columns,
    )

    try:
        _populate_cache(
            ctx,
            db,
            engine,
            file_table,
            path_table,
            file_paths,
            indexers,
            columns,
        )

        return _load_file_tree_from_cache(
            ctx,
            engine,
            file_table,
            path_table,
            root_directories,
            columns,
        )
    finally:
        engine.dispose()

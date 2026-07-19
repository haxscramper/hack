from __future__ import annotations

from sqlalchemy import (
    bindparam,)
from sqlalchemy.dialects.sqlite import insert as sqlite_insert

from index_service.gui.file_tree.model.tree_model_fetch import _FilePathRow, fetch_indexer_assets
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import FileHash
from index_service.services.pydantic_utils import model_to_json_data
import hashlib
import json
from collections.abc import Sequence
from pathlib import Path

from sqlalchemy import (
    Column,
    MetaData,
    Table,
    Text,
    delete,
    inspect,
    select,
)
from sqlalchemy.engine import Engine

from index_service.gui.file_tree.columns.file_tree_column import (
    FileTreeColumnSpec,)
from index_service.services.utils import create_cache_engine
import logging

log = logging.getLogger(__name__)

CACHE_SCHEMA_TABLE = "file_tree_column_schemas"
CACHE_FILE_TABLE = "file_tree_file_columns"
CACHE_PATH_TABLE = "file_tree_paths"


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

    duplicates = [name for name in dict.fromkeys(names) if names.count(name) > 1]
    if duplicates:
        raise ValueError(
            f"File tree column names must be unique; duplicate items: {duplicates}")

    if "hash" in names:
        raise ValueError(
            "'hash' is reserved by the file tree cache and cannot be a column name",)


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
        Column("root", Text, nullable=False),
        Column("relative", Text, nullable=False),
    )

    return metadata, schema_table, file_table, path_table


def _drop_cache_database(engine: Engine) -> None:
    existing_metadata = MetaData()
    existing_metadata.reflect(bind=engine)
    existing_metadata.drop_all(bind=engine)


def initialize_cache(
    cache_path: Path,
    columns: Sequence[FileTreeColumnSpec],
) -> tuple[Engine, Table, Table, Table]:
    _validate_columns(columns)

    engine = create_cache_engine(cache_path)
    metadata, schema_table, file_table, path_table = _build_cache_tables(columns)

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
        path_table_columns = {
            column_info["name"] for column_info in inspector.get_columns(CACHE_PATH_TABLE)
        }
        expected_path_table_columns = {"path", "hash", "root", "relative"}

        cache_needs_rebuild = (len(path_table_columns) != len(expected_path_table_columns)
                               or path_table_columns != expected_path_table_columns)

        if len(path_table_columns) != len(expected_path_table_columns):
            log.info(
                f"Expected path table columns len({expected_path_table_columns}) != path table columns len({path_table_columns})"
            )

        if path_table_columns != expected_path_table_columns:
            log.info(
                f"Expected path table columns {expected_path_table_columns} != path table columns {path_table_columns}"
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


def _get_uncached_hashes(
    engine: Engine,
    file_table: Table,
    file_paths: Sequence[_FilePathRow],
) -> list[str]:
    ordered_hashes = list(dict.fromkeys(result.hash for result in file_paths))

    with engine.connect() as connection:
        cached_hashes = set(connection.scalars(select(file_table.c.hash)))

    return [file_hash for file_hash in ordered_hashes if file_hash not in cached_hashes]


def _store_path_hashes(
    engine: Engine,
    path_table: Table,
    file_paths: Sequence[_FilePathRow],
) -> None:
    path_rows = {result.path: result for result in file_paths}

    path_insert = sqlite_insert(path_table)
    path_upsert = path_insert.on_conflict_do_update(
        index_elements=[path_table.c.path],
        set_={
            "hash": path_insert.excluded.hash,
            "root": path_insert.excluded.root,
            "relative": path_insert.excluded.relative,
        },
    )

    delete_path = delete(path_table).where(path_table.c.path == bindparam("stale_path"))

    with engine.begin() as connection:
        cached_paths = set(connection.scalars(select(path_table.c.path)))

        if path_rows:
            connection.execute(
                path_upsert,
                [{
                    "path": row.path,
                    "hash": row.hash,
                    "root": row.root,
                    "relative": row.relative,
                } for row in path_rows.values()],
            )

        stale_paths = cached_paths - path_rows.keys()

        if stale_paths:
            connection.execute(
                delete_path,
                [{
                    "stale_path": path
                } for path in stale_paths],
            )


def store_missing_file_columns(
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
    assets_by_hash = fetch_indexer_assets(ctx, db, missing_hashes, indexers)

    rows: list[dict[str, str]] = []

    with ctx.trace_scope("populate file tree cache", file_count=len(missing_hashes)):
        for hash_value in missing_hashes:
            path = Path(representative_paths[hash_value])
            file_hash = FileHash(hash=hash_value)
            assets = assets_by_hash[hash_value]

            row: dict[str, str] = {"hash": hash_value}

            for column in columns:
                assert path.exists(), str(path)
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


def populate_cache(
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

    store_missing_file_columns(
        ctx,
        engine,
        file_table,
        file_paths,
        missing_hashes,
        db,
        indexers,
        columns,
    )

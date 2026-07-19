from __future__ import annotations

import json
import logging
from collections.abc import Sequence
from pathlib import Path
from typing import Optional

from beartype import beartype
from pydantic import BaseModel
from sqlalchemy import (
    Table,
    select,
)
from sqlalchemy.engine import Engine

from index_service.cli.cli_config import DirConfig
from index_service.gui.file_tree.columns.file_tree_column import (
    FileTreeColumnSpec,
    FileTreeNode,
)
from index_service.gui.file_tree.model.tree_model_cache import initialize_cache, populate_cache
from index_service.gui.file_tree.model.tree_model_fetch import fetch_file_paths
from index_service.gui.file_tree.model.tree_model_user_edits import load_user_edits, apply_user_edits
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import FileHash
from index_service.services.file_iteration import match_root, RootFilter, prepare_root_filters
from index_service.services.pydantic_utils import model_from_json_data

log = logging.getLogger(__name__)


def _load_flat_file_nodes(
    engine: Engine,
    file_table: Table,
    path_table: Table,
    root_filters: Sequence[RootFilter],
    columns: Sequence[FileTreeColumnSpec],
) -> list[tuple[Path, FileTreeNode]]:
    query = (select(
        path_table.c.path,
        path_table.c.hash,
        path_table.c.root,
        path_table.c.relative,
        *[file_table.c[column.column_name] for column in columns],
    ).select_from(path_table.join(
        file_table, path_table.c.hash == file_table.c.hash)).order_by(path_table.c.path))

    flat_nodes: list[tuple[Path, FileTreeNode]] = []

    with engine.connect() as connection:
        for row in connection.execute(query):
            row_data = row._mapping
            path_str = row_data["path"]

            matched = match_root(path_str, root_filters)
            if matched is None:
                continue

            root_filter, _ = matched

            path = Path(path_str)
            file_hash = FileHash(hash=row_data["hash"])
            node_columns: dict[str, Optional[BaseModel]] = {}

            for column in columns:
                json_data = json.loads(row_data[column.column_name])
                node_columns[column.column_name] = (None if json_data is None else
                                                    model_from_json_data(
                                                        json_data, column.column_type))

            flat_nodes.append((
                root_filter.root_path,
                FileTreeNode(
                    path=path,
                    is_directory=False,
                    hash=file_hash,
                    columns=node_columns,
                    root_relative=row_data["relative"],
                    root=row_data["root"],
                ),
            ))

    return flat_nodes


def _build_directory_tree(
    flat_nodes: Sequence[tuple[Path, FileTreeNode]],
    columns: Sequence[FileTreeColumnSpec],
) -> list[FileTreeNode]:
    files_by_parent: dict[tuple[Path, Path], list[FileTreeNode]] = {}
    directory_paths: dict[Path, set[Path]] = {}
    root_names: dict[Path, str] = {}

    for root_path, file_node in flat_nodes:
        if file_node.root is not None:
            root_names.setdefault(root_path, file_node.root)

        directory_paths.setdefault(root_path, set()).add(root_path)

        relative_path = file_node.path.relative_to(root_path)
        parent_path = root_path

        for part in relative_path.parts[:-1]:
            next_path = parent_path / part
            directory_paths[root_path].add(next_path)
            parent_path = next_path

        files_by_parent.setdefault((root_path, parent_path), []).append(file_node)

    def _root_relative(directory_path: Path, root_path: Path) -> str:
        rel = directory_path.relative_to(root_path)
        return "" if rel == Path(".") else rel.as_posix()

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

        assert directory_path.exists()

        return FileTreeNode(
            path=directory_path,
            is_directory=True,
            hash=None,
            root=root_names.get(root_path),
            root_relative=_root_relative(directory_path, root_path),
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


def load_file_tree_from_cache(
    ctx: RunContext,
    engine: Engine,
    file_table: Table,
    path_table: Table,
    root_filters: Sequence[RootFilter],
    columns: Sequence[FileTreeColumnSpec],
) -> list[FileTreeNode]:
    with ctx.trace_scope("load file tree cache"):
        flat_nodes = _load_flat_file_nodes(
            engine,
            file_table,
            path_table,
            root_filters,
            columns,
        )

    with ctx.trace_scope("arrange file tree"):
        return _build_directory_tree(flat_nodes, columns)


@beartype
def build_file_tree(
    ctx: RunContext,
    db: IndexDatabase,
    root_directories: Sequence[DirConfig],
    indexers: Sequence[BaseIndexer],
    columns: Sequence[FileTreeColumnSpec],
    cache_path: Path,
    user_edit_path: Path,
) -> list[FileTreeNode]:
    root_filters = prepare_root_filters(root_directories)
    if not root_filters:
        return []

    file_paths = fetch_file_paths(ctx, db, root_filters)

    log.info("Build file tree")

    engine, _, file_table, path_table = initialize_cache(
        cache_path,
        columns,
    )

    try:
        populate_cache(
            ctx,
            db,
            engine,
            file_table,
            path_table,
            file_paths,
            indexers,
            columns,
        )

        nodes = load_file_tree_from_cache(
            ctx,
            engine,
            file_table,
            path_table,
            root_filters,
            columns,
        )

        user_edit_rows = load_user_edits(user_edit_path, columns)
        apply_user_edits(nodes, columns, user_edit_rows)
        return nodes
    finally:
        engine.dispose()

from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence, Optional
from pydantic import BaseModel, Field

from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec, FileTreeNode, FilePathResult
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import FileHash

import logging

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


@beartype
def build_file_tree(
    ctx: RunContext,
    db: IndexDatabase,
    root_directories: Sequence[Path],
    indexers: Sequence[BaseIndexer],
    columns: Sequence[type[FileTreeColumnSpec]],
) -> list[FileTreeNode]:
    with ctx.trace_scope("fetch file paths"):
        file_paths = [
            FilePathResult.model_validate(item) for item in db.aql.execute(AQL_FILE_PATHS)
        ]

    rooted_paths: list[tuple[Path, FilePathResult]] = []

    for result in file_paths:
        matching_roots = [
            root for root in root_directories if result.path.is_relative_to(root)
        ]

        if matching_roots:
            rooted_paths.append((
                max(matching_roots, key=lambda root: len(root.parts)),
                result,
            ))

    hashes = list(dict.fromkeys(FileHash(hash=result.hash) for _, result in rooted_paths))
    assets_by_hash: dict[str, dict[str, BaseModel]] = {
        file_hash.hash: {} for file_hash in hashes
    }

    with ctx.trace_scope("fetch indexer results"):
        for indexer in indexers:
            available_hashes = [
                file_hash for file_hash in hashes
                if db.has_indexer_result(file_hash, indexer)
            ]

            if not available_hashes:
                continue

            with ctx.trace_scope(
                    "batch indexer",
                    asset_name=indexer.asset_name,
                    file_count=len(available_hashes),
            ):
                results = db.get_indexer_result_batch(
                    available_hashes,
                    indexer,
                )

            for file_hash, result in zip(available_hashes, results, strict=True):
                assets_by_hash[file_hash.hash][indexer.asset_name] = result

    flat_nodes: list[tuple[Path, FileTreeNode]] = []

    with ctx.trace_scope("build flat file nodes"):
        for root_path, result in rooted_paths:
            file_hash = FileHash(hash=result.hash)

            flat_nodes.append((
                root_path,
                FileTreeNode(
                    path=result.path,
                    is_directory=False,
                    hash=file_hash,
                    columns={
                        column.column_name:
                            column.initColumnData(
                                path=result.path,
                                hash=file_hash,
                                assets=assets_by_hash[file_hash.hash],
                                is_directory=False,
                                nested=[],
                            ) for column in columns
                    },
                ),
            ))

    nodes: dict[tuple[Path, Path], FileTreeNode] = {}
    roots: list[FileTreeNode] = []

    with ctx.trace_scope("arrange file tree"):
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

            files_by_parent.setdefault((root_path, parent_path), []).append(file_node)

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

            nested.extend(files_by_parent.get((root_path, directory_path), []),)

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

        roots = [
            build_directory(root_path, root_path) for root_path in sorted(directory_paths)
        ]

    for i, root in enumerate(roots):
        Path(f"/tmp/result_{i}.json").write_text(
            root.model_dump_json(indent=2, serialize_as_any=True))

    return roots

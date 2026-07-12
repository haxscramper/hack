from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence, Optional
from pydantic import BaseModel, Field

from index_service.gui.file_tree.column_model import ColumnSpec
from index_service.gui.file_tree.file_tree_column import FileTreeColumnSpec
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


class FilePathResult(BaseModel):
    path: Path
    hash: str


class FileTreeNode(BaseModel):
    path: Path
    is_directory: bool
    hash: FileHash | None = None
    columns: dict[str, Optional[BaseModel]] = Field(default_factory=dict)
    nested: list["FileTreeNode"] = Field(default_factory=list)


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
        for root_path, file_node in flat_nodes:
            root_key = (root_path, root_path)
            root_node = nodes.get(root_key)

            if root_node is None:
                root_node = FileTreeNode(
                    path=root_path,
                    is_directory=True,
                )
                nodes[root_key] = root_node
                roots.append(root_node)

            relative_path = file_node.path.relative_to(root_path)
            parent = root_node
            current_path = root_path

            for part in relative_path.parts[:-1]:
                current_path /= part
                directory_key = (root_path, current_path)

                directory = nodes.get(directory_key)
                if directory is None:
                    directory = FileTreeNode(
                        path=current_path,
                        is_directory=True,
                    )
                    nodes[directory_key] = directory
                    parent.nested.append(directory)

                parent = directory

            parent.nested.append(file_node)

    for i, root in enumerate(roots):
        Path(f"/tmp/result_{i}.json").write_text(
            root.model_dump_json(indent=2, serialize_as_any=True))

    return roots

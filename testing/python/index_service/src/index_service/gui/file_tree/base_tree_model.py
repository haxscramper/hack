from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence
from pydantic import BaseModel, Field

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
    assets: dict[str, BaseModel] = Field(default_factory=dict)
    nested: list["FileTreeNode"] = Field(default_factory=list)


@beartype
def build_file_tree(
    ctx: RunContext,
    db: IndexDatabase,
    root_directories: Sequence[Path],
    indexers: Sequence[BaseIndexer],
) -> list[FileTreeNode]:
    cursor = db.aql.execute(AQL_FILE_PATHS)

    nodes: dict[tuple[Path, Path], FileTreeNode] = {}
    roots: list[FileTreeNode] = []

    for item in cursor:
        with ctx.trace_scope("add file to tree"):
            result = FilePathResult.model_validate(item)

            matching_roots = [
                root for root in root_directories if result.path.is_relative_to(root)
            ]

            if not matching_roots:
                continue

            log.info(f"OK {result.path}")

            root_path = max(matching_roots, key=lambda root: len(root.parts))
            root_key = (root_path, root_path)

            root_node = nodes.get(root_key)
            if root_node is None:
                root_node = FileTreeNode(
                    path=root_path,
                    is_directory=True,
                )
                nodes[root_key] = root_node
                roots.append(root_node)

            relative_path = result.path.relative_to(root_path)
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

            file_hash = FileHash(hash=result.hash)
            assets: dict[str, BaseModel] = {}

            with ctx.trace_scope("add indexers"):
                for indexer in indexers:
                    if db.has_indexer_result(file_hash, indexer):
                        with ctx.trace_scope("single indexer",
                                             asset_name=indexer.asset_name):
                            assets[indexer.asset_name] = db.get_indexer_result(
                                file_hash,
                                indexer,
                            )

            parent.nested.append(
                FileTreeNode(
                    path=result.path,
                    is_directory=False,
                    hash=file_hash,
                    assets=assets,
                ))

    return roots

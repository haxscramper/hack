from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence
from pydantic import BaseModel, Field

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer
from index_service.services.core.types import FileHash

import logging

log = logging.getLogger(__name__)

AQL_FILE_PATHS = """
FOR root IN roots
  FOR file IN files
    FOR entry IN file.paths
      FILTER entry.root.name == root._key
      RETURN {
        root: root.path,
        path: CONCAT_SEPARATOR("/", root.path, entry.relative),
        relative: entry.relative,
        hash: entry.hash.hash
      }
"""


class FilePathResult(BaseModel):
    root: Path
    path: Path
    relative: Path
    hash: str


class FileTreeNode(BaseModel):
    path: Path
    is_directory: bool
    hash: FileHash | None = None
    assets: dict[str, BaseModel] = Field(default_factory=dict)
    children: list["FileTreeNode"] = Field(default_factory=list)


@beartype
def build_file_tree(
    db: IndexDatabase,
    root_directories: Sequence[Path],
    indexers: Sequence[BaseIndexer],
) -> list[FileTreeNode]:
    cursor = db.aql.execute(AQL_FILE_PATHS)

    nodes: dict[tuple[Path, Path], FileTreeNode] = {}
    roots: list[FileTreeNode] = []

    for root_path in root_directories:
        root_node = FileTreeNode(
            path=root_path,
            is_directory=True,
        )
        nodes[(root_path, Path())] = root_node
        roots.append(root_node)

    for item in cursor:
        log.debug(item)
        result = FilePathResult.model_validate(item)
        matching_roots = [
            root_path for root_path in root_directories
            if result.path.is_relative_to(root_path)
        ]

        if not matching_roots:
            continue

        tree_root_path = max(matching_roots, key=lambda path: len(path.parts))
        relative_path = result.path.relative_to(tree_root_path)
        parent_node = nodes[(tree_root_path, Path())]

        for index, part in enumerate(relative_path.parts[:-1]):
            directory_relative = Path(*relative_path.parts[:index + 1])
            directory_key = (tree_root_path, directory_relative)
            directory_node = nodes.get(directory_key)

            if directory_node is None:
                directory_node = FileTreeNode(
                    path=tree_root_path / directory_relative,
                    is_directory=True,
                )
                nodes[directory_key] = directory_node
                parent_node.children.append(directory_node)

            parent_node = directory_node

        file_hash = FileHash(hash=result.hash)
        assets: dict[str, BaseModel] = {}

        for indexer in indexers:
            if db.has_indexer_result(file_hash, indexer):
                assets[indexer.__class__.__name__] = db.get_indexer_result(
                    file_hash,
                    indexer,
                )

        parent_node.children.append(
            FileTreeNode(
                path=result.path,
                is_directory=False,
                hash=file_hash,
                assets=assets,
            ))

    return roots

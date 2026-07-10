from pathlib import Path

from beartype import beartype
from beartype.typing import Sequence
from pydantic import BaseModel, Field

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_types import BaseIndexer
from index_service.services.core.types import FileHash

AQL_FILE_PATHS = """
FOR root IN roots
  FILTER root.path IN @root_paths
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
    root_paths = [str(root) for root in root_directories]
    cursor = db.aql.execute(
        AQL_FILE_PATHS,
        bind_vars={"root_paths": root_paths},
    )

    nodes: dict[tuple[Path, Path], FileTreeNode] = {}
    roots: list[FileTreeNode] = []

    for root_path in root_directories:
        root_node = FileTreeNode(
            path=root_path,
            is_directory=True,
        )
        nodes[(root_path, Path())] = root_node
        roots.append(root_node)

    for item in cursor:  # type: ignore
        result = FilePathResult.model_validate(item)
        root_node = nodes[(result.root, Path())]
        parent_node = root_node
        relative_parts = result.relative.parts

        for index, part in enumerate(relative_parts[:-1]):
            directory_relative = Path(*relative_parts[:index + 1])
            directory_key = (result.root, directory_relative)
            directory_node = nodes.get(directory_key)

            if directory_node is None:
                directory_node = FileTreeNode(
                    path=result.root / directory_relative,
                    is_directory=True,
                )
                nodes[directory_key] = directory_node
                parent_node.children.append(directory_node)

            parent_node = directory_node

        file_hash = FileHash(hash=result.hash)
        assets: dict[str, BaseModel] = {}

        for indexer in indexers:
            if db.has_indexer_result(file_hash, indexer):
                assets[indexer.__name__] = db.get_indexer_result(file_hash, indexer)

        file_node = FileTreeNode(
            path=result.path,
            is_directory=False,
            hash=file_hash,
            assets=assets,
        )
        parent_node.children.append(file_node)

    return roots

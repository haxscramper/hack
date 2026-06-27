from abc import ABC, abstractmethod
from dataclasses import dataclass, field

from beartype import beartype
from beartype.typing import Dict, List, Tuple

from index_service.protocol import IndexerOutput


@beartype
@dataclass(frozen=True)
class FileRef:
    md5: str
    paths: List[str]


@beartype
@dataclass
class IndexerContext:
    dependency_results: Dict[str, IndexerOutput] = field(default_factory=dict)


@beartype
class BaseIndexer(ABC):
    indexer_id: str
    dependencies: Tuple[str, ...] = ()

    @abstractmethod
    def can_index(self, file_ref: FileRef, ctx: IndexerContext) -> bool:
        return True

    @abstractmethod
    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        raise NotImplementedError

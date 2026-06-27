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

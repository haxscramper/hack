from dataclasses import dataclass

import pydantic
from beartype import beartype
from beartype.typing import Any, Dict, Optional, Protocol

from index_service.services.core.types import FileHash, FullTextIndexConfig


@beartype
class BaseIndexProtocol(Protocol):
    asset_name: str
    result_model: type[pydantic.BaseModel]
    edge_collection_name: Optional[str]

    def get_document_type_bases(self) -> list[Any]:
        ...


@beartype
@dataclass
class IndexerResultRecord:
    hash: FileHash
    indexer_id: str
    result: Dict[str, object]


class FullTextSearchAccessParams(pydantic.BaseModel, extra="forbid"):
    safe_path: str
    view_name: str
    index: FullTextIndexConfig

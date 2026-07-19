import math
from typing import Any, ClassVar, Union

import glom
from beartype import beartype
from pydantic import Field
from beartype.typing import Annotated, Optional
from sqlalchemy import Engine

from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.job_types import BaseIndexer, RunContext, BaseIndexerConfig
from index_service.services.core.types import IndexerOutput, IndexerRequest, MultiDocumentModel, VectorIndexConfig
from index_service.services.indexers.chunk_indexing.chunking import (
    ChunkConfig,
    ChunkUnit,
    Chunker,
    ChunkDocument,
    ChunkFile,
    ChunkLink,
    chunks_to_multidoc,
)
from index_service.services.indexers.full_document.full_document import (
    DocumentBlockIndexerResult,)


class EmbeddingChunk(ChunkDocument, extra="forbid"):
    # Placeholder embedding. Will be replaced by an embedding-gemma 768-d vector.
    vector: list[float] = Field(default_factory=list)
    dim: int = 0

    vector_index: ClassVar[Optional[VectorIndexConfig]] = VectorIndexConfig(
        vector_dimensions=768,
        vector_metric="cosine",
        index_path="result.vector",
    )


class FileEmbeddingIndexerResult(MultiDocumentModel, extra="forbid"):
    document_type: ClassVar[Any] = Annotated[
        Union[ChunkFile, EmbeddingChunk],
        Field(discriminator="type"),
    ]
    edge_type: ClassVar[Any] = ChunkLink


def _placeholder_vector(text: str) -> list[float]:
    buckets = [0.0] * 768
    for ch in text.lower():
        buckets[ord(ch) % 768] += 1.0
    norm = math.sqrt(sum(x * x for x in buckets))
    return buckets if norm == 0.0 else [x / norm for x in buckets]


class FileEmbeddingIndexerConfig(BaseIndexerConfig, extra="forbid"):
    unit: ChunkUnit = ChunkUnit.CHARS
    max_size: int = 1500
    min_size: int = 300


@beartype
class FileEmbeddingIndexer(BaseIndexer):
    asset_name = "file_embedding"
    result_model = FileEmbeddingIndexerResult
    required_assets = ("document_block",)
    config_model = FileEmbeddingIndexerConfig

    def get_document_type_bases(self) -> list[Any]:
        return [ChunkFile, EmbeddingChunk]

    def __init__(self, database: Engine, config: FileEmbeddingIndexerConfig) -> None:
        super().__init__(config=config, database=database)

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        full_document = glom.glom(assets, "document_block.result", default=None)
        file_hash = request.get_hash_str()
        chunker = Chunker(self._config)

        if isinstance(full_document, DocumentBlockIndexerResult):
            chunks = chunker.chunk_blocks(full_document.documents, full_document.edges,
                                          file_hash)
        else:
            text = ctx.get_path(request.file_ref).read_text()
            chunks = chunker.chunk_text(text, file_hash)

        documents, edges = chunks_to_multidoc(
            chunks,
            file_hash,
            EmbeddingChunk,
            per_chunk=lambda c: {
                "vector": _placeholder_vector(c.text),
                "dim": 8,
            },
        )
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileEmbeddingIndexerResult(documents=documents, edges=edges),
        )

from typing import Any, ClassVar, Union

from sqlalchemy import Engine

import glom
from pydantic import Field
from beartype.typing import Annotated, Optional

from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.job_types import BaseIndexer, BaseIndexerConfig, RunContext
from index_service.services.core.types import FullTextIndexConfig, IndexerOutput, IndexerRequest, MultiDocumentModel, VectorIndexConfig
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

import logging

log = logging.getLogger(__name__)


class FullTextChunk(ChunkDocument, extra="forbid"):
    full_text_index: ClassVar[Optional[FullTextIndexConfig]] = FullTextIndexConfig(
        index_path="result.text",)


class FullTextIndexerResult(MultiDocumentModel, extra="forbid"):
    document_type: ClassVar[Any] = Annotated[
        Union[ChunkFile, FullTextChunk],
        Field(discriminator="type"),
    ]
    edge_type: ClassVar[Any] = ChunkLink


class FullTextIndexer(BaseIndexer):
    asset_name = "full_text"
    result_model = FullTextIndexerResult
    required_assets = ("document_block",)

    def get_document_type_bases(self) -> list[Any]:
        return [ChunkFile, FullTextChunk]

    def __init__(self, config: BaseIndexerConfig, database: Engine) -> None:
        super().__init__(config=config, database=database)
        self._config = ChunkConfig(
            unit=ChunkUnit.CHARS,
            max_size=2000,
            min_size=500,
            start_overlap_min=0,
            start_overlap_max=200,
        )

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        full_document = glom.glom(assets, "document_block.result", default=None)
        assert full_document is None or isinstance(
            full_document,
            DocumentBlockIndexerResult), (f"Input document type is {type(full_document)}")

        file_hash = request.get_hash_str()

        # log.info(
        #     f"document block result is {full_document.model_dump_json(indent=2, serialize_as_any=True)}"
        # )

        if full_document:
            chunker = Chunker(self._config)
            chunks = chunker.chunk_blocks(full_document.documents, full_document.edges,
                                          file_hash)
        else:
            chunks = []

        log.debug(f"created {len(chunks)} chunks")

        documents, edges = chunks_to_multidoc(chunks, file_hash, ChunkDocument)
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FullTextIndexerResult(documents=documents, edges=edges),
        )

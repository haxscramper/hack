from typing import Any, ClassVar, Union

import glom
from pydantic import Field
from beartype.typing import Annotated

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import IndexerOutput, IndexerRequest, MultiDocumentModel
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


class FullTextIndexerResult(MultiDocumentModel, extra="forbid"):
    document_type: ClassVar[Any] = Annotated[
        Union[ChunkFile, ChunkDocument],
        Field(discriminator="type"),
    ]
    edge_type: ClassVar[Any] = ChunkLink


class FullTextIndexer(BaseIndexer):
    asset_name = "full_text"
    result_model = FullTextIndexerResult
    required_assets = ("document_block",)

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._config = ChunkConfig(
            unit=ChunkUnit.CHARS,
            max_size=2000,
            min_size=500,
            start_overlap_min=0,
            start_overlap_max=200,
        )

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
        chunker = Chunker(self._config)

        if full_document:
            chunks = chunker.chunk_blocks(full_document.documents, full_document.edges,
                                          file_hash)
        else:
            path = ctx.get_path(request.file_ref)
            try:
                text = path.read_text()
            except Exception as e:
                log.error(f"{e} {path}", exc_info=e)
                text = ""
            chunks = chunker.chunk_text(text, file_hash)

        documents, edges = chunks_to_multidoc(chunks, file_hash, ChunkDocument)
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FullTextIndexerResult(documents=documents, edges=edges),
        )

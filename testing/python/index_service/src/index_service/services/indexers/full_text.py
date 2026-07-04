from pathlib import Path

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
from pydantic import BaseModel

from index_service.services.indexers.full_document.full_document import DocumentBlockIndexerResult

import logging

log = logging.getLogger(__name__)


class FullTextIndexerResult(IndexDocument, extra="forbid"):
    text: str


class FullTextIndexer(BaseIndexer):
    asset_name = "full_text"
    result_model = FullTextIndexerResult
    required_assets = ("document_block",)

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        full_document = assets.get("document_block")

        assert full_document.result is None or isinstance(
            full_document.result,
            DocumentBlockIndexerResult), f"Input document type is {type(full_document)}"

        path = ctx.get_path(request.file_ref)
        if full_document:
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=FullTextIndexerResult(text="", hash=request.get_hash_str()),
            )

        else:
            try:
                text = path.read_text()
                return IndexerOutput(
                    indexer_id=self.asset_name,
                    result=FullTextIndexerResult(text=text, hash=request.get_hash_str()),
                )

            except Exception as e:
                log.error(f"{e} {path}", exc_info=e)
                return IndexerOutput(indexer_id=self.asset_name,
                                     result=FullTextIndexerResult(
                                         text=str(e), hash=request.get_hash_str()))

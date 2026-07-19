from pathlib import Path

from sqlalchemy import Engine

from index_service.services.core.job_types import BaseIndexer, BaseIndexerConfig, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileSizeIndexerResult(IndexDocument, extra="forbid"):
    size_bytes: int


class FileSizeIndexer(BaseIndexer):
    asset_name = "file_size"
    result_model = FileSizeIndexerResult

    def __init__(self, config: BaseIndexerConfig, database: Engine) -> None:
        super().__init__(config=config, database=database)

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        size = ctx.get_path(request.file_ref).stat().st_size
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSizeIndexerResult(
                size_bytes=size,
                hash=request.get_hash_str(),
            ),
        )

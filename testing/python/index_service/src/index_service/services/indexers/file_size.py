from pathlib import Path

from index_service.services.job_types import BaseIndexer, RunContext
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileSizeIndexerResult(BaseModel, extra="forbid"):
    size_bytes: int


class FileSizeIndexer(BaseIndexer):
    asset_name = "file_size"
    result_model = FileSizeIndexerResult

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        size = Path(request.file_ref.path).stat().st_size
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSizeIndexerResult(size_bytes=size),
        )

from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileSizeIndexerResult(BaseModel, extra="forbid"):
    size_bytes: int


class FileSizeIndexer(BaseIndexer):
    asset_name = "file_size"
    result_model = FileSizeIndexerResult

    def run(self, request: IndexerRequest,
            **resources: object) -> IndexerOutput:
        size = Path(request.file_ref.path).stat().st_size
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSizeIndexerResult(size_bytes=size),
        )

from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileSizeIndexerResult(BaseModel):
    size_bytes: int


class FileSizeIndexerActor(BaseIndexerActor):
    actor_id = "file-size"

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        size = Path(request.file_ref.paths[0]).stat().st_size
        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result=FileSizeIndexerResult(size_bytes=size),
        )

from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest


class FileSizeIndexerActor(BaseIndexerActor):
    actor_id = "file-size"

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        size = Path(request.file_ref.paths[0]).stat().st_size
        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result={"size_bytes": size},
        )

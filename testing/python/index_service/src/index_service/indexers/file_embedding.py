import math
from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileEmbeddingIndexerResult(BaseModel):
    vector: list[float]
    dim: int


class FileEmbeddingIndexerActor(BaseIndexerActor):
    actor_id = "file-embedding"

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        text = Path(request.file_ref.paths[0]).read_text()
        buckets = [0.0] * 8
        for ch in text.lower():
            buckets[ord(ch) % 8] += 1.0
        norm = math.sqrt(sum(x * x for x in buckets))
        vector = buckets if norm == 0.0 else [x / norm for x in buckets]

        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result=FileEmbeddingIndexerResult(
                vector=vector,
                dim=len(vector),
            ),
        )

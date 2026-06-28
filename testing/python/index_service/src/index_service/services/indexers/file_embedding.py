import math
from pathlib import Path

from index_service.services.job_types import BaseIndexer, RunContext
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileEmbeddingIndexerResult(BaseModel, extra="forbid"):
    vector: list[float]
    dim: int


class FileEmbeddingIndexer(BaseIndexer):
    asset_name = "file_embedding"
    result_model = FileEmbeddingIndexerResult

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        text = Path(request.file_ref.path).read_text()
        buckets = [0.0] * 8
        for ch in text.lower():
            buckets[ord(ch) % 8] += 1.0
        norm = math.sqrt(sum(x * x for x in buckets))
        vector = buckets if norm == 0.0 else [x / norm for x in buckets]
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileEmbeddingIndexerResult(vector=vector, dim=len(vector)),
        )

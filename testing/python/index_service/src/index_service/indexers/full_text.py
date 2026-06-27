from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest
from index_service.resources.file_reverser import ReverseLinesRequest, ReverserResult
from pydantic import BaseModel


class FullTextIndexerResult(BaseModel):
    text: str


class FullTextIndexerActor(BaseIndexerActor):
    actor_id = "full-text"

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        path = Path(request.file_ref.paths[0])
        text = path.read_text()
        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result=FullTextIndexerResult(text=text),
        )

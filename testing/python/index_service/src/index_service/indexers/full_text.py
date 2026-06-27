from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FullTextIndexerResult(BaseModel, extra="forbid"):
    text: str


class FullTextIndexer(BaseIndexer):
    asset_name = "full_text"
    result_model = FullTextIndexerResult

    def run(self, request: IndexerRequest,
            **resources: object) -> IndexerOutput:
        path = Path(request.file_ref.path)
        text = path.read_text()
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FullTextIndexerResult(text=text),
        )

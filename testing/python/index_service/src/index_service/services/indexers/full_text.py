from pathlib import Path

from index_service.services.job_types import BaseIndexer, RunContext
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FullTextIndexerResult(BaseModel, extra="forbid"):
    text: str


class FullTextIndexer(BaseIndexer):
    asset_name = "full_text"
    result_model = FullTextIndexerResult

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        path = ctx.get_path(request.file_ref)
        text = path.read_text()
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FullTextIndexerResult(text=text),
        )

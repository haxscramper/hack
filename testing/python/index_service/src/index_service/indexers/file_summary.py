from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from index_service.resources.flm_gemma import (
    FlmGemmaResource,
    FlmSummaryResult,
    SummarizeRequest,
)
from pydantic import BaseModel


class FileSummaryIndexerResult(BaseModel, extra="forbid"):
    summary: FlmSummaryResult


class FileSummaryIndexer(BaseIndexer):
    asset_name = "file_summary"
    result_model = FileSummaryIndexerResult
    required_resources = ("flm_gemma", )

    def run(
        self,
        request: IndexerRequest,
        flm_gemma: FlmGemmaResource,
        **resources: object,
    ) -> IndexerOutput:
        raw_path = request.file_ref.path
        text = Path(raw_path).read_text()
        summary = flm_gemma.handle(SummarizeRequest(text=text))
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSummaryIndexerResult(summary=summary),
        )

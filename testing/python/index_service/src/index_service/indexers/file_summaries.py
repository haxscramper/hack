from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from index_service.resources.flm_gemma import (
    FlmGemmaResource,
    FlmSummaryResult,
    SummarizeRequest,
)
from pydantic import BaseModel


class FileSummariesIndexerResult(BaseModel):
    summaries: dict[str, str]


class FileSummariesIndexer(BaseIndexer):
    asset_name = "file-summaries"
    result_model = FileSummariesIndexerResult
    required_resources = ("flm_gemma", )

    def run(
        self,
        request: IndexerRequest,
        flm_gemma: FlmGemmaResource,
        **resources: object,
    ) -> IndexerOutput:
        summaries: dict[str, str] = {}
        for raw_path in request.file_ref.paths:
            text = Path(raw_path).read_text()
            summary = flm_gemma.handle(SummarizeRequest(text=text))
            summaries[str(raw_path)] = summary.summary
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSummariesIndexerResult(summaries=summaries),
        )

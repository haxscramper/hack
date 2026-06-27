from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest
from index_service.resources.flm_gemma import FlmSummaryResult, SummarizeRequest
from pydantic import BaseModel


class FileSummariesIndexerResult(BaseModel):
    summaries: dict[str, str]


class FileSummariesIndexerActor(BaseIndexerActor):
    actor_id = "file-summaries"
    required_resources = ("flm-gemma", )

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        summaries: dict[str, str] = {}
        for raw_path in request.file_ref.paths:
            text = Path(raw_path).read_text()
            summary = self.request_resource(
                "flm-gemma",
                SummarizeRequest(text=text),
                FlmSummaryResult,
            )
            summaries[str(raw_path)] = summary.summary
        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result=FileSummariesIndexerResult(summaries=summaries),
        )

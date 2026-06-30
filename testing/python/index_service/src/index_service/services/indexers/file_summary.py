from pathlib import Path

from beartype.typing import cast
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.resources.flm_gemma import (
    FlmGemmaResource,
    FlmSummaryResult,
    SummarizeRequest,
)
from index_service.services.core.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileSummaryIndexerResult(BaseModel, extra="forbid"):
    summary: FlmSummaryResult


class FileSummaryIndexer(BaseIndexer):
    asset_name = "file_summary"
    result_model = FileSummaryIndexerResult
    required_resources = ("flm_gemma", )

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        text = ctx.get_path(request.file_ref).read_text()
        flm_gemma = cast(FlmGemmaResource, resources["flm_gemma"])
        summary = flm_gemma.handle(ctx, SummarizeRequest(text=text))
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSummaryIndexerResult(summary=summary),
        )

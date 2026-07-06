from pathlib import Path

import magic
from beartype.typing import cast
from pydantic import BaseModel

from index_service.services.core.job_types import (
    BaseIndexer,
    RunContext,
    cache_indexer_run,
)
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
from index_service.services.resources.text_summary import (
    SummarizeRequest,
    TextSummaryResource,
    TextSummaryResult,
)
import logging

log = logging.getLogger(__name__)


class FileSummaryIndexerResult(TextSummaryResult, extra="forbid"):
    pass


class FileSummaryIndexer(BaseIndexer):
    asset_name = "file_summary"
    result_model = FileSummaryIndexerResult
    required_resources = ("text_summary",)

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic = magic.Magic(mime=True)

    def can_run(self, path: Path) -> bool:
        if not path.exists():
            return False

        mime = self._magic.from_file(str(path.resolve()))
        if mime.startswith("text/"):
            return True

        else:
            return False

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        text = ctx.get_path(request.file_ref).read_text()
        flm_gemma = cast(TextSummaryResource, resources["text_summary"])
        summary = flm_gemma.handle(
            ctx,
            SummarizeRequest(text=text, file_hash=request.get_hash_str()),
            resources=resources,
        )

        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileSummaryIndexerResult(
                documents=summary.documents,
                edges=summary.edges,
            ),
        )

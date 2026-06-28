from pathlib import Path

from beartype.typing import cast
from index_service.services.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.resources.pdf.pdf_extractor import (
    PdfExtractor,
    PdfExtractorRequest,
    PdfExtractorResult,
    PdfPage,
)
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class PdfIndexerResult(BaseModel, extra="forbid"):
    pages: list[PdfPage]


class PdfIndexer(BaseIndexer):
    asset_name = "pdf_pages"
    result_model = PdfIndexerResult
    required_resources = ("pdf_extractor", )

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def can_run(self, path: Path) -> bool:
        return path.suffix.lower() == ".pdf"

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        extractor: PdfExtractor = cast(PdfExtractor,
                                       resources["pdf_extractor"])
        result: PdfExtractorResult = extractor.handle(
            ctx, PdfExtractorRequest(path=str(request.file_ref.path)))
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=PdfIndexerResult(pages=result.pages),
        )

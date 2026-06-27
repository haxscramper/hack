from __future__ import annotations

import hashlib
import logging
from pathlib import Path
from typing import Annotated, Union

import fitz
from pydantic import BaseModel, Field

from index_service.services.harness import BaseResource, cache_indexer_run
from index_service.services.resources.pdf.docling_extractor import DoclingExtractor, DoclingPage
from index_service.services.resources.pdf.mypdf_extractor import MyPDFExtractor, MyPDFPage
from index_service.services.utils import get_xdg_cache_dir

log = logging.getLogger(__name__)


class PdfExtractorRequest(BaseModel, extra="forbid"):
    path: str
    pages: str | None = None
    ocr_only: bool = False


PdfPage = Annotated[
    Union[MyPDFPage, DoclingPage],
    Field(discriminator="extractor"),
]


class PdfExtractorResult(BaseModel, extra="forbid"):
    pages: list[PdfPage]


class PdfExtractor(BaseResource):
    resource_key = "pdf_extractor"

    def __init__(
        self,
        mypdf_extractor: MyPDFExtractor | None = None,
        docling_extractor: DoclingExtractor | None = None,
    ) -> None:
        self.mypdf_extractor = mypdf_extractor or MyPDFExtractor()
        self.docling_extractor = docling_extractor or DoclingExtractor()

    @staticmethod
    def from_default() -> PdfExtractor:
        return PdfExtractor()

    @staticmethod
    def _parse_page_range(pages: str | None,
                          max_pages: int) -> tuple[int, int]:
        if not pages:
            return 1, max_pages
        parts = pages.split("-")
        first = max(1, int(parts[0]))
        if len(parts) > 1:
            last = min(max_pages, int(parts[1]))
        else:
            last = first
        return first, last

    @staticmethod
    def _raster_dir(pdf_path: Path) -> Path:
        digest = hashlib.md5(str(
            pdf_path.absolute()).encode()).hexdigest()[:16]
        return get_xdg_cache_dir(
            ["pdf_extractor", f"{pdf_path.stem}_{digest}"])

    def handle(self, request: PdfExtractorRequest) -> PdfExtractorResult:
        pdf_path = Path(request.path)
        if not pdf_path.exists():
            raise FileNotFoundError(f"PDF not found: {pdf_path}")

        raster_dir = self._raster_dir(pdf_path)

        with fitz.open(str(pdf_path)) as pdf:
            max_pages = len(pdf)
            first_page, last_page = self._parse_page_range(
                request.pages, max_pages)

            log.info(
                f"Processing PDF: {pdf_path.name} | "
                f"Range: {first_page}-{last_page} ({last_page - first_page + 1} pages)"
            )

            pages: list[MyPDFPage | DoclingPage] = []

            for page_num in range(first_page, last_page + 1):
                if not request.ocr_only and MyPDFExtractor.page_has_selectable_text(
                        pdf, page_num):
                    page = self.mypdf_extractor.extract_page(
                        pdf_path, page_num)
                else:
                    page = self.docling_extractor.extract_page(
                        pdf_path, page_num, raster_dir)
                pages.append(page)

        # Clean up raster images after successful processing
        for f in raster_dir.glob("*.png"):
            f.unlink(missing_ok=True)

        return PdfExtractorResult(pages=pages)

from __future__ import annotations

from loguru import logger
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import pymupdf
from loguru import logger


def render_pdf_page(
    input_pdf: Path,
    destination: Path,
    dpi: int,
    page_index: int,
) -> Path:
    matrix = pymupdf.Matrix(dpi / 72.0, dpi / 72.0)
    page_number = page_index + 1
    page_path = destination / f"page_{page_number:06d}.png"

    with pymupdf.open(input_pdf) as document:
        page = document.load_page(page_index)
        pixmap = page.get_pixmap(
            matrix=matrix,
            alpha=False,
        )
        pixmap.save(page_path.as_posix())

    logger.info(f"Rendered page {page_number}: "
                f"{pixmap.width}x{pixmap.height}, "
                f"{page_path.stat().st_size} bytes")

    return page_path


def render_pdf_pages(
    input_pdf: Path,
    destination: Path,
    dpi: int,
    raster_threads: int,
    max_pages: int,
) -> list[Path]:
    destination.mkdir(parents=True, exist_ok=True)

    with pymupdf.open(input_pdf) as document:
        page_count = min(document.page_count, max_pages)

    if page_count == 0:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")

    logger.info(f"Rendering {page_count} PDF pages at {dpi} DPI "
                f"using {raster_threads} threads")

    with ThreadPoolExecutor(max_workers=raster_threads) as executor:
        page_paths = list(
            executor.map(
                lambda page_index: render_pdf_page(
                    input_pdf=input_pdf,
                    destination=destination,
                    dpi=dpi,
                    page_index=page_index,
                ),
                range(page_count),
            ))

    total_size = sum(path.stat().st_size for path in page_paths)

    logger.info(
        f"Rendered {len(page_paths)} pages, total size {total_size} bytes")

    return page_paths

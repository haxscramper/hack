import hashlib
import os
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import pymupdf
from loguru import logger


def pdf_digest(input_pdf: Path) -> str:
    digest = hashlib.sha256()

    with input_pdf.open("rb") as file:
        for chunk in iter(lambda: file.read(1024 * 1024), b""):
            digest.update(chunk)

    return digest.hexdigest()


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

    logger.info(
        f"Rendered page {page_number}: "
        f"{pixmap.width}x{pixmap.height}, "
        f"{page_path.stat().st_size} bytes", )

    return page_path


def render_pdf_pages(
    input_pdf: Path,
    dpi: int,
    raster_threads: int,
    target_pages: set[int],
) -> dict[int, Path]:
    cache_directory = Path("/tmp") / pdf_digest(input_pdf) / f"dpi_{dpi}"
    cache_directory.mkdir(parents=True, exist_ok=True)

    page_paths = {
        page_index: cache_directory / f"page_{page_index + 1:06d}.png"
        for page_index in target_pages
    }

    existing_pages = {
        page_index
        for page_index, page_path in page_paths.items() if page_path.is_file()
    }
    missing_pages = target_pages - existing_pages

    logger.info(
        f"Using cache directory {cache_directory}; "
        f"{len(missing_pages)} of {len(target_pages)} pages need rendering", )

    with ThreadPoolExecutor(max_workers=raster_threads) as executor:
        list(
            executor.map(
                lambda page_index: render_pdf_page(
                    input_pdf=input_pdf,
                    destination=cache_directory,
                    dpi=dpi,
                    page_index=page_index,
                ),
                missing_pages,
            ))

    total_size = sum(path.stat().st_size for path in page_paths.values())

    logger.info(
        f"Prepared {len(page_paths)} pages, total size {total_size} bytes", )

    return page_paths

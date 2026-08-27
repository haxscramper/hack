#!/usr/bin/env python
from __future__ import annotations

import os
from pathlib import Path

import click
from beartype import beartype
from beartype.typing import Optional
from loguru import logger
from sqlalchemy.orm import Session
from sqlalchemy import select
import pymupdf

from ocr_db.collect.ocr_processor import OcrProcessor
from ocr_db.ocr_db import (
    PageRecord,
    create_engine_and_tables,
    ensure_document_and_input_file,
    save_result_to_database,
)

IMAGE_EXTENSIONS = {
    ".png",
    ".jpg",
    ".jpeg",
    ".webp",
    ".bmp",
    ".tif",
    ".tiff",
}


@beartype
def collect_inputs(input_path: Path) -> list[Path]:
    if input_path.is_file():
        ext = input_path.suffix.lower()
        if ext != ".pdf" and ext not in IMAGE_EXTENSIONS:
            raise ValueError(f"Unsupported input file extension: {ext}")
        return [input_path]

    if not input_path.is_dir():
        raise ValueError(f"Input path does not exist: {input_path}")

    files = sorted(p for p in input_path.rglob("*") if p.is_file() and (
        p.suffix.lower() == ".pdf" or p.suffix.lower() in IMAGE_EXTENSIONS))

    if not files:
        raise RuntimeError(
            f"No supported files found in directory: {input_path}")

    return files


@beartype
def process_file(
    session: Session,
    processor: OcrProcessor,
    file: Path,
    indices: set[int],
) -> None:
    document_id, file_sha256 = ensure_document_and_input_file(
        session,
        file,
    )

    for record in session.scalars(
            select(PageRecord).where(PageRecord.document_id == document_id)):
        indices.remove(record.page_number)

    result = processor.process_file(
        file=file,
        indices=indices,
    )

    save_result_to_database(
        session=session,
        document_id=document_id,
        result=result,
    )

    logger.info(f"Finished: {file}")


@beartype
def parse_page_range(spec: str, page_count: int) -> set[int]:
    pages = set()

    for part in spec.split(","):
        if "-" in part:
            start_text, end_text = part.split("-", 1)
            start = int(start_text)
            end = int(end_text) if end_text else page_count

            if not 1 <= start <= end <= page_count:
                raise ValueError(f"Invalid page range: {part}")

            pages.update(range(start, end + 1))
        else:
            page = int(part)

            if not 1 <= page <= page_count:
                raise ValueError(f"Invalid page: {page}")

            pages.add(page)

    return pages


@click.command()
@click.argument(
    "input_path",
    type=click.Path(path_type=Path),
)
@click.argument(
    "db_path",
    type=click.Path(path_type=Path),
    required=True,
)
@click.option("--model-id", default="ibm/granite-docling")
@click.option("--dpi", type=int, default=300)
@click.option("--llama-url", default="http://localhost:8080")
@click.option("--request-threads", type=int, default=1)
@click.option("--raster-threads", type=int, default=(os.cpu_count() or 4) * 2)
@click.option("--request-timeout", type=int, default=300)
@click.option("--page-range", default=None)
@beartype
def main(
    input_path: Path,
    db_path: Path,
    model_id: str,
    dpi: int,
    llama_url: str,
    request_threads: int,
    raster_threads: int,
    request_timeout: int,
    page_range: Optional[str] = None,
) -> None:
    input_path = input_path.resolve()
    db_path = db_path.resolve()
    files = collect_inputs(input_path)
    session_factory = create_engine_and_tables(db_path)
    processor = OcrProcessor(
        llama_server_url=llama_url,
        dpi=dpi,
        model_id=model_id,
        request_threads=request_threads,
        raster_threads=raster_threads,
        request_timeout=request_timeout,
    )

    failures: list[tuple[Path, Exception]] = []

    with session_factory() as session:
        for file in files:
            with pymupdf.open(file) as document:
                page_count = document.page_count

            indices: set[int] = set()
            if page_range:
                indices = parse_page_range(page_range, page_count=page_count)
            else:
                for i in range(page_count):
                    indices.add(i)

            try:
                process_file(
                    session=session,
                    processor=processor,
                    file=file,
                    indices=indices,
                )
            except Exception as error:
                failures.append((file, error))
                logger.exception(f"Failed processing {file}: {error}")

    logger.info(f"Finished writing {db_path}")

    if failures:
        lines = "\n".join(f"- {path}: {error}" for path, error in failures)
        raise RuntimeError(f"OCR processing finished with failures:\n{lines}")


if __name__ == "__main__":
    main()

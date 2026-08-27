#!/usr/bin/env python
from __future__ import annotations

import os
from pathlib import Path
import sys

import click
from beartype import beartype
from beartype.typing import Iterable, Optional
from loguru import logger
from sqlalchemy.orm import Session

from ocr_db.collect.ocr_models import OcrChunkOcrResult
from ocr_db.collect.ocr_processor import DEFAULT_OCR_MODEL_ID, OcrProcessor
from ocr_db.ocr_db import (
    clear_document_data,
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
def save_docling_ocr_to_database(
    session: Session,
    document_id: int,
    result: OcrChunkOcrResult,
) -> None:
    save_result_to_database(
        session=session,
        document_id=document_id,
        result=result,
    )


@beartype
def process_file(
    session: Session,
    processor: OcrProcessor,
    source_file: Path,
    overwrite: bool,
    cleared_documents: set[int],
) -> None:
    document_id, file_sha256 = ensure_document_and_input_file(
        session,
        source_file,
    )

    if overwrite and document_id not in cleared_documents:
        clear_document_data(session, document_id=document_id)
        cleared_documents.add(document_id)

        logger.info(f"Cleared indexed data for document_id={document_id} "
                    f"hash={file_sha256}")

        page_indices = set()
        result = processor.process_file(
            source_file=source_file,
            page_indices=page_indices,
        )

        save_docling_ocr_to_database(session, document_id, result)

    logger.info(f"Finished: {source_file}")


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
@click.option(
    "--mirror-from",
    type=click.Path(path_type=Path),
    default=None,
    help="Base directory used to compute mirrored relative paths.",
)
@click.option("--model-id", default="ibm/granite-docling")
@click.option("--dpi", type=int, default=300)
@click.option("--overwrite", is_flag=True)
@click.option("--llama-url", default="http://localhost:8080")
@click.option("--request-threads", type=int, default=1)
@click.option("--raster-threads", type=int, default=(os.cpu_count() or 4) * 2)
@click.option("--request-timeout", type=int, default=300)
@beartype
def main(
    input_path: Path,
    db_path: Path,
    model_id: str,
    dpi: int,
    overwrite: bool,
    llama_url: str,
    request_threads: int,
    raster_threads: int,
    request_timeout: int,
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
    cleared_documents: set[int] = set()

    with session_factory() as session:
        for file_path in files:
            try:
                process_file(
                    session=session,
                    processor=processor,
                    source_file=file_path,
                    overwrite=overwrite,
                    cleared_documents=cleared_documents,
                )
            except Exception as error:
                failures.append((file_path, error))
                logger.exception(f"Failed processing {file_path}: {error}")

    if failures:
        lines = "\n".join(f"- {path}: {error}" for path, error in failures)
        raise RuntimeError(f"OCR processing finished with failures:\n{lines}")


if __name__ == "__main__":
    main()

#!/usr/bin/env python
from __future__ import annotations

from pathlib import Path

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
def resolve_mirror_from(input_path: Path, mirror_from: Optional[Path]) -> Path:
    if mirror_from is not None:
        if not mirror_from.exists():
            raise ValueError(f"--mirror-from does not exist: {mirror_from}")
        return mirror_from.resolve()

    if input_path.is_dir():
        return input_path.resolve()

    return input_path.resolve().parent


@beartype
def mirrored_output_base(
    source_file: Path,
    mirror_from: Path,
    output_root: Path,
) -> Path:
    source_file = source_file.resolve()

    try:
        relative = source_file.relative_to(mirror_from)
    except ValueError as error:
        raise ValueError(
            f"Cannot mirror {source_file} from base {mirror_from}; "
            "pass a correct --mirror-from") from error

    return (output_root / relative).with_suffix("")


@beartype
def split_chunks(
    items: list[Path],
    chunk_size: int,
) -> Iterable[tuple[int, list[Path]]]:
    if chunk_size <= 0:
        raise ValueError(f"chunk_size must be positive, got {chunk_size}")

    for i in range(0, len(items), chunk_size):
        yield i // chunk_size, items[i:i + chunk_size]


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
    output_root: Path,
    mirror_from: Path,
    overwrite: bool,
    cleared_documents: set[int],
) -> None:
    output_base = mirrored_output_base(source_file, mirror_from, output_root)
    output_base.mkdir(parents=True, exist_ok=True)

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

    logger.info(f"Finished: {source_file} -> {output_base}")


@click.command()
@click.argument(
    "input_path",
    type=click.Path(path_type=Path),
)
@click.argument(
    "output_dir",
    type=click.Path(path_type=Path),
)
@click.option(
    "--db-path",
    type=click.Path(path_type=Path),
    required=True,
)
@click.option(
    "--mirror-from",
    type=click.Path(path_type=Path),
    default=None,
    help="Base directory used to compute mirrored relative paths.",
)
@click.option("--model-id", default=DEFAULT_OCR_MODEL_ID)
@click.option("--dpi", type=int, default=300)
@click.option("--overwrite", is_flag=True)
@click.option("--llama-url", default="http://localhost:8080")
@beartype
def main(
    input_path: Path,
    output_dir: Path,
    db_path: Path,
    mirror_from: Optional[Path],
    model_id: str,
    dpi: int,
    overwrite: bool,
    llama_url: str,
) -> None:
    input_path = input_path.resolve()
    output_dir = output_dir.resolve()
    db_path = db_path.resolve()
    mirror_from = resolve_mirror_from(input_path, mirror_from)
    files = collect_inputs(input_path)

    output_dir.mkdir(parents=True, exist_ok=True)
    session_factory = create_engine_and_tables(db_path)

    processor = OcrProcessor(
        llama_server_url=llama_url,
        dpi=dpi,
        model_id=model_id,
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
                    output_root=output_dir,
                    mirror_from=mirror_from,
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

#!/usr/bin/env python
from __future__ import annotations

import argparse
import json
from pathlib import Path

from beartype import beartype
from beartype.typing import Iterable, Optional
from loguru import logger
from sqlalchemy.orm import Session

from ocr_db import (chunk_exists, clear_document_data,
                    create_engine_and_tables, ensure_document_and_input_file,
                    get_chunk_record, parse_pages_from_chunk_json,
                    save_chunk_to_database)
from ocr_unlimited_models import OcrChunkResult, OcrDocumentResult
from ocr_unlimited import UnlimitedOcrProcessor

IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff"}


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=Path)
    parser.add_argument("output_dir", type=Path)
    parser.add_argument("--db-path", type=Path, required=True)
    parser.add_argument(
        "--mirror-from",
        type=Path,
        default=None,
        help="Base directory used to compute mirrored relative paths.",
    )
    parser.add_argument("--model-id", default="baidu/Unlimited-OCR")
    parser.add_argument("--chunk-size", type=int, default=20)
    parser.add_argument("--dpi", type=int, default=300)
    parser.add_argument("--max-length", type=int, default=32768)
    parser.add_argument("--image-size", type=int, default=1024)
    parser.add_argument("--prompt-image", default="<image>document parsing.")
    parser.add_argument("--prompt-pdf", default="<image>Multi page parsing.")
    parser.add_argument("--overwrite", action="store_true")
    return parser.parse_args()


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
def mirrored_output_base(source_file: Path, mirror_from: Path,
                         output_root: Path) -> Path:
    source_file = source_file.resolve()
    try:
        relative = source_file.relative_to(mirror_from)
    except ValueError as error:
        raise ValueError(
            f"Cannot mirror {source_file} from base {mirror_from}; pass a correct --mirror-from"
        ) from error
    return (output_root / relative).with_suffix("")


@beartype
def split_chunks(items: list[Path],
                 chunk_size: int) -> Iterable[tuple[int, list[Path]]]:
    if chunk_size <= 0:
        raise ValueError(f"chunk_size must be positive, got {chunk_size}")
    for i in range(0, len(items), chunk_size):
        yield i // chunk_size, items[i:i + chunk_size]


@beartype
def load_chunk_from_db(session: Session, document_id: int, chunk_index: int,
                       page_start: int, page_end: int,
                       chunk_dir: Path) -> OcrChunkResult:
    row = get_chunk_record(session,
                           document_id=document_id,
                           chunk_index=chunk_index)
    pages = parse_pages_from_chunk_json(row.structured_json,
                                        document_id=document_id,
                                        chunk_index=chunk_index)
    return OcrChunkResult(
        chunk_index=chunk_index,
        page_start=page_start,
        page_end=page_end,
        raw_text_file=str(chunk_dir / "raw_text.txt"),
        structured_json_file=str(chunk_dir / "structured_data.json"),
        pages_dir=str(chunk_dir / "pages"),
        annotated_pages_dir=str(chunk_dir / "annotated_pages"),
        extracted_images_dir=str(chunk_dir / "images"),
        pages=pages,
    )


@beartype
def process_file(
    session: Session,
    processor: UnlimitedOcrProcessor,
    source_file: Path,
    output_root: Path,
    mirror_from: Path,
    chunk_size: int,
    overwrite: bool,
    cleared_documents: set[int],
) -> None:
    output_base = mirrored_output_base(source_file, mirror_from, output_root)
    output_base.mkdir(parents=True, exist_ok=True)

    document_id, file_sha256 = ensure_document_and_input_file(
        session, source_file)

    if overwrite and document_id not in cleared_documents:
        clear_document_data(session, document_id=document_id)
        cleared_documents.add(document_id)
        logger.info(
            f"Cleared indexed data for document_id={document_id} hash={file_sha256}"
        )

    rendered_pages = processor.render_pages(source_file,
                                            output_base / "_rendered_pages")

    chunks: list[OcrChunkResult] = []
    for chunk_index, chunk_pages in split_chunks(rendered_pages, chunk_size):
        chunk_dir = output_base / "chunks" / f"chunk_{chunk_index:04d}"
        page_start = chunk_index * chunk_size + 1
        page_end = page_start + len(chunk_pages) - 1

        if chunk_exists(session,
                        document_id=document_id,
                        chunk_index=chunk_index):
            logger.info(
                f"Skipping already indexed chunk document_id={document_id} chunk_index={chunk_index}"
            )
            chunks.append(
                load_chunk_from_db(session, document_id, chunk_index,
                                   page_start, page_end, chunk_dir))
            continue

        chunk_dir.mkdir(parents=True, exist_ok=True)
        result = processor.process_chunk(
            source_file=source_file,
            chunk_index=chunk_index,
            chunk_page_files=chunk_pages,
            page_offset=chunk_index * chunk_size,
            chunk_dir=chunk_dir,
        )

        structured_json_path = chunk_dir / "structured_data.json"
        structured_json_path.write_text(
            json.dumps([page.model_dump() for page in result.pages],
                       indent=2,
                       ensure_ascii=False),
            encoding="utf-8",
        )

        save_chunk_to_database(
            session=session,
            document_id=document_id,
            chunk_index=chunk_index,
            raw_output=result.raw_text,
            pages=result.pages,
            image_elements_by_page=result.image_elements_by_page,
        )

        chunks.append(
            OcrChunkResult(
                chunk_index=chunk_index,
                page_start=page_start,
                page_end=page_end,
                raw_text_file=str(chunk_dir / "raw_text.txt"),
                structured_json_file=str(structured_json_path),
                pages_dir=str(chunk_dir / "pages"),
                annotated_pages_dir=str(chunk_dir / "annotated_pages"),
                extracted_images_dir=str(chunk_dir / "images"),
                pages=result.pages,
            ))

    doc_result = OcrDocumentResult(
        source_file=str(source_file),
        relative_source=source_file.resolve().relative_to(
            mirror_from).as_posix(),
        output_dir=str(output_base),
        chunks=chunks,
    )
    (output_base / "document_result.json").write_text(
        doc_result.model_dump_json(indent=2), encoding="utf-8")
    logger.info(f"Finished: {source_file} -> {output_base}")


@beartype
def main() -> None:
    args = parse_args()
    input_path = args.input_path.resolve()
    output_dir = args.output_dir.resolve()
    db_path = args.db_path.resolve()
    mirror_from = resolve_mirror_from(input_path, args.mirror_from)
    files = collect_inputs(input_path)

    output_dir.mkdir(parents=True, exist_ok=True)
    session_factory = create_engine_and_tables(db_path)
    processor = UnlimitedOcrProcessor(
        model_id=args.model_id,
        image_size=args.image_size,
        max_length=args.max_length,
        dpi=args.dpi,
        prompt_image=args.prompt_image,
        prompt_pdf=args.prompt_pdf,
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
                    chunk_size=args.chunk_size,
                    overwrite=args.overwrite,
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

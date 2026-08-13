#!/usr/bin/env python
from __future__ import annotations

import argparse
import hashlib
import io
import json
from loguru import logger
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

import fitz
import torch
from beartype import beartype
from beartype.typing import Any, Iterable, Optional
from PIL import Image, ImageDraw, ImageFont
from pydantic import BaseModel, Field
from sqlalchemy import ForeignKey, LargeBinary, String, Text, UniqueConstraint, create_engine, delete, event, select
from sqlalchemy.engine import Engine
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column, sessionmaker
from transformers import AutoModel, AutoTokenizer

IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff"}
DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"


class OcrBBox(BaseModel):
    x1: int = Field(ge=0)
    y1: int = Field(ge=0)
    x2: int = Field(ge=0)
    y2: int = Field(ge=0)


class OcrElement(BaseModel):
    label: str = Field(min_length=1)
    bbox: OcrBBox
    text: str = ""


class OcrPage(BaseModel):
    page_number: int = Field(ge=1)
    elements: list[OcrElement] = Field(default_factory=list)


class OcrChunkResult(BaseModel):
    chunk_index: int = Field(ge=0)
    page_start: int = Field(ge=1)
    page_end: int = Field(ge=1)
    raw_text_file: str
    structured_json_file: str
    pages_dir: str
    annotated_pages_dir: str
    extracted_images_dir: str
    pages: list[OcrPage]


class OcrDocumentResult(BaseModel):
    source_file: str
    relative_source: str
    output_dir: str
    chunks: list[OcrChunkResult]


@dataclass(frozen=True)
class ExtractedImageElement:
    element_index: int
    element: OcrElement
    image_blob: bytes


class Base(DeclarativeBase):
    pass


class InputFileRecord(Base):
    __tablename__ = "input_files"

    id: Mapped[int] = mapped_column(primary_key=True)
    absolute_path: Mapped[str] = mapped_column(String,
                                               unique=True,
                                               nullable=False)
    file_sha256: Mapped[str] = mapped_column(
        String(64),
        ForeignKey("documents.file_sha256"),
        nullable=False,
    )


class DocumentRecord(Base):
    __tablename__ = "documents"

    id: Mapped[int] = mapped_column(primary_key=True)
    file_sha256: Mapped[str] = mapped_column(String(64),
                                             unique=True,
                                             nullable=False)


class PageRecord(Base):
    __tablename__ = "pages"
    __table_args__ = (UniqueConstraint("document_id",
                                       "page_number",
                                       name="uq_pages_document_page"), )

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(ForeignKey("documents.id"),
                                             nullable=False)
    page_number: Mapped[int] = mapped_column(nullable=False)


class ElementRecord(Base):
    __tablename__ = "elements"
    __table_args__ = (UniqueConstraint("page_id",
                                       "element_index",
                                       name="uq_elements_page_index"), )

    id: Mapped[int] = mapped_column(primary_key=True)
    page_id: Mapped[int] = mapped_column(ForeignKey("pages.id"),
                                         nullable=False)
    element_index: Mapped[int] = mapped_column(nullable=False)
    bbox_x1: Mapped[int] = mapped_column(nullable=False)
    bbox_y1: Mapped[int] = mapped_column(nullable=False)
    bbox_x2: Mapped[int] = mapped_column(nullable=False)
    bbox_y2: Mapped[int] = mapped_column(nullable=False)
    element_type: Mapped[str] = mapped_column(String, nullable=False)
    label: Mapped[str] = mapped_column(String, nullable=False)
    text: Mapped[str] = mapped_column(Text, nullable=False, default="")
    image_blob: Mapped[bytes] = mapped_column(LargeBinary, nullable=False)


class ChunkRecord(Base):
    __tablename__ = "chunks"
    __table_args__ = (UniqueConstraint("document_id",
                                       "chunk_index",
                                       name="uq_chunks_document_index"), )

    id: Mapped[int] = mapped_column(primary_key=True)
    document_id: Mapped[int] = mapped_column(ForeignKey("documents.id"),
                                             nullable=False)
    chunk_index: Mapped[int] = mapped_column(nullable=False)
    raw_output: Mapped[str] = mapped_column(Text, nullable=False)
    structured_json: Mapped[str] = mapped_column(Text, nullable=False)


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
    parser.add_argument("--model-id", default=DEFAULT_MODEL_ID)
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
def load_model_and_tokenizer(model_id: str) -> tuple[Any, Any]:
    tokenizer = AutoTokenizer.from_pretrained(model_id, trust_remote_code=True)
    model = AutoModel.from_pretrained(
        model_id,
        trust_remote_code=True,
        torch_dtype=torch.bfloat16,
    ).cuda().eval()
    return model, tokenizer


@beartype
def create_engine_and_tables(db_path: Path) -> sessionmaker[Session]:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    engine = create_engine(f"sqlite:///{db_path}", future=True)

    @event.listens_for(engine, "connect")
    def on_connect(dbapi_connection: Any, connection_record: Any) -> None:
        del connection_record
        cursor = dbapi_connection.cursor()
        cursor.execute("PRAGMA foreign_keys = ON")
        cursor.close()

    Base.metadata.create_all(engine)
    return sessionmaker(bind=engine,
                        autoflush=False,
                        autocommit=False,
                        future=True)


@beartype
def sha256_of_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(1024 * 1024)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


@beartype
def ensure_document_and_input_file(session: Session,
                                   source_file: Path) -> tuple[int, str]:
    absolute_path = str(source_file.resolve())
    file_sha256 = sha256_of_file(source_file)

    document = session.scalar(
        select(DocumentRecord).where(
            DocumentRecord.file_sha256 == file_sha256))
    if document is None:
        document = DocumentRecord(file_sha256=file_sha256)
        session.add(document)
        session.flush()

    input_record = session.scalar(
        select(InputFileRecord).where(
            InputFileRecord.absolute_path == absolute_path))
    if input_record is None:
        session.add(
            InputFileRecord(absolute_path=absolute_path,
                            file_sha256=file_sha256))
    else:
        input_record.file_sha256 = file_sha256

    session.commit()
    if document.id is None:
        raise RuntimeError(
            f"Document row id is missing for hash {file_sha256}")
    return document.id, file_sha256


@beartype
def clear_document_data(session: Session, document_id: int) -> None:
    page_ids = list(
        session.scalars(
            select(
                PageRecord.id).where(PageRecord.document_id == document_id)))
    if page_ids:
        session.execute(
            delete(ElementRecord).where(ElementRecord.page_id.in_(page_ids)))
    session.execute(
        delete(PageRecord).where(PageRecord.document_id == document_id))
    session.execute(
        delete(ChunkRecord).where(ChunkRecord.document_id == document_id))
    session.commit()


@beartype
def chunk_exists(session: Session, document_id: int, chunk_index: int) -> bool:
    row = session.scalar(
        select(ChunkRecord.id).where(
            ChunkRecord.document_id == document_id,
            ChunkRecord.chunk_index == chunk_index,
        ))
    return row is not None


@beartype
def get_chunk_record(session: Session, document_id: int,
                     chunk_index: int) -> ChunkRecord:
    row = session.scalar(
        select(ChunkRecord).where(
            ChunkRecord.document_id == document_id,
            ChunkRecord.chunk_index == chunk_index,
        ))
    if row is None:
        raise RuntimeError(
            f"Chunk row is missing for document_id={document_id}, chunk_index={chunk_index}"
        )
    return row


@beartype
def get_or_create_page(session: Session, document_id: int,
                       page_number: int) -> PageRecord:
    page = session.scalar(
        select(PageRecord).where(PageRecord.document_id == document_id,
                                 PageRecord.page_number == page_number))
    if page is not None:
        return page
    page = PageRecord(document_id=document_id, page_number=page_number)
    session.add(page)
    session.flush()
    return page


@beartype
def save_chunk_to_database(
    session: Session,
    document_id: int,
    chunk_index: int,
    raw_output: str,
    pages: list[OcrPage],
    image_elements_by_page: dict[int, list[ExtractedImageElement]],
) -> None:
    structured_json = json.dumps([page.model_dump() for page in pages],
                                 ensure_ascii=False)
    chunk = ChunkRecord(
        document_id=document_id,
        chunk_index=chunk_index,
        raw_output=raw_output,
        structured_json=structured_json,
    )
    session.add(chunk)
    session.flush()

    for page in pages:
        page_row = get_or_create_page(session,
                                      document_id=document_id,
                                      page_number=page.page_number)
        extracted_for_page = image_elements_by_page.get(page.page_number, [])
        image_blob_by_element_index = {
            item.element_index: item.image_blob
            for item in extracted_for_page
        }

        for element_index, element in enumerate(page.elements):
            row = ElementRecord(
                page_id=page_row.id,
                element_index=element_index,
                bbox_x1=element.bbox.x1,
                bbox_y1=element.bbox.y1,
                bbox_x2=element.bbox.x2,
                bbox_y2=element.bbox.y2,
                element_type=element.label,
                label=element.label,
                text=element.text,
                image_blob=image_blob_by_element_index.get(element_index, b""),
            )
            session.add(row)

    session.commit()


@beartype
def render_pdf_pages(input_pdf: Path, dst_dir: Path, dpi: int) -> list[Path]:
    dst_dir.mkdir(parents=True, exist_ok=True)
    page_paths: list[Path] = []

    doc = fitz.open(input_pdf)
    zoom = dpi / 72.0
    matrix = fitz.Matrix(zoom, zoom)

    for page_index, page in enumerate(doc, start=1):
        pix = page.get_pixmap(matrix=matrix, alpha=False)
        page_path = dst_dir / f"page_{page_index:04d}.png"
        pix.save(page_path.as_posix())
        page_paths.append(page_path)

    doc.close()
    if not page_paths:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")
    return page_paths


@beartype
def split_chunks(items: list[Path],
                 chunk_size: int) -> Iterable[tuple[int, list[Path]]]:
    if chunk_size <= 0:
        raise ValueError(f"chunk_size must be positive, got {chunk_size}")
    for i in range(0, len(items), chunk_size):
        yield i // chunk_size, items[i:i + chunk_size]


@beartype
def unpack_infer_multi_result(result: Any) -> str:
    match result:
        case tuple() as value:
            if not value:
                raise RuntimeError("infer_multi returned an empty tuple")
            first = value[0]
            if not isinstance(first, str):
                raise RuntimeError(
                    f"infer_multi[0] must be str, got {type(first)}")
            return first
        case str() as value:
            return value
        case _:
            raise RuntimeError(
                f"infer_multi returned unsupported type: {type(result)}")


@beartype
def parse_bbox(text: str) -> OcrBBox:
    match = re.fullmatch(
        r"\[\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]",
        text.strip(),
    )
    if match is None:
        raise ValueError(f"Invalid bbox syntax: {text}")

    x1, y1, x2, y2 = map(int, match.groups())
    if x2 < x1 or y2 < y1:
        raise ValueError(
            f"Invalid bbox coordinates with negative width or height: {text}")
    return OcrBBox(x1=x1, y1=y1, x2=x2, y2=y2)


@beartype
def parse_page_content(page_text: str, page_number: int) -> OcrPage:
    det_pattern = re.compile(
        r"<\|det\|>\s*(?P<label>[^\[]+?)\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )
    ref_det_pattern = re.compile(
        r"<\|ref\|>\s*(?P<label>.*?)\s*<\|/ref\|>\s*<\|det\|>\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|ref\|>|<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )

    elements: list[OcrElement] = []

    for match in det_pattern.finditer(page_text):
        bbox = parse_bbox(match.group("bbox"))
        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=bbox,
                text=match.group("text").strip(),
            ))

    for match in ref_det_pattern.finditer(page_text):
        bbox = parse_bbox(match.group("bbox"))
        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=bbox,
                text=match.group("text").strip(),
            ))

    return OcrPage(page_number=page_number, elements=elements)


@beartype
def parse_ocr_output(raw_text: str, page_offset: int) -> list[OcrPage]:
    if not raw_text.strip():
        return []

    pages_raw = raw_text.split("<PAGE>")
    pages: list[OcrPage] = []
    absolute_page = page_offset + 1

    for page_text in pages_raw:
        page_text = page_text.strip()
        if not page_text:
            continue
        page = parse_page_content(page_text, absolute_page)
        pages.append(page)
        absolute_page += 1

    if raw_text.strip() and not pages:
        raise RuntimeError(
            "Model output is non-empty, but no structured OCR tokens were parsed"
        )
    return pages


@beartype
def scale_bbox(bbox: OcrBBox, width: int,
               height: int) -> tuple[int, int, int, int]:
    x1 = int(bbox.x1 / 999 * width)
    y1 = int(bbox.y1 / 999 * height)
    x2 = int(bbox.x2 / 999 * width)
    y2 = int(bbox.y2 / 999 * height)

    x1 = max(0, min(width - 1, x1))
    y1 = max(0, min(height - 1, y1))
    x2 = max(0, min(width - 1, x2))
    y2 = max(0, min(height - 1, y2))

    if x2 < x1:
        x1, x2 = x2, x1
    if y2 < y1:
        y1, y2 = y2, y1
    return x1, y1, x2, y2


@beartype
def color_for_label(label: str) -> tuple[int, int, int]:
    seed = sum(ord(char) for char in label)
    r = 50 + (seed * 37) % 180
    g = 50 + (seed * 67) % 180
    b = 50 + (seed * 97) % 180
    return r, g, b


@beartype
def annotate_and_extract(
    source_page: Path,
    page: OcrPage,
    annotated_pages_dir: Path,
    extracted_images_dir: Path,
) -> list[ExtractedImageElement]:
    annotated_pages_dir.mkdir(parents=True, exist_ok=True)
    extracted_images_dir.mkdir(parents=True, exist_ok=True)

    image = Image.open(source_page).convert("RGB")
    draw = ImageDraw.Draw(image)
    font = ImageFont.load_default()
    width, height = image.size

    extracted: list[ExtractedImageElement] = []
    image_crop_index = 0

    for element_index, element in enumerate(page.elements):
        x1, y1, x2, y2 = scale_bbox(element.bbox, width, height)
        color = color_for_label(element.label)

        draw.rectangle((x1, y1, x2, y2), outline=color, width=2)
        label_text = element.label
        text_bbox = draw.textbbox((x1, y1), label_text, font=font)
        draw.rectangle(text_bbox, fill=(255, 255, 255))
        draw.text((x1, y1), label_text, fill=color, font=font)

        is_image = element.label.strip().casefold() == "image"
        if is_image:
            crop = image.crop((x1, y1, x2, y2))
            crop_name = f"page_{page.page_number:04d}_image_{image_crop_index:04d}.png"
            crop_path = extracted_images_dir / crop_name
            crop.save(crop_path)
            image_crop_index += 1

            buffer = io.BytesIO()
            crop.save(buffer, format="PNG")
            extracted.append(
                ExtractedImageElement(
                    element_index=element_index,
                    element=element,
                    image_blob=buffer.getvalue(),
                ))

    annotated_path = annotated_pages_dir / f"page_{page.page_number:04d}.png"
    image.save(annotated_path)
    return extracted


@beartype
def run_chunk_infer_multi(
    model: Any,
    tokenizer: Any,
    image_files: list[Path],
    prompt: str,
    output_path: Path,
    image_size: int,
    max_length: int,
) -> str:
    result = model.infer_multi(
        tokenizer,
        prompt=prompt,
        image_files=[str(path) for path in image_files],
        output_path=str(output_path),
        image_size=image_size,
        save_results=False,
        max_length=max_length,
    )
    return unpack_infer_multi_result(result)


@beartype
def run_chunk_infer_single(
    model: Any,
    tokenizer: Any,
    image_file: Path,
    prompt: str,
    output_path: Path,
    image_size: int,
    max_length: int,
) -> str:
    result = model.infer(
        tokenizer,
        prompt=prompt,
        image_file=str(image_file),
        output_path=str(output_path),
        base_size=1024,
        image_size=image_size,
        crop_mode=True,
        save_results=False,
        eval_mode=True,
        max_length=max_length,
    )
    if not isinstance(result, str):
        raise RuntimeError(
            f"infer did not return str with eval_mode=True, got {type(result)}"
        )
    return result


@beartype
def parse_pages_from_chunk_json(chunk_json: str, document_id: int,
                                chunk_index: int) -> list[OcrPage]:
    try:
        payload = json.loads(chunk_json)
    except json.JSONDecodeError as error:
        raise RuntimeError(
            f"Invalid JSON in chunks table for document_id={document_id}, chunk_index={chunk_index}: {error}"
        ) from error

    if not isinstance(payload, list):
        raise RuntimeError(
            f"Chunk JSON must be a list for document_id={document_id}, chunk_index={chunk_index}, got {type(payload)}"
        )

    pages: list[OcrPage] = []
    for item in payload:
        pages.append(OcrPage.model_validate(item))
    return pages


@beartype
def process_pdf(
    session: Session,
    model: Any,
    tokenizer: Any,
    document_id: int,
    source_file: Path,
    output_base: Path,
    chunk_size: int,
    dpi: int,
    prompt: str,
    image_size: int,
    max_length: int,
) -> OcrDocumentResult:
    output_base.mkdir(parents=True, exist_ok=True)
    rendered_dir = output_base / "_rendered_pages"
    rendered_pages = render_pdf_pages(source_file, rendered_dir, dpi=dpi)

    chunks: list[OcrChunkResult] = []

    for chunk_index, chunk_pages in split_chunks(rendered_pages, chunk_size):
        chunk_dir = output_base / "chunks" / f"chunk_{chunk_index:04d}"
        pages_dir = chunk_dir / "pages"
        annotated_pages_dir = chunk_dir / "annotated_pages"
        extracted_images_dir = chunk_dir / "images"
        model_tmp_dir = chunk_dir / "_model"

        page_start = chunk_index * chunk_size + 1
        page_end = page_start + len(chunk_pages) - 1

        if chunk_exists(session,
                        document_id=document_id,
                        chunk_index=chunk_index):
            row = get_chunk_record(session,
                                   document_id=document_id,
                                   chunk_index=chunk_index)
            pages = parse_pages_from_chunk_json(row.structured_json,
                                                document_id=document_id,
                                                chunk_index=chunk_index)
            chunks.append(
                OcrChunkResult(
                    chunk_index=chunk_index,
                    page_start=page_start,
                    page_end=page_end,
                    raw_text_file=str(chunk_dir / "raw_text.txt"),
                    structured_json_file=str(chunk_dir /
                                             "structured_data.json"),
                    pages_dir=str(pages_dir),
                    annotated_pages_dir=str(annotated_pages_dir),
                    extracted_images_dir=str(extracted_images_dir),
                    pages=pages,
                ))
            logger.info(
                f"Skipping already indexed chunk document_id={document_id} chunk_index={chunk_index}"
            )
            continue

        chunk_dir.mkdir(parents=True, exist_ok=True)
        pages_dir.mkdir(parents=True, exist_ok=True)
        annotated_pages_dir.mkdir(parents=True, exist_ok=True)
        extracted_images_dir.mkdir(parents=True, exist_ok=True)
        model_tmp_dir.mkdir(parents=True, exist_ok=True)

        local_page_paths: list[Path] = []
        for source_page in chunk_pages:
            destination = pages_dir / source_page.name
            shutil.copy2(source_page, destination)
            local_page_paths.append(destination)

        raw_text = run_chunk_infer_multi(
            model=model,
            tokenizer=tokenizer,
            image_files=local_page_paths,
            prompt=prompt,
            output_path=model_tmp_dir,
            image_size=image_size,
            max_length=max_length,
        )

        raw_text_path = chunk_dir / "raw_text.txt"
        raw_text_path.write_text(raw_text, encoding="utf-8")

        page_offset = chunk_index * chunk_size
        pages = parse_ocr_output(raw_text, page_offset=page_offset)

        local_pages_by_number = {
            page_offset + idx + 1: path
            for idx, path in enumerate(local_page_paths)
        }
        images_by_page: dict[int, list[ExtractedImageElement]] = {}

        for page in pages:
            source_page = local_pages_by_number.get(page.page_number)
            if source_page is None:
                known_pages = sorted(local_pages_by_number.keys())
                raise RuntimeError(
                    f"Parsed page number {page.page_number} is not in chunk {chunk_index}, known pages: {known_pages}"
                )
            extracted = annotate_and_extract(
                source_page=source_page,
                page=page,
                annotated_pages_dir=annotated_pages_dir,
                extracted_images_dir=extracted_images_dir,
            )
            images_by_page[page.page_number] = extracted

        structured_json_path = chunk_dir / "structured_data.json"
        structured_json_path.write_text(
            json.dumps([page.model_dump() for page in pages],
                       indent=2,
                       ensure_ascii=False),
            encoding="utf-8",
        )

        save_chunk_to_database(
            session=session,
            document_id=document_id,
            chunk_index=chunk_index,
            raw_output=raw_text,
            pages=pages,
            image_elements_by_page=images_by_page,
        )

        chunks.append(
            OcrChunkResult(
                chunk_index=chunk_index,
                page_start=page_start,
                page_end=page_end,
                raw_text_file=str(raw_text_path),
                structured_json_file=str(structured_json_path),
                pages_dir=str(pages_dir),
                annotated_pages_dir=str(annotated_pages_dir),
                extracted_images_dir=str(extracted_images_dir),
                pages=pages,
            ))

    return OcrDocumentResult(
        source_file=str(source_file),
        relative_source="",
        output_dir=str(output_base),
        chunks=chunks,
    )


@beartype
def process_image(
    session: Session,
    model: Any,
    tokenizer: Any,
    document_id: int,
    source_file: Path,
    output_base: Path,
    prompt: str,
    image_size: int,
    max_length: int,
) -> OcrDocumentResult:
    output_base.mkdir(parents=True, exist_ok=True)

    chunk_index = 0
    chunk_dir = output_base / "chunks" / "chunk_0000"
    pages_dir = chunk_dir / "pages"
    annotated_pages_dir = chunk_dir / "annotated_pages"
    extracted_images_dir = chunk_dir / "images"
    model_tmp_dir = chunk_dir / "_model"

    if chunk_exists(session, document_id=document_id, chunk_index=chunk_index):
        row = get_chunk_record(session,
                               document_id=document_id,
                               chunk_index=chunk_index)
        pages = parse_pages_from_chunk_json(row.structured_json,
                                            document_id=document_id,
                                            chunk_index=chunk_index)
        return OcrDocumentResult(
            source_file=str(source_file),
            relative_source="",
            output_dir=str(output_base),
            chunks=[
                OcrChunkResult(
                    chunk_index=0,
                    page_start=1,
                    page_end=1,
                    raw_text_file=str(chunk_dir / "raw_text.txt"),
                    structured_json_file=str(chunk_dir /
                                             "structured_data.json"),
                    pages_dir=str(pages_dir),
                    annotated_pages_dir=str(annotated_pages_dir),
                    extracted_images_dir=str(extracted_images_dir),
                    pages=pages,
                )
            ],
        )

    chunk_dir.mkdir(parents=True, exist_ok=True)
    pages_dir.mkdir(parents=True, exist_ok=True)
    annotated_pages_dir.mkdir(parents=True, exist_ok=True)
    extracted_images_dir.mkdir(parents=True, exist_ok=True)
    model_tmp_dir.mkdir(parents=True, exist_ok=True)

    page_copy = pages_dir / f"page_0001{source_file.suffix.lower()}"
    shutil.copy2(source_file, page_copy)

    raw_text = run_chunk_infer_single(
        model=model,
        tokenizer=tokenizer,
        image_file=page_copy,
        prompt=prompt,
        output_path=model_tmp_dir,
        image_size=image_size,
        max_length=max_length,
    )

    raw_text_path = chunk_dir / "raw_text.txt"
    raw_text_path.write_text(raw_text, encoding="utf-8")

    pages = parse_ocr_output(raw_text, page_offset=0)
    if not pages:
        pages = [OcrPage(page_number=1, elements=[])]

    images_by_page: dict[int, list[ExtractedImageElement]] = {}
    for page in pages:
        if page.page_number != 1:
            raise RuntimeError(
                f"Single image produced unexpected page number: {page.page_number}"
            )
        images_by_page[page.page_number] = annotate_and_extract(
            source_page=page_copy,
            page=page,
            annotated_pages_dir=annotated_pages_dir,
            extracted_images_dir=extracted_images_dir,
        )

    structured_json_path = chunk_dir / "structured_data.json"
    structured_json_path.write_text(
        json.dumps([page.model_dump() for page in pages],
                   indent=2,
                   ensure_ascii=False),
        encoding="utf-8",
    )

    save_chunk_to_database(
        session=session,
        document_id=document_id,
        chunk_index=0,
        raw_output=raw_text,
        pages=pages,
        image_elements_by_page=images_by_page,
    )

    chunk_result = OcrChunkResult(
        chunk_index=0,
        page_start=1,
        page_end=1,
        raw_text_file=str(raw_text_path),
        structured_json_file=str(structured_json_path),
        pages_dir=str(pages_dir),
        annotated_pages_dir=str(annotated_pages_dir),
        extracted_images_dir=str(extracted_images_dir),
        pages=pages,
    )

    return OcrDocumentResult(
        source_file=str(source_file),
        relative_source="",
        output_dir=str(output_base),
        chunks=[chunk_result],
    )


@beartype
def process_file(
    session: Session,
    model: Any,
    tokenizer: Any,
    source_file: Path,
    output_root: Path,
    mirror_from: Path,
    chunk_size: int,
    dpi: int,
    prompt_image: str,
    prompt_pdf: str,
    image_size: int,
    max_length: int,
    overwrite: bool,
    cleared_documents: set[int],
) -> None:
    output_base = mirrored_output_base(source_file, mirror_from, output_root)
    output_base.mkdir(parents=True, exist_ok=True)

    document_id, file_sha256 = ensure_document_and_input_file(
        session, source_file)

    should_clear = overwrite and document_id not in cleared_documents
    if should_clear:
        clear_document_data(session, document_id=document_id)
        cleared_documents.add(document_id)
        logger.info(
            f"Cleared indexed data for document_id={document_id} hash={file_sha256}"
        )

    ext = source_file.suffix.lower()
    match ext:
        case ".pdf":
            doc_result = process_pdf(
                session=session,
                model=model,
                tokenizer=tokenizer,
                document_id=document_id,
                source_file=source_file,
                output_base=output_base,
                chunk_size=chunk_size,
                dpi=dpi,
                prompt=prompt_pdf,
                image_size=image_size,
                max_length=max_length,
            )
        case _ if ext in IMAGE_EXTENSIONS:
            doc_result = process_image(
                session=session,
                model=model,
                tokenizer=tokenizer,
                document_id=document_id,
                source_file=source_file,
                output_base=output_base,
                prompt=prompt_image,
                image_size=image_size,
                max_length=max_length,
            )
        case _:
            raise ValueError(
                f"Unsupported extension for file {source_file}: {ext}")

    relative_source = source_file.resolve().relative_to(mirror_from).as_posix()
    doc_result.relative_source = relative_source

    doc_json_path = output_base / "document_result.json"
    doc_json_path.write_text(doc_result.model_dump_json(indent=2),
                             encoding="utf-8")
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
    model, tokenizer = load_model_and_tokenizer(args.model_id)

    failures: list[tuple[Path, Exception]] = []
    cleared_documents: set[int] = set()

    with session_factory() as session:
        for file_path in files:
            try:
                process_file(
                    session=session,
                    model=model,
                    tokenizer=tokenizer,
                    source_file=file_path,
                    output_root=output_dir,
                    mirror_from=mirror_from,
                    chunk_size=args.chunk_size,
                    dpi=args.dpi,
                    prompt_image=args.prompt_image,
                    prompt_pdf=args.prompt_pdf,
                    image_size=args.image_size,
                    max_length=args.max_length,
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

#!/usr/bin/env python
from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import Any
from sqlalchemy import (Boolean, ForeignKey, LargeBinary, String, Text,
                        UniqueConstraint, create_engine, delete, event, select)
from sqlalchemy.orm import (DeclarativeBase, Mapped, Session, mapped_column,
                            sessionmaker)

from ocr_unlimited_models import OcrPage


@dataclass(frozen=True)
class ExtractedImageElement:
    element_index: int
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
        String(64), ForeignKey("documents.file_sha256"), nullable=False)


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
    enabled: Mapped[bool] = mapped_column(Boolean,
                                          nullable=False,
                                          default=True)


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

    return [OcrPage.model_validate(item) for item in payload]


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
                enabled=True,
            )
            session.add(row)

    session.commit()

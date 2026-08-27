#!/usr/bin/env python
from __future__ import annotations
from dataclasses import dataclass

from pydantic import BaseModel, Field


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
    raw_text: str
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
class OcrExtractedImage:
    page_number: int
    element_index: int
    image_blob: bytes


@dataclass(frozen=True)
class OcrChunkOcrResult:
    """Ocr-OCR specific result for one processed chunk."""

    pages: list[OcrPage]
    extracted_images: list[OcrExtractedImage]

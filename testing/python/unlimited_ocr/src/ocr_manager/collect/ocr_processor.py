#!/usr/bin/env python
from __future__ import annotations

import base64
import io
import shutil
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

import requests
from beartype import beartype
from docling_core.types.doc.document import (
    ContentLayer,
    DoclingDocument,
    DocTagsDocument,
    DocTagsPage,
)
from loguru import logger
from PIL import Image

from ocr_manager.collect.ocr_unlimited import render_pdf_pages
from src.ocr_manager.collect.ocr_models import (
    OcrBBox,
    OcrElement,
    OcrPage,
)

DEFAULT_DOCLING_MODEL_ID = "ibm/granite-docling"
DEFAULT_LLAMA_SERVER_URL = "http://localhost:8080"
DEFAULT_REQUEST_THREADS = 1


@dataclass(frozen=True)
class DoclingExtractedImage:
    page_number: int
    element_index: int
    image_blob: bytes


@dataclass(frozen=True)
class DoclingChunkOcrResult:
    """Docling-OCR specific result for one processed chunk."""

    chunk_index: int
    raw_text: str
    pages: list[OcrPage]
    extracted_images: list[DoclingExtractedImage]


@beartype
def check_llama_server(
    llama_server_url: str,
    request_timeout: int,
) -> None:
    response = requests.get(
        f"{llama_server_url.rstrip('/')}/health",
        timeout=request_timeout,
    )
    response.raise_for_status()

    logger.info(f"Connected to llama.cpp server at {llama_server_url}")


@beartype
def normalize_bbox(
    left: float,
    top: float,
    right: float,
    bottom: float,
    width: int,
    height: int,
) -> OcrBBox:
    x1 = max(0, min(999, int(left / width * 999)))
    y1 = max(0, min(999, int(top / height * 999)))
    x2 = max(0, min(999, int(right / width * 999)))
    y2 = max(0, min(999, int(bottom / height * 999)))

    if x2 < x1:
        x1, x2 = x2, x1

    if y2 < y1:
        y1, y2 = y2, y1

    return OcrBBox(
        x1=x1,
        y1=y1,
        x2=x2,
        y2=y2,
    )


@beartype
def parse_doctags_page(
    raw_text: str,
    page_number: int,
    page_width: int,
    page_height: int,
) -> OcrPage:
    content = raw_text.strip()

    if not content:
        raise RuntimeError(
            f"Model returned empty output for page {page_number}")

    if not content.startswith("<doctag>"):
        content = f"<doctag>{content}</doctag>"

    doc_tags = DocTagsDocument(pages=[
        DocTagsPage(tokens=content),
    ])
    document = DoclingDocument.load_from_doctags(doctag_document=doc_tags, )

    elements: list[OcrElement] = []

    for item, _ in document.iterate_items(
            included_content_layers=set(ContentLayer),
            with_groups=False,
    ):
        if hasattr(item, "label") and item.label:
            label = str(item.label.value)
        else:
            label = type(item).__name__

        text = getattr(item, "text", "") or ""
        bbox: OcrBBox | None = None

        if hasattr(item, "prov") and item.prov:
            provenance = item.prov[0]

            if hasattr(provenance, "bbox") and provenance.bbox:
                bbox = normalize_bbox(
                    left=provenance.bbox.l,
                    top=provenance.bbox.t,
                    right=provenance.bbox.r,
                    bottom=provenance.bbox.b,
                    width=page_width,
                    height=page_height,
                )

        if bbox is None:
            continue

        elements.append(OcrElement(
            label=label,
            bbox=bbox,
            text=text.strip(),
        ))

    logger.info(f"Parsed page {page_number}: found {len(elements)} elements")

    return OcrPage(
        page_number=page_number,
        elements=elements,
    )


class DoclingOcrProcessor:
    """Runs Docling OCR through a remote llama.cpp server."""

    @beartype
    def __init__(
        self,
        model_id: str = DEFAULT_DOCLING_MODEL_ID,
        llama_server_url: str = DEFAULT_LLAMA_SERVER_URL,
        dpi: int = 300,
        prompt: str = "Convert this page to docling.",
        request_timeout: int = 600,
        request_threads: int = DEFAULT_REQUEST_THREADS,
    ) -> None:
        self.model_id = model_id
        self.llama_server_url = llama_server_url.rstrip("/")
        self.dpi = dpi
        self.prompt = prompt
        self.request_timeout = request_timeout
        self.request_threads = request_threads

        check_llama_server(
            llama_server_url=self.llama_server_url,
            request_timeout=self.request_timeout,
        )

    @beartype
    def render_pages(
        self,
        source_file: Path,
        rendered_dir: Path,
    ) -> list[Path]:
        extension = source_file.suffix.lower()

        if extension == ".pdf":
            return render_pdf_pages(
                source_file,
                rendered_dir,
                dpi=self.dpi,
            )

        rendered_dir.mkdir(parents=True, exist_ok=True)
        page_copy = rendered_dir / f"page_0001{extension}"
        shutil.copy2(source_file, page_copy)

        return [page_copy]

    @beartype
    def call_vlm(
        self,
        image: Image.Image,
        page_number: int,
    ) -> str:
        image_buffer = io.BytesIO()
        image.save(
            image_buffer,
            format="JPEG",
            quality=95,
        )

        encoded_image = base64.b64encode(
            image_buffer.getvalue()).decode("ascii")

        response = requests.post(
            f"{self.llama_server_url}/v1/chat/completions",
            json={
                "model":
                self.model_id,
                "messages": [
                    {
                        "role":
                        "user",
                        "content": [
                            {
                                "type": "text",
                                "text": self.prompt,
                            },
                            {
                                "type": "image_url",
                                "image_url": {
                                    "url": ("data:image/jpeg;base64,"
                                            f"{encoded_image}"),
                                },
                            },
                        ],
                    },
                ],
                "temperature":
                0,
                "stream":
                False,
            },
            timeout=self.request_timeout,
        )
        response.raise_for_status()

        response_data = response.json()
        raw_text = response_data["choices"][0]["message"]["content"]

        if not isinstance(raw_text, str) or not raw_text.strip():
            raise RuntimeError(
                f"Model returned empty output for page {page_number}")

        return raw_text

    @beartype
    def process_page(
        self,
        source_page: Path,
        page_number: int,
        pages_dir: Path,
        extracted_images_dir: Path,
    ) -> tuple[str, OcrPage, list[DoclingExtractedImage]]:
        destination = pages_dir / source_page.name
        shutil.copy2(source_page, destination)

        logger.info(
            f"Running Docling OCR for page {page_number}, {destination}")

        with Image.open(destination) as source_image:
            image = source_image.convert("RGB")

        width, height = image.size
        raw_text = self.call_vlm(
            image=image,
            page_number=page_number,
        )

        page = parse_doctags_page(
            raw_text=raw_text,
            page_number=page_number,
            page_width=width,
            page_height=height,
        )

        extracted_images: list[DoclingExtractedImage] = []
        page_image_index = 0

        for element_index, element in enumerate(page.elements):
            if element.label.strip().casefold() not in {
                    "picture",
                    "image",
            }:
                continue

            x1 = int(element.bbox.x1 / 999 * width)
            y1 = int(element.bbox.y1 / 999 * height)
            x2 = int(element.bbox.x2 / 999 * width)
            y2 = int(element.bbox.y2 / 999 * height)

            crop = image.crop((x1, y1, x2, y2))
            image_path = (extracted_images_dir /
                          (f"page_{page_number:04d}_"
                           f"image_{page_image_index:04d}.png"))
            crop.save(image_path, format="PNG")

            image_buffer = io.BytesIO()
            crop.save(image_buffer, format="PNG")

            extracted_images.append(
                DoclingExtractedImage(
                    page_number=page_number,
                    element_index=element_index,
                    image_blob=image_buffer.getvalue(),
                ))

            page_image_index += 1

        logger.info(f"Completed page {page_number}: "
                    f"{len(raw_text)} characters, "
                    f"{len(extracted_images)} extracted images")

        return raw_text, page, extracted_images

    @beartype
    def process_chunk(
        self,
        source_file: Path,
        chunk_index: int,
        chunk_page_files: list[Path],
        page_offset: int,
        chunk_dir: Path,
    ) -> DoclingChunkOcrResult:
        """OCR one chunk of page images through llama.cpp."""

        del source_file

        pages_dir = chunk_dir / "pages"
        extracted_images_dir = chunk_dir / "images"

        pages_dir.mkdir(parents=True, exist_ok=True)
        extracted_images_dir.mkdir(parents=True, exist_ok=True)

        page_entries = [(source_page, page_offset + index + 1)
                        for index, source_page in enumerate(chunk_page_files)]

        def process_page_entry(
            entry: tuple[Path, int],
        ) -> tuple[str, OcrPage, list[DoclingExtractedImage]]:
            source_page, page_number = entry

            return self.process_page(
                source_page=source_page,
                page_number=page_number,
                pages_dir=pages_dir,
                extracted_images_dir=extracted_images_dir,
            )

        raw_parts: list[str] = []
        pages: list[OcrPage] = []
        extracted_images: list[DoclingExtractedImage] = []

        with ThreadPoolExecutor(max_workers=self.request_threads) as executor:
            for raw_text, page, page_images in executor.map(
                    process_page_entry,
                    page_entries,
            ):
                raw_parts.append(raw_text)
                pages.append(page)
                extracted_images.extend(page_images)

        combined_raw_text = "\n<PAGE>\n".join(raw_parts)
        (chunk_dir / "raw_text.txt").write_text(
            combined_raw_text,
            encoding="utf-8",
        )

        return DoclingChunkOcrResult(
            chunk_index=chunk_index,
            raw_text=combined_raw_text,
            pages=pages,
            extracted_images=extracted_images,
        )

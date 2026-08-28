#!/usr/bin/env python
from __future__ import annotations

import base64
import io
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import pymupdf
import requests
from beartype import beartype
from docling_core.types.doc.document import (
    ContentLayer,  # type: ignore
    DoclingDocument,  # type: ignore
    DocTagsDocument,  # type: ignore
    DocTagsPage,  # type: ignore
)
from loguru import logger
from PIL import Image as PILImage

from ocr_db.collect.ocr_models import (
    OcrBBox,
    OcrChunkOcrResult,
    OcrElement,
    OcrExtractedImage,
    OcrPage,
)
from ocr_db.collect.pdf_render import render_pdf_pages


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


def clamp(value, min_val, max_val):
    if value < min_val:
        return min_val
    if value > max_val:
        return max_val
    return value


@beartype
def normalize_bbox(
    left: float,
    top: float,
    right: float,
    bottom: float,
    width: int,
    height: int,
) -> OcrBBox:
    x1 = left / float(width)
    y1 = top / float(height)
    x2 = right / float(width)
    y2 = bottom / float(height)

    if x2 < x1:
        x1, x2 = x2, x1

    if y2 < y1:
        y1, y2 = y2, y1

    x1 = clamp(x1, 0, 1)
    x2 = clamp(x2, 0, 1)
    y1 = clamp(y1, 0, 1)
    y2 = clamp(y2, 0, 1)

    # logger.debug(f"left={left} right={right} top={top} bottom={bottom} width={width} height={height} -> x1={x1} x2={x2} y1={y1} y2={y2}")
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
    page_image: PILImage.Image,
) -> OcrPage:
    content = raw_text.strip()

    if not content:
        raise RuntimeError(
            f"Model returned empty output for page {page_number}")

    doc_tags = DocTagsDocument.from_doctags_and_image_pairs(
        doctags=[content],
        images=[page_image],
    )

    document = DoclingDocument.load_from_doctags(doctag_document=doc_tags)
    elements: list[OcrElement] = []

    for item, _ in document.iterate_items(
            included_content_layers=set(ContentLayer),
            with_groups=False,
    ):
        if hasattr(item, "label") and item.label:  # type: ignore
            label = str(item.label.value)  # type: ignore
        else:
            label = type(item).__name__

        text = getattr(item, "text", "") or ""
        bbox: OcrBBox | None = None

        if hasattr(item, "prov") and item.prov:  # type: ignore
            provenance = item.prov[0]  # type: ignore

            if hasattr(provenance, "bbox") and provenance.bbox:  # type: ignore
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

    return OcrPage(
        page_number=page_number,
        elements=elements,
        raw_text=raw_text,
        document=document,
    )


class OcrProcessor:
    """Runs OCR through a remote llama.cpp server."""

    @beartype
    def __init__(
        self,
        request_threads: int,
        raster_threads: int,
        model_id: str,
        llama_server_url: str,
        dpi: int,
        request_timeout: int = 600,
    ) -> None:
        self.model_id = model_id
        self.llama_server_url = llama_server_url.rstrip("/")
        self.dpi = dpi
        self.request_timeout = request_timeout
        self.request_threads = request_threads
        self.raster_threads = raster_threads

        check_llama_server(
            llama_server_url=self.llama_server_url,
            request_timeout=self.request_timeout,
        )

    @beartype
    def call_model(
        self,
        image: PILImage.Image,
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
                                "text": "Convert this page to docling."
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
    ) -> tuple[OcrPage, list[OcrExtractedImage]]:
        logger.info(f"Running OCR for page {page_number}, {source_page}")

        with PILImage.open(source_page) as source_image:
            image = source_image.convert("RGB")

        width, height = image.size
        raw_text = self.call_model(
            image=image,
            page_number=page_number,
        )

        page = parse_doctags_page(
            raw_text=raw_text,
            page_number=page_number,
            page_width=width,
            page_height=height,
            page_image=image,
        )

        extracted_images: list[OcrExtractedImage] = []

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

            if x2 <= x1 or y2 <= y1:
                continue

            crop = image.crop((x1, y1, x2, y2))
            image_buffer = io.BytesIO()
            crop.save(image_buffer, format="PNG")

            extracted_images.append(
                OcrExtractedImage(
                    page_number=page_number,
                    element_index=element_index,
                    image_blob=image_buffer.getvalue(),
                ))

        logger.info(f"Completed page {page_number}: "
                    f"{len(raw_text)} characters, "
                    f"{len(extracted_images)} extracted images")

        return page, extracted_images

    @beartype
    def process_file(
        self,
        file: Path,
        indices: set[int],
    ) -> OcrChunkOcrResult:
        """Rasterize and OCR the requested PDF pages."""

        with pymupdf.open(file) as document:
            page_count = document.page_count

        target_pages = {
            page_index
            for page_index in indices if 0 <= page_index < page_count
        }

        if not target_pages:
            return OcrChunkOcrResult(
                pages=[],
                extracted_images=[],
            )
        rasterized_pages = render_pdf_pages(
            input_pdf=file,
            dpi=self.dpi,
            raster_threads=self.raster_threads,
            target_pages=target_pages,
        )

        missing_pages = target_pages.difference(rasterized_pages)

        if missing_pages:
            missing_page_numbers = sorted(page_index + 1
                                          for page_index in missing_pages)
            raise RuntimeError("PDF rasterization did not return pages "
                               f"{missing_page_numbers}")

        page_entries = [(rasterized_pages[page_index], page_index + 1)
                        for page_index in sorted(target_pages)]

        def process_page_entry(
            entry: tuple[Path,
                         int], ) -> tuple[OcrPage, list[OcrExtractedImage]]:
            source_page, page_number = entry

            return self.process_page(
                source_page=source_page,
                page_number=page_number,
            )

        pages: list[OcrPage] = []
        extracted_images: list[OcrExtractedImage] = []

        with ThreadPoolExecutor(max_workers=self.request_threads) as executor:
            for page, page_images in executor.map(
                    process_page_entry,
                    page_entries,
            ):
                pages.append(page)
                extracted_images.extend(page_images)

        return OcrChunkOcrResult(
            pages=pages,
            extracted_images=extracted_images,
        )

#!/usr/bin/env python
from __future__ import annotations

import base64
import io
import os
import shutil
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import click
import pymupdf
import requests
from docling_core.types.doc.document import (
    ContentLayer,
    DoclingDocument,
    DocTagsDocument,
    DocTagsPage,
)
from loguru import logger
from PIL import Image
from pydantic import BaseModel, Field

DEFAULT_MODEL_ID = "ibm/granite-docling"
DEFAULT_DPI = 300
DEFAULT_RASTER_THREADS = os.cpu_count() or 1
DEFAULT_REQUEST_THREADS = 1
DEFAULT_REQUEST_TIMEOUT = 600
DOCLING_PROMPT = "Convert this page to docling."


class OcrBBox(BaseModel):
    x1: int = Field(ge=0, le=999)
    y1: int = Field(ge=0, le=999)
    x2: int = Field(ge=0, le=999)
    y2: int = Field(ge=0, le=999)


class OcrElement(BaseModel):
    label: str = Field(min_length=1)
    bbox: OcrBBox
    text: str = ""


class OcrPage(BaseModel):
    page_number: int = Field(ge=1)
    raw_text: str
    elements: list[OcrElement] = Field(default_factory=list)


class OcrExtractedImage(BaseModel):
    page_number: int = Field(ge=1)
    element_index: int = Field(ge=0)
    file: str


class OcrDocumentResult(BaseModel):
    source_file: str
    model: str
    pages: list[OcrPage]
    extracted_images: list[OcrExtractedImage] = Field(default_factory=list)


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

    logger.info(f"Rendered page {page_number}: "
                f"{pixmap.width}x{pixmap.height}, "
                f"{page_path.stat().st_size} bytes")

    return page_path


def render_pdf_pages(
    input_pdf: Path,
    destination: Path,
    dpi: int,
    raster_threads: int,
) -> list[Path]:
    destination.mkdir(parents=True, exist_ok=True)

    with pymupdf.open(input_pdf) as document:
        page_count = document.page_count

    if page_count == 0:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")

    logger.info(f"Rendering {page_count} PDF pages at {dpi} DPI "
                f"using {raster_threads} threads")

    with ThreadPoolExecutor(max_workers=raster_threads) as executor:
        page_paths = list(
            executor.map(
                lambda page_index: render_pdf_page(
                    input_pdf=input_pdf,
                    destination=destination,
                    dpi=dpi,
                    page_index=page_index,
                ),
                range(page_count),
            ))

    total_size = sum(path.stat().st_size for path in page_paths)

    logger.info(
        f"Rendered {len(page_paths)} pages, total size {total_size} bytes")

    return page_paths


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
        raw_text=raw_text,
        elements=elements,
    )


class DoclingOcrProcessor:

    def __init__(
        self,
        model_id: str,
        llama_server_url: str,
        request_timeout: int,
    ) -> None:
        self.model_id = model_id
        self.llama_server_url = llama_server_url.rstrip("/")
        self.request_timeout = request_timeout

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
                                "text": DOCLING_PROMPT,
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

    def process_page(
        self,
        page_file: Path,
        page_number: int,
        extracted_images_directory: Path,
    ) -> tuple[OcrPage, list[OcrExtractedImage]]:
        logger.info(f"Running Docling OCR for page {page_number}, {page_file}")

        with Image.open(page_file) as source_image:
            image = source_image.convert("RGB")

        width, height = image.size
        raw_text = self.call_vlm(image, page_number)

        page = parse_doctags_page(
            raw_text=raw_text,
            page_number=page_number,
            page_width=width,
            page_height=height,
        )

        extracted_images: list[OcrExtractedImage] = []
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
            image_path = (extracted_images_directory /
                          (f"page_{page_number:06d}_"
                           f"image_{page_image_index:04d}.png"))
            crop.save(image_path, format="PNG")

            extracted_images.append(
                OcrExtractedImage(
                    page_number=page_number,
                    element_index=element_index,
                    file=str(image_path.resolve()),
                ))

            page_image_index += 1

        logger.info(f"Completed page {page_number}: "
                    f"{len(raw_text)} characters, "
                    f"{len(extracted_images)} extracted images")

        return page, extracted_images


def process_pdf(
    input_pdf: Path,
    output_json: Path,
    llama_server_url: str,
    model_id: str,
    dpi: int,
    raster_threads: int,
    request_threads: int,
    request_timeout: int,
) -> None:
    output_text = output_json.with_suffix(".txt")
    extracted_images_directory = (output_json.parent /
                                  f"{output_json.stem}_images")

    output_json.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    if extracted_images_directory.exists():
        shutil.rmtree(extracted_images_directory)

    extracted_images_directory.mkdir(
        parents=True,
        exist_ok=True,
    )

    check_llama_server(
        llama_server_url=llama_server_url,
        request_timeout=request_timeout,
    )

    logger.info(f"Processing {input_pdf} with model {model_id} through "
                f"llama.cpp at {llama_server_url}; "
                f"JSON output: {output_json}; text output: {output_text}; "
                f"parallel requests: {request_threads}")

    processor = DoclingOcrProcessor(
        model_id=model_id,
        llama_server_url=llama_server_url,
        request_timeout=request_timeout,
    )

    pages: list[OcrPage] = []
    extracted_images: list[OcrExtractedImage] = []

    with tempfile.TemporaryDirectory(
            prefix="docling-ocr-") as temporary_directory:
        rendered_directory = Path(temporary_directory) / "rendered"

        page_files = render_pdf_pages(
            input_pdf=input_pdf,
            destination=rendered_directory,
            dpi=dpi,
            raster_threads=raster_threads,
        )

        def process_page(
            page_entry: tuple[int, Path],
        ) -> tuple[OcrPage, list[OcrExtractedImage]]:
            page_number, page_file = page_entry

            return processor.process_page(
                page_file=page_file,
                page_number=page_number,
                extracted_images_directory=extracted_images_directory,
            )

        page_entries = list(enumerate(page_files, start=1))

        with (
                ThreadPoolExecutor(max_workers=request_threads) as executor,
                output_text.open("w", encoding="utf-8") as text_file,
        ):
            for page, page_images in executor.map(
                    process_page,
                    page_entries,
            ):
                pages.append(page)
                extracted_images.extend(page_images)

                if page.page_number > 1:
                    text_file.write("\n<PAGE>\n")

                text_file.write(page.raw_text)

                if not page.raw_text.endswith("\n"):
                    text_file.write("\n")

                text_file.flush()

        result = OcrDocumentResult(
            source_file=str(input_pdf.resolve()),
            model=model_id,
            pages=pages,
            extracted_images=extracted_images,
        )

        output_json.write_text(
            result.model_dump_json(indent=2),
            encoding="utf-8",
        )

    logger.info(f"Wrote {len(pages)} OCR pages to {output_json}")


@click.command(context_settings={
    "show_default": True,
})
@click.option(
    "--input",
    "input_pdf",
    required=True,
    type=click.Path(
        path_type=Path,
        exists=True,
        file_okay=True,
        dir_okay=False,
        readable=True,
    ),
    help="Input PDF path.",
)
@click.option(
    "--output",
    "output_json",
    required=True,
    type=click.Path(
        path_type=Path,
        file_okay=True,
        dir_okay=False,
        writable=True,
    ),
    help="Destination JSON path.",
)
@click.option(
    "--llama-server-url",
    required=True,
    help="llama.cpp HTTP server base URL.",
)
@click.option(
    "--model",
    "model_id",
    default=DEFAULT_MODEL_ID,
    help="Model identifier sent to the llama.cpp server.",
)
@click.option(
    "--dpi",
    default=DEFAULT_DPI,
    type=click.IntRange(min=1),
    help="PDF rendering DPI.",
)
@click.option(
    "--raster-threads",
    default=DEFAULT_RASTER_THREADS,
    type=click.IntRange(min=1),
    help="Number of concurrent PDF rasterization threads.",
)
@click.option(
    "--request-threads",
    default=DEFAULT_REQUEST_THREADS,
    type=click.IntRange(min=1),
    help="Number of concurrent llama.cpp requests.",
)
@click.option(
    "--request-timeout",
    default=DEFAULT_REQUEST_TIMEOUT,
    type=click.IntRange(min=1),
    help="llama.cpp request timeout in seconds.",
)
def main(
    input_pdf: Path,
    output_json: Path,
    llama_server_url: str,
    model_id: str,
    dpi: int,
    raster_threads: int,
    request_threads: int,
    request_timeout: int,
) -> None:
    if input_pdf.suffix.lower() != ".pdf":
        raise click.BadParameter(
            "input must be a PDF file",
            param_hint="--input",
        )

    if output_json.suffix.lower() != ".json":
        raise click.BadParameter(
            "output must use the .json extension",
            param_hint="--output",
        )

    if not llama_server_url.startswith(("http://", "https://")):
        raise click.BadParameter(
            "URL must start with http:// or https://",
            param_hint="--llama-server-url",
        )

    try:
        process_pdf(
            input_pdf=input_pdf,
            output_json=output_json,
            llama_server_url=llama_server_url,
            model_id=model_id,
            dpi=dpi,
            raster_threads=raster_threads,
            request_threads=request_threads,
            request_timeout=request_timeout,
        )
    except Exception:
        logger.exception("OCR processing failed")
        raise

    logger.info("OCR processing finished successfully")


if __name__ == "__main__":
    main()

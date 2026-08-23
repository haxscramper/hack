#!/usr/bin/env python
from __future__ import annotations

import base64
import io
import os
import shutil
import subprocess
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Literal

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
DEFAULT_LOCAL_OLLAMA_URL = "http://127.0.0.1:11434"
DEFAULT_DPI = 300
DEFAULT_RASTER_THREADS = os.cpu_count() or 1
DEFAULT_REQUEST_TIMEOUT = 600


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


def wait_for_ollama(
    ollama_url: str,
    attempts: int = 15,
) -> None:
    tags_url = f"{ollama_url.rstrip('/')}/api/tags"

    for _ in range(attempts):
        try:
            response = requests.get(tags_url, timeout=2)
            response.raise_for_status()
            return
        except requests.RequestException:
            time.sleep(1)

    raise RuntimeError(f"Failed to connect to Ollama at {ollama_url}")


def ensure_local_ollama_running(
    ollama_url: str,
    model_id: str,
) -> None:
    try:
        response = requests.get(
            f"{ollama_url.rstrip('/')}/api/tags",
            timeout=2,
        )
        response.raise_for_status()
    except requests.RequestException:
        logger.info("Ollama is not running; starting 'ollama serve'")

        subprocess.Popen(
            ["ollama", "serve"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )

        wait_for_ollama(ollama_url)

    model_check = subprocess.run(
        ["ollama", "show", model_id],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    if model_check.returncode != 0:
        logger.info(f"Pulling Ollama model {model_id}")
        subprocess.run(
            ["ollama", "pull", model_id],
            check=True,
        )


def ensure_remote_ollama_running(ollama_url: str) -> None:
    response = requests.get(
        f"{ollama_url.rstrip('/')}/api/tags",
        timeout=30,
    )
    response.raise_for_status()

    logger.info(f"Connected to remote Ollama instance at {ollama_url}")


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
        ollama_url: str,
        prompt: str,
        request_timeout: int,
    ) -> None:
        self.model_id = model_id
        self.ollama_url = ollama_url.rstrip("/")
        self.prompt = prompt
        self.request_timeout = request_timeout
        self.session = requests.Session()

    def close(self) -> None:
        self.session.close()

    def call_vlm(self, image: Image.Image, page_number: int) -> str:
        image_buffer = io.BytesIO()
        image.save(image_buffer, format="PNG")

        encoded_image = base64.b64encode(
            image_buffer.getvalue()).decode("ascii")

        response = self.session.post(
            f"{self.ollama_url}/api/chat",
            json={
                "model":
                self.model_id,
                "messages": [{
                    "role": "user",
                    "content": self.prompt,
                    "images": [encoded_image],
                }],
                "stream":
                False,
                "options": {
                    "temperature": 0,
                },
            },
            timeout=self.request_timeout,
        )
        response.raise_for_status()

        response_data = response.json()
        raw_text = response_data["message"]["content"]

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
    ollama_mode: Literal["local", "remote"],
    ollama_url: str,
    model_id: str,
    prompt: str,
    dpi: int,
    raster_threads: int,
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

    if ollama_mode == "local":
        ensure_local_ollama_running(
            ollama_url=ollama_url,
            model_id=model_id,
        )
    else:
        ensure_remote_ollama_running(ollama_url)

    logger.info(f"Processing {input_pdf} with model {model_id} through "
                f"{ollama_mode} Ollama at {ollama_url}; "
                f"JSON output: {output_json}; text output: {output_text}")

    processor = DoclingOcrProcessor(
        model_id=model_id,
        ollama_url=ollama_url,
        prompt=prompt,
        request_timeout=request_timeout,
    )

    pages: list[OcrPage] = []
    extracted_images: list[OcrExtractedImage] = []

    try:
        with tempfile.TemporaryDirectory(
                prefix="docling-ocr-") as temporary_directory:
            rendered_directory = (Path(temporary_directory) / "rendered")

            page_files = render_pdf_pages(
                input_pdf=input_pdf,
                destination=rendered_directory,
                dpi=dpi,
                raster_threads=raster_threads,
            )

            with output_text.open("w", encoding="utf-8") as text_file:
                for page_number, page_file in enumerate(
                        page_files,
                        start=1,
                ):
                    page, page_images = processor.process_page(
                        page_file=page_file,
                        page_number=page_number,
                        extracted_images_directory=(
                            extracted_images_directory),
                    )

                    pages.append(page)
                    extracted_images.extend(page_images)

                    if page_number > 1:
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
    finally:
        processor.close()

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
    "--ollama-mode",
    required=True,
    type=click.Choice(
        ["local", "remote"],
        case_sensitive=False,
    ),
    help=("Use and automatically manage a local Ollama instance, "
          "or connect to an existing remote instance."),
)
@click.option(
    "--ollama-url",
    help=("Ollama HTTP base URL. Defaults to "
          f"{DEFAULT_LOCAL_OLLAMA_URL} in local mode."),
)
@click.option(
    "--model",
    "model_id",
    default=DEFAULT_MODEL_ID,
    help="Ollama Docling model name.",
)
@click.option(
    "--prompt",
    default="Convert this page to docling.",
    help="Prompt sent with each page image.",
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
    "--request-timeout",
    default=DEFAULT_REQUEST_TIMEOUT,
    type=click.IntRange(min=1),
    help="Ollama request timeout in seconds.",
)
def main(
    input_pdf: Path,
    output_json: Path,
    ollama_mode: str,
    ollama_url: str | None,
    model_id: str,
    prompt: str,
    dpi: int,
    raster_threads: int,
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

    normalized_mode: Literal["local", "remote"]

    if ollama_mode.casefold() == "local":
        normalized_mode = "local"
        effective_ollama_url = (ollama_url or DEFAULT_LOCAL_OLLAMA_URL)
    else:
        normalized_mode = "remote"

        if ollama_url is None:
            raise click.BadParameter(
                "URL is required in remote mode",
                param_hint="--ollama-url",
            )

        effective_ollama_url = ollama_url

    if not effective_ollama_url.startswith(("http://", "https://")):
        raise click.BadParameter(
            "URL must start with http:// or https://",
            param_hint="--ollama-url",
        )

    try:
        process_pdf(
            input_pdf=input_pdf,
            output_json=output_json,
            ollama_mode=normalized_mode,
            ollama_url=effective_ollama_url,
            model_id=model_id,
            prompt=prompt,
            dpi=dpi,
            raster_threads=raster_threads,
            request_timeout=request_timeout,
        )
    except Exception:
        logger.exception("OCR processing failed")
        raise

    logger.info("OCR processing finished successfully")


if __name__ == "__main__":
    main()

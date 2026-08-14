#!/usr/bin/env python
from __future__ import annotations

import base64
import io
import shutil
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path

import requests
from beartype import beartype
from docling_core.types.doc.document import (ContentLayer, DoclingDocument,
                                             DocTagsDocument, DocTagsPage)
from loguru import logger
from PIL import Image

from ocr_unlimited import render_pdf_pages
from ocr_unlimited_models import OcrBBox, OcrElement, OcrPage

DEFAULT_DOCLING_MODEL_ID = "ibm/granite-docling"
DEFAULT_OLLAMA_URL = "http://localhost:11434"


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
def ensure_ollama_running(ollama_url: str, model_id: str) -> None:
    try:
        requests.get(f"{ollama_url}/api/tags", timeout=2)
    except requests.exceptions.RequestException:
        logger.info("Ollama is not running, starting 'ollama serve'")
        subprocess.Popen(
            ["ollama", "serve"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        for _ in range(15):
            try:
                requests.get(f"{ollama_url}/api/tags", timeout=2)
                break
            except requests.exceptions.RequestException:
                time.sleep(1)
        else:
            raise RuntimeError(
                f"Failed to start or connect to 'ollama serve' at {ollama_url}"
            )

    result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
    if model_id not in result.stdout:
        logger.info(f"Model {model_id} not found, pulling")
        subprocess.run(["ollama", "pull", model_id], check=True)


@beartype
def normalize_bbox(l: float, t: float, r: float, b: float, width: int,
                   height: int) -> OcrBBox:
    x1 = max(0, min(999, int(l / width * 999)))
    y1 = max(0, min(999, int(t / height * 999)))
    x2 = max(0, min(999, int(r / width * 999)))
    y2 = max(0, min(999, int(b / height * 999)))
    if x2 < x1:
        x1, x2 = x2, x1
    if y2 < y1:
        y1, y2 = y2, y1
    return OcrBBox(x1=x1, y1=y1, x2=x2, y2=y2)


@beartype
def parse_doctags_page(raw_text: str, page_number: int, page_width: int,
                       page_height: int) -> OcrPage:
    content = raw_text.strip()
    if not content.startswith("<doctag>"):
        content = f"<doctag>{content}</doctag>"

    doc_tags = DocTagsDocument(pages=[DocTagsPage(tokens=content)])
    doc = DoclingDocument.load_from_doctags(doctag_document=doc_tags)

    elements: list[OcrElement] = []
    for item, level in doc.iterate_items(
            included_content_layers=set(ContentLayer), with_groups=False):
        del level
        label = (str(item.label.value) if hasattr(item, "label") and item.label
                 else type(item).__name__)
        text = getattr(item, "text", "") or ""

        bbox = None
        if hasattr(item, "prov") and item.prov:
            prov = item.prov[0]
            if hasattr(prov, "bbox") and prov.bbox:
                bbox = normalize_bbox(prov.bbox.l, prov.bbox.t, prov.bbox.r,
                                      prov.bbox.b, page_width, page_height)

        if bbox is None:
            continue

        elements.append(OcrElement(label=label, bbox=bbox, text=text.strip()))

    return OcrPage(page_number=page_number, elements=elements)


class DoclingOcrProcessor:
    """Runs docling OCR over page images of a file via the Ollama API, chunk by chunk."""

    @beartype
    def __init__(
        self,
        model_id: str = DEFAULT_DOCLING_MODEL_ID,
        ollama_url: str = DEFAULT_OLLAMA_URL,
        dpi: int = 300,
        prompt: str = "Convert this page to docling.",
        request_timeout: int = 600,
    ) -> None:
        self.model_id = model_id
        self.ollama_url = ollama_url
        self.dpi = dpi
        self.prompt = prompt
        self.request_timeout = request_timeout
        ensure_ollama_running(ollama_url, model_id)

    @beartype
    def render_pages(self, source_file: Path,
                     rendered_dir: Path) -> list[Path]:
        ext = source_file.suffix.lower()
        if ext == ".pdf":
            return render_pdf_pages(source_file, rendered_dir, dpi=self.dpi)
        rendered_dir.mkdir(parents=True, exist_ok=True)
        page_copy = rendered_dir / f"page_0001{ext}"
        shutil.copy2(source_file, page_copy)
        return [page_copy]

    @beartype
    def call_vlm(self, image: Image.Image) -> str:
        buffered = io.BytesIO()
        image.save(buffered, format="PNG")
        base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

        payload = {
            "model":
            self.model_id,
            "messages": [{
                "role": "user",
                "content": self.prompt,
                "images": [base64_image],
            }],
            "stream":
            False,
            "options": {
                "temperature": 0
            },
        }

        response = requests.post(f"{self.ollama_url}/api/chat",
                                 json=payload,
                                 timeout=self.request_timeout)
        response.raise_for_status()
        return response.json().get("message", {}).get("content", "")

    @beartype
    def process_chunk(
        self,
        source_file: Path,
        chunk_index: int,
        chunk_page_files: list[Path],
        page_offset: int,
        chunk_dir: Path,
    ) -> DoclingChunkOcrResult:
        """OCR one chunk of page images, returning docling specific data."""
        pages_dir = chunk_dir / "pages"
        extracted_images_dir = chunk_dir / "images"
        for directory in (pages_dir, extracted_images_dir):
            directory.mkdir(parents=True, exist_ok=True)

        raw_parts: list[str] = []
        pages: list[OcrPage] = []
        extracted_images: list[DoclingExtractedImage] = []

        for idx, source_page in enumerate(chunk_page_files):
            page_number = page_offset + idx + 1
            destination = pages_dir / source_page.name
            shutil.copy2(source_page, destination)

            image = Image.open(destination).convert("RGB")
            width, height = image.size

            raw_text = self.call_vlm(image)
            logger.info(f"{raw_text}")
            raw_parts.append(raw_text)

            page = parse_doctags_page(raw_text, page_number, width, height)
            pages.append(page)

            image_crop_index = 0
            for element_index, element in enumerate(page.elements):
                if element.label.strip().casefold() not in ("picture",
                                                            "image"):
                    continue
                x1 = int(element.bbox.x1 / 999 * width)
                y1 = int(element.bbox.y1 / 999 * height)
                x2 = int(element.bbox.x2 / 999 * width)
                y2 = int(element.bbox.y2 / 999 * height)
                crop = image.crop((x1, y1, x2, y2))
                crop.save(
                    extracted_images_dir /
                    f"page_{page_number:04d}_image_{image_crop_index:04d}.png")
                image_crop_index += 1

                buffer = io.BytesIO()
                crop.save(buffer, format="PNG")
                extracted_images.append(
                    DoclingExtractedImage(
                        page_number=page_number,
                        element_index=element_index,
                        image_blob=buffer.getvalue(),
                    ))

        raw_text = "\n<PAGE>\n".join(raw_parts)
        (chunk_dir / "raw_text.txt").write_text(raw_text, encoding="utf-8")

        return DoclingChunkOcrResult(
            chunk_index=chunk_index,
            raw_text=raw_text,
            pages=pages,
            extracted_images=extracted_images,
        )

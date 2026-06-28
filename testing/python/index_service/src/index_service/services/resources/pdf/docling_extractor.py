from __future__ import annotations

import base64
import io
import logging
import subprocess
import time
from pathlib import Path
from typing import Literal

import fitz
import requests
from PIL import Image as PILImage
from pydantic import BaseModel

from index_service.services.resources.pdf.page_model import DocTag

from docling_core.types.doc.document import (
    DocTagsDocument,
    DocTagsPage,
    DoclingDocument,
    ContentLayer,
)

log = logging.getLogger(__name__)


class DoclingPage(BaseModel, extra="forbid"):
    extractor: Literal["docling"] = "docling"
    page_number: int
    raw_docling_response: str
    spatial_tags: list[DocTag]


def _ensure_ollama_running(model_name: str, ollama_url: str) -> None:
    """Ensures that ollama serve is running and the model is pulled."""
    base_url = ollama_url.rsplit("/api/", 1)[0]

    try:
        requests.get(f"{base_url}/api/tags", timeout=2)
    except requests.exceptions.RequestException:
        log.info("Ollama is not running. Starting 'ollama serve'...")
        subprocess.Popen(
            ["ollama", "serve"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        for _ in range(15):
            try:
                requests.get(f"{base_url}/api/tags", timeout=2)
                break
            except requests.exceptions.RequestException:
                time.sleep(1)
        else:
            raise RuntimeError("Failed to start or connect to 'ollama serve'")

    log.info(f"Checking if model {model_name} is available...")
    result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
    if model_name not in result.stdout:
        log.info(
            f"Model {model_name} not found. Pulling (this might take a while)..."
        )
        subprocess.run(["ollama", "pull", model_name], check=True)
        log.info(f"Model {model_name} pulled successfully.")


def _rasterize_page(pdf_path: Path, page_num: int, raster_dir: Path) -> Path:
    """Renders a single PDF page to a PNG file using fitz."""
    with fitz.open(str(pdf_path)) as pdf:
        page = pdf[page_num - 1]
        mat = fitz.Matrix(2, 2)
        pix = page.get_pixmap(matrix=mat)
        output_path = raster_dir / f"page_{page_num:04d}.png"
        pix.save(str(output_path))
    return output_path


def parse_spatial_tags(raw_xml: str, page_num: int) -> list[DocTag]:
    """
    Parses the raw XML from Ollama/Docling using docling_core
    to extract spatial tags, bounding boxes, and hierarchical data.
    """
    root_tag = DocTag(id=f"page{page_num}-root", tag_name="root", text="")

    doc_tags = DocTagsDocument(pages=[DocTagsPage(tokens=raw_xml)])
    doc = DoclingDocument.load_from_doctags(doctag_document=doc_tags)

    tag_counter = 1
    for item, level in doc.iterate_items(
            included_content_layers=set(ContentLayer), with_groups=True):
        tag_name = (str(item.label.value) if hasattr(item, "label")
                    and item.label else type(item).__name__)
        text_content = getattr(item, "text", "")

        bbox = None
        if hasattr(item, "prov") and item.prov and len(item.prov) > 0:
            p = item.prov[0]
            if hasattr(p, "bbox") and p.bbox:
                bbox = BoundingBox(
                    x=p.bbox.l,
                    y=p.bbox.t,
                    width=p.bbox.r - p.bbox.l,
                    height=p.bbox.b - p.bbox.t,
                )

        nested_tag = DocTag(
            id=f"page{page_num}-{tag_name}-{tag_counter}",
            tag_name=tag_name,
            text=text_content,
            bbox=bbox,
        )
        root_tag.nested.append(nested_tag)
        tag_counter += 1

    return [root_tag]


class DoclingExtractor:
    """Extracts structured tags from PDF pages using the Ollama-hosted docling VLM."""

    def __init__(
        self,
        ollama_url: str = "http://localhost:11434/api/chat",
        model_name: str = "ibm/granite-docling",
        timeout: int = 600,
    ) -> None:
        self.ollama_url = ollama_url
        self.model_name = model_name
        self.timeout = timeout
        _ensure_ollama_running(model_name, ollama_url)

    def _call_ollama_vlm(self, image: PILImage.Image) -> str:
        """Sends image to Ollama API and returns the response text."""
        buffered = io.BytesIO()
        image.save(buffered, format="PNG")
        base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

        payload = {
            "model":
            self.model_name,
            "messages": [{
                "role": "user",
                "content": "Convert this page to docling.",
                "images": [base64_image],
            }],
            "stream":
            False,
            "options": {
                "temperature": 0
            },
        }

        response = requests.post(self.ollama_url,
                                 json=payload,
                                 timeout=self.timeout)
        response.raise_for_status()

        return response.json().get("message", {}).get("content", "")

    def extract_page(self, pdf_path: Path, page_num: int,
                     raster_dir: Path) -> DoclingPage:
        log.info(f"Docling extracting page {page_num} from {pdf_path.name}")

        image_path = _rasterize_page(pdf_path, page_num, raster_dir)
        image = PILImage.open(str(image_path))

        response_text = self._call_ollama_vlm(image)

        clean_text = response_text.strip()
        if not clean_text.startswith("<doctag>"):
            clean_text = f"<doctag>{clean_text}</doctag>"

        spatial_tags = parse_spatial_tags(clean_text, page_num)

        return DoclingPage(
            page_number=page_num,
            raw_docling_response=clean_text,
            spatial_tags=spatial_tags,
        )

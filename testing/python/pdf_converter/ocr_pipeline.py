import io
import json
import base64
import logging
import hashlib
import time
from pathlib import Path
from beartype.typing import List, Optional
from beartype import beartype
from datetime import timedelta

import requests
from PIL import Image as PILImage
from pdf2image import convert_from_path, pdfinfo_from_path
from xdg_base_dirs import xdg_cache_home

from models import PageData, DocTag
from config import AppConfig
import re
import fitz
import pymupdf4llm

# --- Configuration ---
OLLAMA_URL = "http://localhost:11434/api/chat"
MODEL_NAME = "ibm/granite-docling"  # Name as it appears in 'ollama list'

logger = logging.getLogger(__name__)


@beartype
def get_cache_path(pdf_path: Path, page_num: int) -> Path:
    """Generates an XDG-compliant cache path for the specific page image."""
    cache_dir = xdg_cache_home() / "docling-vlm-cache" / "images"
    cache_dir.mkdir(parents=True, exist_ok=True)

    with open(pdf_path, "rb") as f:
        file_prefix = f.read(16).hex()[:16]

    name_hash = hashlib.md5(str(pdf_path.absolute()).encode()).hexdigest()[:16]
    return cache_dir / f"{file_prefix}_{name_hash}_{page_num}.png"


@beartype
def format_eta(seconds: float) -> str:
    if seconds < 0:
        return "00:00:00"
    return str(timedelta(seconds=int(seconds)))


@beartype
def call_ollama_vlm(image: PILImage.Image) -> str:
    """Sends image to Ollama API and returns the response text."""
    buffered = io.BytesIO()
    image.save(buffered, format="PNG")
    base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

    payload = {
        "model": MODEL_NAME,
        "messages": [
            {
                "role": "user",
                "content": "Convert this page to docling.",
                "images": [base64_image],
            }
        ],
        "stream": False,
        "options": {"temperature": 0},
    }

    response = requests.post(OLLAMA_URL, json=payload, timeout=600)
    response.raise_for_status()

    return response.json().get("message", {}).get("content", "")


import re
from docling_core.types.doc.document import (
    DocTagsDocument,
    DocTagsPage,
    DoclingDocument,
    ContentLayer,
)
from models import BoundingBox


@beartype
def parse_spatial_tags(raw_xml: str, page_num: int) -> List[DocTag]:
    """
    Parses the raw XML from Ollama/Docling using docling_core
    to extract spatial tags, bounding boxes, and hierarchical data.
    """
    root_tag = DocTag(id=f"page{page_num}-root", tag_name="root", text="")

    try:
        doc_tags = DocTagsDocument(pages=[DocTagsPage(tokens=raw_xml)])
        doc = DoclingDocument.load_from_doctags(doctag_document=doc_tags)

        tag_counter = 1
        for item, level in doc.iterate_items(
            included_content_layers=set(ContentLayer), with_groups=True
        ):
            tag_name = (
                str(item.label.value)
                if hasattr(item, "label") and item.label
                else type(item).__name__
            )
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

            child_tag = DocTag(
                id=f"page{page_num}-{tag_name}-{tag_counter}",
                tag_name=tag_name,
                text=text_content,
                bbox=bbox,
            )
            root_tag.children.append(child_tag)
            tag_counter += 1

    except Exception as e:
        logger.error(f"Failed to parse spatial tags using docling_core: {e}")

    return [root_tag]


@beartype
def page_has_selectable_text(pdf: fitz.Document, page_num: int) -> bool:
    """Check if a given page (1-indexed) has selectable text."""
    page = pdf[page_num - 1]
    text = page.get_text("text")
    return len(text.strip()) > 20


@beartype
def _extract_block_text(block: dict) -> str:
    for key in ("text", "md", "content", "html"):
        value = block.get(key)
        if isinstance(value, str) and value.strip():
            return value.strip()

    lines = block.get("lines")
    if isinstance(lines, list):
        line_texts = []
        for line in lines:
            if isinstance(line, dict):
                spans = line.get("spans")
                if isinstance(spans, list):
                    span_text = "".join(
                        str(span.get("text", ""))
                        for span in spans
                        if isinstance(span, dict)
                    ).strip()
                    if span_text:
                        line_texts.append(span_text)
                        continue

                line_text = line.get("text")
                if isinstance(line_text, str) and line_text.strip():
                    line_texts.append(line_text.strip())
        if line_texts:
            return "\n".join(line_texts)

    return ""


@beartype
def _extract_block_bbox(
    block: dict,
    page_width: Optional[float],
    page_height: Optional[float],
) -> Optional[BoundingBox]:
    raw_bbox = block.get("bbox")
    if isinstance(raw_bbox, dict):
        x0 = raw_bbox.get("x0", raw_bbox.get("left"))
        y0 = raw_bbox.get("y0", raw_bbox.get("top"))
        x1 = raw_bbox.get("x1", raw_bbox.get("right"))
        y1 = raw_bbox.get("y1", raw_bbox.get("bottom"))
    elif isinstance(raw_bbox, (list, tuple)) and len(raw_bbox) >= 4:
        x0, y0, x1, y1 = raw_bbox[:4]
    else:
        return None

    if not all(isinstance(v, (int, float)) for v in (x0, y0, x1, y1)):
        return None

    x0 = float(x0)
    y0 = float(y0)
    x1 = float(x1)
    y1 = float(y1)

    if page_width and page_height:
        return BoundingBox(
            x=x0 / page_width,
            y=y0 / page_height,
            width=(x1 - x0) / page_width,
            height=(y1 - y0) / page_height,
        )

    return BoundingBox(
        x=x0,
        y=y0,
        width=x1 - x0,
        height=y1 - y0,
    )


@beartype
def _collect_block_attributes(block: dict) -> dict:
    attrs = {}

    for key in ("type", "level", "section", "header_level", "caption", "kind"):
        if key in block:
            attrs[key] = block[key]

    if "bbox" in block:
        attrs["source_bbox"] = block["bbox"]

    return attrs


@beartype
def _attach_table_children(
    table_tag: DocTag,
    block: dict,
    page_num: int,
    parent_counter: int,
    page_width: Optional[float],
    page_height: Optional[float],
) -> None:
    rows = block.get("rows")
    if not isinstance(rows, list):
        return

    child_counter = 1
    for row in rows:
        if not isinstance(row, list):
            continue

        row_text_parts = []
        for cell in row:
            if isinstance(cell, dict):
                cell_text = str(cell.get("text", "")).strip()
            else:
                cell_text = str(cell).strip()
            row_text_parts.append(cell_text)

        row_text = "\t".join(row_text_parts).strip()
        if not row_text:
            continue

        row_tag = DocTag(
            id=f"page{page_num}-table-{parent_counter}-row-{child_counter}",
            tag_name="text",
            text=row_text,
            bbox=None,
        )
        table_tag.children.append(row_tag)
        child_counter += 1


@beartype
def _extend_parent_bbox(parent: DocTag, child_bbox: Optional[BoundingBox]) -> None:
    if child_bbox is None:
        return

    if parent.bbox is None:
        parent.bbox = BoundingBox(
            x=child_bbox.x,
            y=child_bbox.y,
            width=child_bbox.width,
            height=child_bbox.height,
        )
        return

    x0 = min(parent.bbox.x, child_bbox.x)
    y0 = min(parent.bbox.y, child_bbox.y)
    x1 = max(parent.bbox.x + parent.bbox.width, child_bbox.x + child_bbox.width)
    y1 = max(parent.bbox.y + parent.bbox.height, child_bbox.y + child_bbox.height)

    parent.bbox = BoundingBox(
        x=x0,
        y=y0,
        width=x1 - x0,
        height=y1 - y0,
    )


@beartype
def _looks_like_list_item(text: str) -> bool:
    return bool(re.match(r"^\s*(?:[-•*]|(?:\d+|[a-zA-Z]+)[.)])\s+", text))


@beartype
def _looks_like_section_header(block: dict, text: str) -> bool:
    if not text or len(text) > 200:
        return False

    level = block.get("level")
    if isinstance(level, int) and level > 0:
        return True

    header_level = block.get("header_level")
    if isinstance(header_level, int) and header_level > 0:
        return True

    if text == text.upper() and len(text.split()) <= 12:
        return True

    if text.endswith(":") and len(text.split()) <= 12:
        return True

    return False


@beartype
def _looks_like_page_footer(text: str) -> bool:
    stripped = text.strip()
    if not stripped:
        return False

    if re.fullmatch(r"\d+", stripped):
        return True

    if re.fullmatch(r"page\s+\d+", stripped.lower()):
        return True

    return False


@beartype
def _infer_tag_name(block: dict) -> str:
    block_type = str(block.get("type", "")).lower()
    text = _extract_block_text(block).strip()

    if block_type in {"table"}:
        return "table"
    if block_type in {"image", "picture", "figure"}:
        return "picture"
    if block_type in {"formula", "equation"}:
        return "formula"
    if block_type in {"footnote"}:
        return "footnote"
    if block_type in {"footer", "page_footer"}:
        return "page_footer"
    if block_type in {"list", "list_item", "bullet_list_item"}:
        return "list_item"
    if block_type in {"header", "heading", "title", "section_header"}:
        return "section_header"

    if _looks_like_list_item(text):
        return "list_item"
    if _looks_like_section_header(block, text):
        return "section_header"
    if _looks_like_page_footer(text):
        return "page_footer"

    if text:
        return "text"

    return "unspecified"


@beartype
def _collect_page_box_attributes(box: dict) -> dict:
    attrs = {}

    for key in ("index", "class", "bbox", "pos"):
        if key in box:
            attrs[key] = box[key]

    return attrs


@beartype
def _extract_page_box_bbox(
    box: dict,
    page_width: Optional[float],
    page_height: Optional[float],
) -> Optional[BoundingBox]:
    raw_bbox = box.get("bbox")
    if not isinstance(raw_bbox, (list, tuple)) or len(raw_bbox) < 4:
        return None

    x0, y0, x1, y1 = raw_bbox[:4]
    if not all(isinstance(v, (int, float)) for v in (x0, y0, x1, y1)):
        return None

    x0 = float(x0)
    y0 = float(y0)
    x1 = float(x1)
    y1 = float(y1)

    if page_width and page_height:
        return BoundingBox(
            x=x0 / page_width,
            y=y0 / page_height,
            width=(x1 - x0) / page_width,
            height=(y1 - y0) / page_height,
        )

    return BoundingBox(
        x=x0,
        y=y0,
        width=x1 - x0,
        height=y1 - y0,
    )


@beartype
def _extract_page_box_text(box: dict, page_text: str) -> str:
    pos = box.get("pos")
    if isinstance(pos, (list, tuple)) and len(pos) >= 2:
        start, end = pos[:2]
        if isinstance(start, int) and isinstance(end, int):
            if 0 <= start <= end <= len(page_text):
                return page_text[start:end].strip()

    return ""


@beartype
def _infer_tag_name_from_page_box(box: dict, page_text: str) -> str:
    box_class = str(box.get("class", "")).lower()
    text = _extract_page_box_text(box, page_text).strip()

    if box_class in {"image", "picture", "figure"}:
        return "picture"
    if box_class in {"table"}:
        return "table"
    if box_class in {"formula", "equation"}:
        return "formula"
    if box_class in {"footnote"}:
        return "footnote"
    if box_class in {"page-footer", "footer"}:
        return "page_footer"
    if box_class in {"header", "heading", "title", "section-header"}:
        return "section_header"

    if box_class == "page-header":
        if _looks_like_page_footer(text):
            return "page_footer"
        return "section_header"

    if _looks_like_list_item(text):
        return "list_item"
    if _looks_like_section_header({}, text):
        return "section_header"
    if _looks_like_page_footer(text):
        return "page_footer"

    if box_class == "text" or text:
        return "text"

    return "unspecified"


@beartype
def _extract_page_dimensions(
    page_chunk: dict,
) -> tuple[Optional[float], Optional[float]]:
    width = page_chunk.get("width")
    height = page_chunk.get("height")

    if (
        isinstance(width, (int, float))
        and isinstance(height, (int, float))
        and width > 0
        and height > 0
    ):
        return float(width), float(height)

    metadata = page_chunk.get("metadata", {})
    if isinstance(metadata, dict):
        width = metadata.get("width")
        height = metadata.get("height")
        if (
            isinstance(width, (int, float))
            and isinstance(height, (int, float))
            and width > 0
            and height > 0
        ):
            return float(width), float(height)

    file_path = metadata.get("file_path") if isinstance(metadata, dict) else None
    page_number = metadata.get("page_number") if isinstance(metadata, dict) else None

    if isinstance(file_path, str) and isinstance(page_number, int) and page_number > 0:
        with fitz.open(file_path) as pdf:
            page = pdf[page_number - 1]
            rect = page.rect
            return float(rect.width), float(rect.height)

    return None, None


@beartype
def _populate_tags_from_page_chunk(
    root_tag: DocTag, page_chunk: dict, page_num: int
) -> None:
    tag_counter = 1
    page_width, page_height = _extract_page_dimensions(page_chunk)

    blocks = page_chunk.get("blocks")
    if isinstance(blocks, list) and blocks:
        current_list_tag: Optional[DocTag] = None

        for block in blocks:
            tag_name = _infer_tag_name(block)
            text = _extract_block_text(block)
            bbox = _extract_block_bbox(block, page_width, page_height)

            if tag_name == "list_item":
                if current_list_tag is None:
                    current_list_tag = DocTag(
                        id=f"page{page_num}-list-{tag_counter}",
                        tag_name="list",
                        text="",
                        bbox=bbox,
                    )
                    root_tag.children.append(current_list_tag)
                    tag_counter += 1

                item_tag = DocTag(
                    id=f"page{page_num}-list_item-{tag_counter}",
                    tag_name="list_item",
                    text=text,
                    bbox=bbox,
                    attributes=_collect_block_attributes(block),
                )
                current_list_tag.children.append(item_tag)
                _extend_parent_bbox(current_list_tag, bbox)
                tag_counter += 1
                continue

            current_list_tag = None

            child_tag = DocTag(
                id=f"page{page_num}-{tag_name}-{tag_counter}",
                tag_name=tag_name,
                text=text,
                bbox=bbox,
                attributes=_collect_block_attributes(block),
            )

            if tag_name == "table":
                _attach_table_children(
                    child_tag,
                    block,
                    page_num,
                    tag_counter,
                    page_width,
                    page_height,
                )

            root_tag.children.append(child_tag)
            tag_counter += 1
        return

    page_boxes = page_chunk.get("page_boxes")
    page_text = page_chunk.get("text", "")

    if isinstance(page_boxes, list) and page_boxes:
        current_list_tag: Optional[DocTag] = None

        for box in page_boxes:
            if not isinstance(box, dict):
                continue

            tag_name = _infer_tag_name_from_page_box(box, page_text)
            text = _extract_page_box_text(box, page_text)
            bbox = _extract_page_box_bbox(box, page_width, page_height)

            attrs = _collect_page_box_attributes(box)

            if tag_name == "list_item":
                if current_list_tag is None:
                    current_list_tag = DocTag(
                        id=f"page{page_num}-list-{tag_counter}",
                        tag_name="list",
                        text="",
                        bbox=bbox,
                    )
                    root_tag.children.append(current_list_tag)
                    tag_counter += 1

                item_tag = DocTag(
                    id=f"page{page_num}-list_item-{tag_counter}",
                    tag_name="list_item",
                    text=text,
                    bbox=bbox,
                    attributes=attrs,
                )
                current_list_tag.children.append(item_tag)
                _extend_parent_bbox(current_list_tag, bbox)
                tag_counter += 1
                continue

            current_list_tag = None

            child_tag = DocTag(
                id=f"page{page_num}-{tag_name}-{tag_counter}",
                tag_name=tag_name,
                text=text,
                bbox=bbox,
                attributes=attrs,
            )
            root_tag.children.append(child_tag)
            tag_counter += 1
        return

    text = page_chunk.get("text", "")
    if text.strip():
        root_tag.children.append(
            DocTag(
                id=f"page{page_num}-text-{tag_counter}",
                tag_name="text",
                text=text,
                bbox=None,
            )
        )


@beartype
def extract_native_page_structure(
    pdf_path: Path, page_num: int
) -> tuple[str, List[DocTag]]:
    """
    Extract structured document data from a native PDF page using pymupdf4llm
    page chunk output, and convert it into a granular DocTag tree.
    """
    root_tag = DocTag(id=f"page{page_num}-root", tag_name="root", text="")

    chunks = pymupdf4llm.to_markdown(
        str(pdf_path),
        pages=[page_num - 1],
        page_chunks=True,
    )

    if not chunks:
        return "{}", [root_tag]

    page_chunk = chunks[0] if isinstance(chunks, list) else chunks
    raw_json = json.dumps(page_chunk, indent=2, ensure_ascii=False)

    _populate_tags_from_page_chunk(root_tag, page_chunk, page_num)

    return raw_json, [root_tag]


@beartype
def process_pdf(
    pdf_path: Path,
    output_dir: Path,
    pages: Optional[str] = None,
    ocr_only: bool = False,
):
    """Processes a single PDF file and saves per-page JSON dumps in the output directory."""
    if not pdf_path.exists():
        logger.error(f"Input file not found: {pdf_path}")
        return

    pdf_output_dir = output_dir / pdf_path.stem
    pdf_output_dir.mkdir(parents=True, exist_ok=True)

    # 1. Determine Page Range
    try:
        info = pdfinfo_from_path(str(pdf_path))
        max_pages = info["Pages"]
    except Exception as e:
        logger.error(f"Could not read PDF info: {e}")
        return

    first_page, last_page = 1, max_pages
    if pages:
        parts = pages.split("-")
        first_page = max(1, int(parts[0]))
        if len(parts) > 1:
            last_page = min(max_pages, int(parts[1]))
        else:
            last_page = first_page

    total_to_convert = (last_page - first_page) + 1
    logger.info(
        f"Processing PDF: {pdf_path.name} | Range: {first_page}-{last_page} ({total_to_convert} pages)"
    )

    # Check if all pages are already cached
    all_cached = True
    for current_page_num in range(first_page, last_page + 1):
        if not (pdf_output_dir / f"page_{current_page_num:03d}.json").exists():
            all_cached = False
            break

    if all_cached:
        logger.info(
            f"Skipping PDF {pdf_path.name}: All {total_to_convert} pages are already cached."
        )
        return

    # 2. Convert PDF to images
    logger.info("Rasterizing PDF pages to images...")
    all_pages = convert_from_path(
        str(pdf_path), first_page=first_page, last_page=last_page
    )

    start_time = time.perf_counter()

    # 3. Processing Loop
    pdf = fitz.open(str(pdf_path))
    for i, page_img in enumerate(all_pages):
        current_page_idx = i + 1
        current_page_num = first_page + i

        # Save image to cache so GUI can load it
        image_cache_file = get_cache_path(pdf_path, current_page_num)
        page_img.save(image_cache_file, format="PNG")

        # JSON dump output file
        json_output_file = pdf_output_dir / f"page_{current_page_num:03d}.json"

        # Calculate Statistics
        elapsed = time.perf_counter() - start_time
        pps = current_page_idx / elapsed if elapsed > 0 else 0
        remaining = total_to_convert - current_page_idx
        eta_seconds = remaining / pps if pps > 0 else 0

        stats_header = f"[{current_page_idx}/{total_to_convert}] PPS: {pps:.2f} | ETA: {format_eta(eta_seconds)}"

        try:
            if json_output_file.exists():
                logger.info(
                    f"{stats_header} | Page {current_page_num}: Using cached OCR dump"
                )
                continue

            elif not ocr_only and page_has_selectable_text(pdf, current_page_num):
                logger.info(
                    f"{stats_header} | Page {current_page_num}: Native text detected, extracting structure with pymupdf4llm"
                )
                clean_text, spatial_tags = extract_native_page_structure(
                    pdf_path, current_page_num
                )

            else:
                logger.info(f"{stats_header} | Page {current_page_num}: Requesting VLM")
                response_text = call_ollama_vlm(page_img)

                clean_text = response_text.strip()
                if not clean_text.startswith("<doctag>"):
                    clean_text = f"<doctag>{clean_text}</doctag>"
                spatial_tags = parse_spatial_tags(clean_text, current_page_num)

            page_data = PageData(
                page_number=current_page_num,
                raw_docling_response=clean_text,
                spatial_tags=spatial_tags,
                image_cache_path=str(image_cache_file.absolute()),
            )

            with open(json_output_file, "w") as f:
                f.write(page_data.model_dump_json(indent=2))

        except Exception as e:
            logger.error(f"Error processing page {current_page_num}: {e}")


@beartype
def run_headless_pipeline(config: AppConfig):
    """Entry point for the headless OCR pipeline."""
    for input_path in config.input_dirs:
        input_path = Path(input_path)
        if input_path.is_file() and input_path.suffix.lower() == ".pdf":
            ocr_only = (
                str(input_path.absolute()) in config.ocr_only_files
                or str(input_path) in config.ocr_only_files
            )
            process_pdf(input_path, config.output_dir, ocr_only=ocr_only)
        elif input_path.is_dir():
            for pdf_file in input_path.rglob("*.pdf"):
                ocr_only = (
                    str(pdf_file.absolute()) in config.ocr_only_files
                    or str(pdf_file) in config.ocr_only_files
                )
                process_pdf(pdf_file, config.output_dir, ocr_only=ocr_only)

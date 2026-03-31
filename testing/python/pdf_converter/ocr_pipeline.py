import io
import json
import base64
import logging
import hashlib
import time
from pathlib import Path
from typing import List, Optional
from datetime import timedelta

import requests
from PIL import Image as PILImage
from pdf2image import convert_from_path, pdfinfo_from_path
from xdg_base_dirs import xdg_cache_home

from models import PageData, DocTag
from config import AppConfig
import pdfplumber
from pdfplumber.page import Page as PlumberPage

# --- Configuration ---
OLLAMA_URL = "http://localhost:11434/api/chat"
MODEL_NAME = "ibm/granite-docling"  # Name as it appears in 'ollama list'

logger = logging.getLogger(__name__)


def get_cache_path(pdf_path: Path, page_num: int) -> Path:
    """Generates an XDG-compliant cache path for the specific page image."""
    cache_dir = xdg_cache_home() / "docling-vlm-cache" / "images"
    cache_dir.mkdir(parents=True, exist_ok=True)

    with open(pdf_path, 'rb') as f:
        file_prefix = f.read(16).hex()[:16]

    name_hash = hashlib.md5(str(pdf_path.absolute()).encode()).hexdigest()[:16]
    return cache_dir / f"{file_prefix}_{name_hash}_{page_num}.png"


def format_eta(seconds: float) -> str:
    if seconds < 0: return "00:00:00"
    return str(timedelta(seconds=int(seconds)))


def call_ollama_vlm(image: PILImage.Image) -> str:
    """Sends image to Ollama API and returns the response text."""
    buffered = io.BytesIO()
    image.save(buffered, format="PNG")
    base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

    payload = {
        "model":
        MODEL_NAME,
        "messages": [{
            "role": "user",
            "content": "Convert this page to docling.",
            "images": [base64_image]
        }],
        "stream":
        False,
        "options": {
            "temperature": 0
        }
    }

    response = requests.post(OLLAMA_URL, json=payload, timeout=600)
    response.raise_for_status()

    return response.json().get("message", {}).get("content", "")


import re
from docling_core.types.doc.document import DocTagsDocument, DocTagsPage, DoclingDocument, ContentLayer
from models import BoundingBox


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
                included_content_layers=set(ContentLayer), with_groups=True):
            tag_name = str(item.label.value) if hasattr(
                item, 'label') and item.label else type(item).__name__
            text_content = getattr(item, 'text', "")

            bbox = None
            if hasattr(item, 'prov') and item.prov and len(item.prov) > 0:
                p = item.prov[0]
                if hasattr(p, 'bbox') and p.bbox:
                    bbox = BoundingBox(x=p.bbox.l,
                                       y=p.bbox.t,
                                       width=p.bbox.r - p.bbox.l,
                                       height=p.bbox.b - p.bbox.t)

            child_tag = DocTag(id=f"page{page_num}-{tag_name}-{tag_counter}",
                               tag_name=tag_name,
                               text=text_content,
                               bbox=bbox)
            root_tag.children.append(child_tag)
            tag_counter += 1

    except Exception as e:
        logger.error(f"Failed to parse spatial tags using docling_core: {e}")

    return [root_tag]


def page_has_selectable_text(pdf: pdfplumber.PDF, page_num: int) -> bool:
    """Check if a given page (1-indexed) has selectable text."""
    page = pdf.pages[page_num - 1]
    text = page.extract_text()
    if text and text.strip():
        # Heuristic: if we get at least some meaningful characters, it's selectable
        non_whitespace = len(text.strip())
        return non_whitespace > 20
        
    return False


def extract_native_page_structure(pdf: pdfplumber.PDF,
                                  page_num: int) -> List[DocTag]:
    """
    Extract document structure from a native (non-scanned) PDF page
    using pdfplumber, returning DocTag tree.
    """
    root_tag = DocTag(id=f"page{page_num}-root", tag_name="root", text="")

    page = pdf.pages[page_num - 1]
    page_width = float(page.width)
    page_height = float(page.height)

    words = page.extract_words(
        x_tolerance=3,
        y_tolerance=3,
        keep_blank_chars=False,
        extra_attrs=["fontname", "size"],
    )

    if not words:
        return [root_tag]

    # Group words into lines by clustering on y-coordinate (top)
    lines: List[dict] = []
    current_line_words = []
    current_y = None
    y_tolerance = 3.0

    # Sort words by vertical position then horizontal
    words_sorted = sorted(words,
                          key=lambda w: (float(w["top"]), float(w["x0"])))

    for w in words_sorted:
        w_top = float(w["top"])
        if current_y is None or abs(w_top - current_y) > y_tolerance:
            if current_line_words:
                lines.append(_merge_line_words(current_line_words))
            current_line_words = [w]
            current_y = w_top
        else:
            current_line_words.append(w)

    if current_line_words:
        lines.append(_merge_line_words(current_line_words))

    # Classify lines and group consecutive same-type lines into blocks
    classified_lines = [
        _classify_line(line, page_width, page_height) for line in lines
    ]

    blocks = _group_lines_into_blocks(classified_lines)

    tag_counter = 1
    for block in blocks:
        text = "\n".join(l["text"] for l in block["lines"])
        tag_name = block["tag_name"]

        # Compute bounding box encompassing all lines in the block
        x_min = min(l["x0"] for l in block["lines"])
        y_min = min(l["top"] for l in block["lines"])
        x_max = max(l["x1"] for l in block["lines"])
        y_max = max(l["bottom"] for l in block["lines"])

        bbox = BoundingBox(
            x=x_min / page_width,
            y=y_min / page_height,
            width=(x_max - x_min) / page_width,
            height=(y_max - y_min) / page_height,
        )

        child_tag = DocTag(
            id=f"page{page_num}-{tag_name}-{tag_counter}",
            tag_name=tag_name,
            text=text,
            bbox=bbox,
        )
        root_tag.children.append(child_tag)
        tag_counter += 1

    # Extract tables separately
    tables = page.extract_tables()
    for table_data in tables:
        if not table_data:
            continue
        table_text = "\n".join("\t".join(cell or "" for cell in row)
                               for row in table_data if row)
        child_tag = DocTag(
            id=f"page{page_num}-table-{tag_counter}",
            tag_name="table",
            text=table_text,
            bbox=
            None,  # pdfplumber table bbox would require table.bbox from find_tables
        )
        root_tag.children.append(child_tag)
        tag_counter += 1

    return [root_tag]


def _merge_line_words(words: List[dict]) -> dict:
    """Merge a list of words on the same line into a single line dict."""
    words_sorted = sorted(words, key=lambda w: float(w["x0"]))
    text = " ".join(w["text"] for w in words_sorted)
    x0 = min(float(w["x0"]) for w in words_sorted)
    x1 = max(float(w["x1"]) for w in words_sorted)
    top = min(float(w["top"]) for w in words_sorted)
    bottom = max(float(w["bottom"]) for w in words_sorted)

    # Use the most common font/size in the line
    sizes = [float(w.get("size", 12)) for w in words_sorted]
    fontnames = [w.get("fontname", "") for w in words_sorted]
    avg_size = sum(sizes) / len(sizes) if sizes else 12.0
    primary_font = max(set(fontnames),
                       key=fontnames.count) if fontnames else ""

    return {
        "text": text,
        "x0": x0,
        "x1": x1,
        "top": top,
        "bottom": bottom,
        "font_size": avg_size,
        "fontname": primary_font,
    }


def _classify_line(line: dict, page_width: float, page_height: float) -> dict:
    """Classify a line into a tag type based on heuristics."""
    text = line["text"].strip()
    font_size = line["font_size"]
    fontname = line["fontname"].lower()
    y_rel = line["top"] / page_height

    tag_name = "text"

    # Page footer: near the bottom of the page
    if y_rel > 0.90:
        tag_name = "page_footer"
    # Footnote: small font near the bottom
    elif y_rel > 0.75 and font_size < 9:
        tag_name = "footnote"
    # Section header: bold or large font
    elif "bold" in fontname or font_size >= 14:
        tag_name = "section_header"
    # List item: starts with bullet, dash, or number-dot pattern
    elif (text.startswith("•") or text.startswith("-") or text.startswith("–")
          or _is_numbered_list_item(text)):
        tag_name = "list_item"
    # Formula: contains lots of math-like characters
    elif _looks_like_formula(text):
        tag_name = "formula"

    line["tag_name"] = tag_name
    return line


def _is_numbered_list_item(text: str) -> bool:
    """Check if text starts with a numbered list pattern like '1.' or '(a)'."""
    import re
    return bool(
        re.match(r"^\s*(\d{1,3}[.)]\s|[a-zA-Z][.)]\s|\([a-zA-Z0-9]+\)\s)",
                 text))


def _looks_like_formula(text: str) -> bool:
    """Heuristic: high ratio of math-related characters."""
    if len(text) < 3:
        return False
    math_chars = set("∑∏∫∂√∞±≤≥≠≈∈∉⊂⊃∪∩∀∃∇αβγδεζηθλμπσφψω=+/<>^{}[]")
    count = sum(1 for c in text if c in math_chars)
    return count / len(text) > 0.3


def _group_lines_into_blocks(classified_lines: List[dict]) -> List[dict]:
    """Group consecutive lines of the same tag type into blocks."""
    if not classified_lines:
        return []

    blocks = []
    current_block = {
        "tag_name": classified_lines[0]["tag_name"],
        "lines": [classified_lines[0]],
    }

    for line in classified_lines[1:]:
        tag = line["tag_name"]
        prev_tag = current_block["tag_name"]

        # list_items get grouped under the same block but we keep them individual
        # so they map to separate list_item tags; instead group into a "list" parent later
        if tag == prev_tag and tag != "list_item":
            current_block["lines"].append(line)
        else:
            blocks.append(current_block)
            current_block = {
                "tag_name": tag,
                "lines": [line],
            }

    blocks.append(current_block)

    # Post-process: wrap consecutive list_item blocks into a list block
    final_blocks = []
    i = 0
    while i < len(blocks):
        if blocks[i]["tag_name"] == "list_item":
            # Collect consecutive list_item blocks
            list_block = {
                "tag_name": "list",
                "lines": [],
            }
            while i < len(blocks) and blocks[i]["tag_name"] == "list_item":
                list_block["lines"].extend(blocks[i]["lines"])
                i += 1
            final_blocks.append(list_block)
        else:
            final_blocks.append(blocks[i])
            i += 1

    return final_blocks


def process_pdf(pdf_path: Path, output_dir: Path, pages: Optional[str] = None):
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
        parts = pages.split('-')
        first_page = max(1, int(parts[0]))
        if len(parts) > 1:
            last_page = min(max_pages, int(parts[1]))
        else:
            last_page = first_page

    total_to_convert = (last_page - first_page) + 1
    logger.info(
        f"Processing PDF: {pdf_path.name} | Range: {first_page}-{last_page} ({total_to_convert} pages)"
    )

    # 2. Convert PDF to images
    logger.info("Rasterizing PDF pages to images...")
    all_pages = convert_from_path(str(pdf_path),
                                  first_page=first_page,
                                  last_page=last_page)

    start_time = time.perf_counter()

    # 3. Processing Loop
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

        pdf = pdfplumber.open(str(pdf_path))
        try:
            if json_output_file.exists():
                logger.info(
                    f"{stats_header} | Page {current_page_num}: Using cached OCR dump (re-creating spatial tags)"
                )
                with open(json_output_file, 'r') as f:
                    page_data_json = json.load(f)
                    clean_text = page_data_json.get("raw_docling_response", "")
                spatial_tags = parse_spatial_tags(clean_text, current_page_num)
            elif page_has_selectable_text(pdf_path, current_page_num):
                logger.info(
                    f"{stats_header} | Page {current_page_num}: Native text detected, extracting structure directly"
                )
                spatial_tags = extract_native_page_structure(
                    pdf, current_page_num)
                clean_text = ""  # No VLM response needed
            else:
                logger.info(
                    f"{stats_header} | Page {current_page_num}: Requesting VLM"
                )
                response_text = call_ollama_vlm(page_img)

                clean_text = response_text.strip()
                if not clean_text.startswith("<doctag>"):
                    clean_text = f"<doctag>{clean_text}</doctag>"
                spatial_tags = parse_spatial_tags(clean_text, current_page_num)

            page_data = PageData(page_number=current_page_num,
                                 raw_docling_response=clean_text,
                                 spatial_tags=spatial_tags,
                                 image_cache_path=str(
                                     image_cache_file.absolute()))

            # Save dump
            with open(json_output_file, 'w') as f:
                f.write(page_data.model_dump_json(indent=2))

        except Exception as e:
            logger.error(f"Error processing page {current_page_num}: {e}")


def run_headless_pipeline(config: AppConfig):
    """Entry point for the headless OCR pipeline."""
    for input_path in config.input_dirs:
        input_path = Path(input_path)
        if input_path.is_file() and input_path.suffix.lower() == '.pdf':
            process_pdf(input_path, config.output_dir)
        elif input_path.is_dir():
            for pdf_file in input_path.rglob('*.pdf'):
                process_pdf(pdf_file, config.output_dir)

from __future__ import annotations

import contextlib
import io
import json
import logging
import os
import re
from pathlib import Path
import sys
import tempfile
from typing import Any, Literal

import fitz
import pymupdf4llm
from pydantic import BaseModel

from index_service.services.resources.pdf.page_model import BoundingBox, DocTag

log = logging.getLogger(__name__)


class MyPDFPage(BaseModel, extra="forbid"):
    extractor: Literal["mypdf"] = "mypdf"
    page_number: int
    raw_chunk_json: str
    spatial_tags: list[DocTag]


# --- Block-level helpers ---


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
                        str(span.get("text", "")) for span in spans
                        if isinstance(span, dict)).strip()
                    if span_text:
                        line_texts.append(span_text)
                        continue

                line_text = line.get("text")
                if isinstance(line_text, str) and line_text.strip():
                    line_texts.append(line_text.strip())
        if line_texts:
            return "\n".join(line_texts)

    return ""


def _extract_block_bbox(
    block: dict,
    page_width: float | None,
    page_height: float | None,
) -> BoundingBox | None:
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


def _collect_block_attributes(block: dict) -> dict[str, Any]:
    attrs = {}

    for key in ("type", "level", "section", "header_level", "caption", "kind"):
        if key in block:
            attrs[key] = block[key]

    if "bbox" in block:
        attrs["source_bbox"] = block["bbox"]

    return attrs


def _attach_table_nested(
    table_tag: DocTag,
    block: dict,
    page_num: int,
    parent_counter: int,
    page_width: float | None,
    page_height: float | None,
) -> None:
    rows = block.get("rows")
    if not isinstance(rows, list):
        return

    nested_counter = 1
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
            id=f"page{page_num}-table-{parent_counter}-row-{nested_counter}",
            tag_name="text",
            text=row_text,
            bbox=None,
        )
        table_tag.nested.append(row_tag)
        nested_counter += 1


def _extend_parent_bbox(parent: DocTag,
                        nested_bbox: BoundingBox | None) -> None:
    if nested_bbox is None:
        return

    if parent.bbox is None:
        parent.bbox = BoundingBox(
            x=nested_bbox.x,
            y=nested_bbox.y,
            width=nested_bbox.width,
            height=nested_bbox.height,
        )
        return

    x0 = min(parent.bbox.x, nested_bbox.x)
    y0 = min(parent.bbox.y, nested_bbox.y)
    x1 = max(parent.bbox.x + parent.bbox.width,
             nested_bbox.x + nested_bbox.width)
    y1 = max(parent.bbox.y + parent.bbox.height,
             nested_bbox.y + nested_bbox.height)

    parent.bbox = BoundingBox(
        x=x0,
        y=y0,
        width=x1 - x0,
        height=y1 - y0,
    )


# --- Tag name inference ---


def _looks_like_list_item(text: str) -> bool:
    return bool(re.match(r"^\s*(?:[-•*]|(?:\d+|[a-zA-Z]+)[.)])\s+", text))


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


def _looks_like_page_footer(text: str) -> bool:
    stripped = text.strip()
    if not stripped:
        return False

    if re.fullmatch(r"\d+", stripped):
        return True

    if re.fullmatch(r"page\s+\d+", stripped.lower()):
        return True

    return False


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


# --- Page-box helpers (fallback when blocks are absent) ---


def _collect_page_box_attributes(box: dict) -> dict[str, Any]:
    attrs = {}

    for key in ("index", "class", "bbox", "pos"):
        if key in box:
            attrs[key] = box[key]

    return attrs


def _extract_page_box_bbox(
    box: dict,
    page_width: float | None,
    page_height: float | None,
) -> BoundingBox | None:
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


def _extract_page_box_text(box: dict, page_text: str) -> str:
    pos = box.get("pos")
    if isinstance(pos, (list, tuple)) and len(pos) >= 2:
        start, end = pos[:2]
        if isinstance(start, int) and isinstance(end, int):
            if 0 <= start <= end <= len(page_text):
                return page_text[start:end].strip()

    return ""


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


# --- Page dimension extraction ---


def _extract_page_dimensions(
    page_chunk: dict, ) -> tuple[float | None, float | None]:
    width = page_chunk.get("width")
    height = page_chunk.get("height")

    if (isinstance(width, (int, float)) and isinstance(height, (int, float))
            and width > 0 and height > 0):
        return float(width), float(height)

    metadata = page_chunk.get("metadata", {})
    if isinstance(metadata, dict):
        width = metadata.get("width")
        height = metadata.get("height")
        if (isinstance(width,
                       (int, float)) and isinstance(height, (int, float))
                and width > 0 and height > 0):
            return float(width), float(height)

    file_path = metadata.get("file_path") if isinstance(metadata,
                                                        dict) else None
    page_number = metadata.get("page_number") if isinstance(metadata,
                                                            dict) else None

    if isinstance(file_path, str) and isinstance(page_number,
                                                 int) and page_number > 0:
        with fitz.open(file_path) as pdf:
            page = pdf[page_number - 1]
            rect = page.rect
            return float(rect.width), float(rect.height)

    return None, None


# --- Page chunk → DocTag tree ---


def _populate_tags_from_page_chunk(root_tag: DocTag, page_chunk: dict,
                                   page_num: int) -> None:
    tag_counter = 1
    page_width, page_height = _extract_page_dimensions(page_chunk)

    blocks = page_chunk.get("blocks")
    if isinstance(blocks, list) and blocks:
        current_list_tag: DocTag | None = None

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
                    root_tag.nested.append(current_list_tag)
                    tag_counter += 1

                item_tag = DocTag(
                    id=f"page{page_num}-list_item-{tag_counter}",
                    tag_name="list_item",
                    text=text,
                    bbox=bbox,
                    attributes=_collect_block_attributes(block),
                )
                current_list_tag.nested.append(item_tag)
                _extend_parent_bbox(current_list_tag, bbox)
                tag_counter += 1
                continue

            current_list_tag = None

            nested_tag = DocTag(
                id=f"page{page_num}-{tag_name}-{tag_counter}",
                tag_name=tag_name,
                text=text,
                bbox=bbox,
                attributes=_collect_block_attributes(block),
            )

            if tag_name == "table":
                _attach_table_nested(
                    nested_tag,
                    block,
                    page_num,
                    tag_counter,
                    page_width,
                    page_height,
                )

            root_tag.nested.append(nested_tag)
            tag_counter += 1
        return

    page_boxes = page_chunk.get("page_boxes")
    page_text = page_chunk.get("text", "")

    if isinstance(page_boxes, list) and page_boxes:
        current_list_tag: DocTag | None = None

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
                    root_tag.nested.append(current_list_tag)
                    tag_counter += 1

                item_tag = DocTag(
                    id=f"page{page_num}-list_item-{tag_counter}",
                    tag_name="list_item",
                    text=text,
                    bbox=bbox,
                    attributes=attrs,
                )
                current_list_tag.nested.append(item_tag)
                _extend_parent_bbox(current_list_tag, bbox)
                tag_counter += 1
                continue

            current_list_tag = None

            nested_tag = DocTag(
                id=f"page{page_num}-{tag_name}-{tag_counter}",
                tag_name=tag_name,
                text=text,
                bbox=bbox,
                attributes=attrs,
            )
            root_tag.nested.append(nested_tag)
            tag_counter += 1
        return

    text = page_chunk.get("text", "")
    if text.strip():
        root_tag.nested.append(
            DocTag(
                id=f"page{page_num}-text-{tag_counter}",
                tag_name="text",
                text=text,
                bbox=None,
            ))


@contextlib.contextmanager
def _silence_pymupdf4llm():
    # Flush Python-level buffers so their content isn't redirected.
    sys.stdout.flush()
    sys.stderr.flush()

    saved_out_fd = os.dup(1)
    saved_err_fd = os.dup(2)

    with tempfile.TemporaryFile(mode="w+b") as out_tmp, \
            tempfile.TemporaryFile(mode="w+b") as err_tmp:
        os.dup2(out_tmp.fileno(), 1)
        os.dup2(err_tmp.fileno(), 2)
        try:
            yield out_tmp, err_tmp
        finally:
            sys.stdout.flush()
            sys.stderr.flush()
            os.dup2(saved_out_fd, 1)
            os.dup2(saved_err_fd, 2)
            os.close(saved_out_fd)
            os.close(saved_err_fd)


class MyPDFExtractor:
    """Extracts structured tags from PDF pages with selectable text using pymupdf4llm."""

    @staticmethod
    def page_has_selectable_text(pdf: fitz.Document, page_num: int) -> bool:
        page = pdf[page_num - 1]
        text = page.get_text("text")
        return len(text.strip()) > 20

    def extract_page(self, pdf_path: Path, page_num: int) -> MyPDFPage:
        log.info(f"MyPDF extracting page {page_num} from {pdf_path.name}")

        root_tag = DocTag(id=f"page{page_num}-root", tag_name="root", text="")

        with _silence_pymupdf4llm() as (out, err):
            try:
                chunks = pymupdf4llm.to_markdown(
                    str(pdf_path),
                    pages=[page_num - 1],
                    page_chunks=True,
                )
            except Exception as e:
                captured = (out.getvalue() + err.getvalue()).strip()
                raise RuntimeError(
                    f"pymupdf4llm failed on page {page_num}: {e}" +
                    (f"\n--- captured output ---\n{captured}"
                     if captured else "")) from e

        if not chunks:
            return MyPDFPage(
                page_number=page_num,
                raw_chunk_json="{}",
                spatial_tags=[root_tag],
            )

        page_chunk = chunks[0] if isinstance(chunks, list) else chunks
        raw_json = json.dumps(page_chunk, indent=2, ensure_ascii=False)

        _populate_tags_from_page_chunk(root_tag, page_chunk, page_num)

        return MyPDFPage(
            page_number=page_num,
            raw_chunk_json=raw_json,
            spatial_tags=[root_tag],
        )
